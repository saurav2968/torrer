%%%-------------------------------------------------------------------
%%% @author saurav
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jan 2018 3:45 PM
%%%-------------------------------------------------------------------
-module(bootstrap_fsm).
-author("saurav").
-include("../include/torrer.hrl").
-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([
  init/1,
  format_status/2,
  state_bootstrap/3,
  state_operate/3,
  handle_event/4,
  terminate/3,
  code_change/4,
  callback_mode/0
]).

-define(SERVER, ?MODULE).

-record(state, {
  peer_id,
  current_state=undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  lager:info("Starting bootstrap_fsm"),
  {ok, state_bootstrap, #state{},[{next_event, internal, mnesia_start}]}.

callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @spec format_status(Opt, [PDict, StateName, State]) -> term()
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%%   {next_state, StateName, Data, [{next_event, internal, data}]}.
%% @end
%%--------------------------------------------------------------------
state_bootstrap(internal, mnesia_start, State) ->
  %% Start mnesia, setup tables
  lager:info("~p | Starting mnesia and setting up tables",[self()]),
  lager:info("~p~n", [mnesia:create_schema([node()])]), % create a schema table on local disk, returns error if schema already there
  mnesia:start(),
  lager:info("~p", [mnesia:create_table(torrents, [{disc_copies, [node()]},{attributes, record_info(fields, torrents)}])]),
  mnesia:create_table(meta, [{disc_copies, [node()]},{attributes, record_info(fields, meta)}]),
  mnesia:wait_for_tables([torrents, meta], 5000),
  %% lager:info("~p", [mnesia:info()]),
  lager:info("Done with mnesia bootstrap"),
  NextStateName = state_bootstrap,
  {next_state, NextStateName, State#state{current_state = mnesia_inited},[{next_event, internal, gen_peer_id}]};

state_bootstrap(internal, gen_peer_id, State) ->
  lager:info("Setting peer id..."),
  {ok, PeerId} = db_controller:get_and_update_peer_id(),
  NextStateName = state_bootstrap,
  {next_state, NextStateName, State#state{current_state = peer_id_set, peer_id = PeerId},[{next_event, internal, restore_torrents}]};

state_bootstrap(internal, restore_torrents, State) ->
  lager:info("Restoring torrents..."),
  ok = restore_torrents(),
  NextStateName = state_bootstrap,
  {next_state, NextStateName, State#state{current_state = torrents_restored}, [{next_event, internal, persist_name}]};

state_bootstrap(internal, persist_name, State) ->
  lager:info("Registering in gproc..."),
  true = gproc:reg({n, l, bootstrap_fsm}),
  NextStateName = state_operate,
  lager:info("Now ready to operate...~n"),
  {next_state, NextStateName, State#state{current_state = persisted_name}};

state_bootstrap(_EventType, _EventContent, State) ->
  NextStateName = state_bootstrap,
  {next_state, NextStateName, State}.


state_operate(_EventType, _EventContent, State) ->
  NextStateName = state_operate,
  {next_state, NextStateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

restore_torrents() ->
  ok.
