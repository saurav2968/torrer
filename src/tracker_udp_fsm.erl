%%%-------------------------------------------------------------------
%%% @author saurav
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jan 2018 9:23 PM
%%%-------------------------------------------------------------------
-module(tracker_udp_fsm).
-author("saurav").

-behaviour(gen_statem).

%% API
-export([start_link/3]).

%% gen_statem callbacks
-export([
  init/1,
  callback_mode/0,
  format_status/2,
  state_connect/3,
  state_bootstrap/3,
  state_name/3,
  handle_event/4,
  terminate/3,
  code_change/4
]).

-define(SERVER, ?MODULE).
-define(PROTOCOL_ID, 16#41727101980). %% hex to decimal
-define(CONNECT_SEQ, 0).
-define(ANNOUNCE_SEQ, 1).

-record(state, {uri, infohash, torrentdict, socket, connection_id, connection_time}).

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
start_link(InfoHash, TorrentDict, Uri) ->
  Name = torrer_utils:get_uuid(),
  gen_statem:start_link({local, Name}, ?MODULE, [InfoHash, TorrentDict, Uri], []).

callback_mode() -> state_functions.

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
init([InfoHash, TorrentDict, Uri]) ->
  lager:info("Starting tracker_udp_fsm for InfoHash: ~p, Uri: ~p",[InfoHash, Uri]),
  gproc:reg({n, l,{?SERVER, InfoHash, Uri}}),
  {ok, state_bootstrap, #state{infohash = InfoHash, uri = Uri, torrentdict = TorrentDict},
    [{next_event, internal, ok}]}.


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
%% @end
%%--------------------------------------------------------------------
state_bootstrap(internal, ok, #state{uri = Uri} = State) ->
  %% create a udp socket
  {ok, Socket} = gen_udp:open(0, [binary, {active, once}]),
  lager:info("Opened socket for ~p: ~p", [Uri, Socket]),
  {next_state, state_connect, State#state{socket = Socket}, [{next_event, internal, ok}]}.

state_connect(internal, ok, #state{socket = Socket, uri = Uri} = State) ->
  lager:info("Connecting to Uri ~p", [Uri]),
  TransactionId = random:uniform() * 2147483647, % 32 bit int
  %%ok = gen_udp:send(Socket, Uri)
  {next_state, state_name, State}.

state_name(_EventType, _EventContent, State) ->
  NextStateName = next_state,
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
terminate(_Reason, _StateName, #state{uri =Uri, socket = Socket}) ->
  lager:info("Closing socket ~p for Uri ~p",[Socket, Uri]),
  gen_udp:close(Socket),
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
