%%%-------------------------------------------------------------------
%%% @author saurav
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2018 12:16 AM
%%%-------------------------------------------------------------------
-module(request_fsm_sup).
-author("saurav").

-behaviour(supervisor).

%% API
-export([start_link/0,
  new_request/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  lager:info("in start_link of ~p",[?MODULE]),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

new_request(FileName) ->
  Key = {n, l, "request:" ++ FileName},
  case gproc:where(Key) of
    undefined ->
      supervisor:start_child(?SERVER, [FileName]),
      ok;
    Pid ->
      lager:info("Request for ~p already active with pid: ~p~n", [FileName, Pid]),
      {error, already_exists}
  end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  lager:info("Starting request_fsm_sup..."),
  process_flag(trap_exit, true),
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  ChildSpecs = [#{id => requests,
    start => {request_fsm, start_link, []},
    shutdown => 1000,
    type => worker,
    restart => transient,
    modules => [request_fsm]}],

  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
