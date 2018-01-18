%%%-------------------------------------------------------------------
%%% @author saurav
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2018 4:44 PM
%%%-------------------------------------------------------------------
-module(torrent_sup).
-author("saurav").

-behaviour(supervisor).

%% API
-export([start_link/0, new_torrent/2]).

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
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

new_torrent(InfoHash, TorrentDict) ->
  Key = {n, l, "torrent:" ++ InfoHash},
  case gproc:where(Key) of
    undefined ->
      lager:info("Starting new torrent_sup child: ~p", [InfoHash]),
      case supervisor:start_child(?SERVER, [InfoHash, TorrentDict]) of
        {ok, _Pid} -> ok;
        E ->
          lager:error("Failed to start torrent_sup child: ~p", [E]),
          ok
      end;
    Pid ->
      lager:info("torrent for ~p already active with pid: ~p~n", [InfoHash, Pid]),
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
  lager:info("Starting torrent_sup..."),
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {torrent_instance_sup, {torrent_instance_sup, start_link, []},
    Restart, Shutdown, Type, [torrent_instance_sup]},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
