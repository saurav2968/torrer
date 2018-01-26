%%%-------------------------------------------------------------------
%%% @author saurav
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2018 4:51 PM
%%%-------------------------------------------------------------------
-module(torrent_instance_sup).
-author("saurav").

-behaviour(supervisor).

%% API
-export([start_link/2]).

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
-spec(start_link(list(), map()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(InfoHash, TorrentDict) ->
  Name = torrer_utils:get_uuid(),
  supervisor:start_link({local, Name}, ?MODULE, [InfoHash, TorrentDict]).

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
init([InfoHash, TorrentDict]) ->
  lager:info("Starting torrent_instance_sup for InfoHash: ~p", [InfoHash]),
  gproc:reg({n, l, {?SERVER, InfoHash}}),
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  TrackerFsmSupSpec = #{id => list_to_atom(atom_to_list(tracker_fsm_sup) ++ InfoHash),
    start => {tracker_fsm_sup, start_link, [InfoHash, TorrentDict]},
    restart => permanent,
    shutdown => infinity,
    type => supervisor,
    modules => [tracker_fsm_sup]},

  TorrentServerSpec = #{id => list_to_atom(atom_to_list(torrent_server) ++ InfoHash),
    start => {torrent_server, start_link, [InfoHash, TorrentDict]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [torrent_server]},


  ChildSpecs = [TrackerFsmSupSpec, TorrentServerSpec],
  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
