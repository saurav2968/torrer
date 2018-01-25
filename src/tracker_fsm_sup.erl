%%%-------------------------------------------------------------------
%%% @author saurav
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2018 5:00 PM
%%%-------------------------------------------------------------------
-module(tracker_fsm_sup).
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
  %% 2 child type possible. all started by tacker_fsm
  lager:info("~p | Starting tracker_fsm_sup for InfoHash: ~p", [self(), InfoHash]),
  gproc:reg({n,l, {?SERVER, InfoHash}}),
  SupFlags = #{strategy => one_for_one, intensity => 20, period => 1},

  %% iterate through trackers list and start child
  UdpTrackerUrls = torrent_file:get_trackers(TorrentDict, udp),
  HttpTrackerUrls = torrent_file:get_trackers(TorrentDict, http),
  UdpChildSpecs = lists:map(fun(E) ->
                          #{id => list_to_atom("tracker_udp_fsm:" ++ InfoHash ++ binary_to_list(E)),
                            start => {tracker_udp_fsm, start_link, [InfoHash, TorrentDict, binary_to_list(E)]},
                            restart => transient,
                            shutdown => 2000,
                            type => worker,
                            modules => [tracker_udp_fsm]}
                         end, UdpTrackerUrls),
  ChildSpecs = UdpChildSpecs,
  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
