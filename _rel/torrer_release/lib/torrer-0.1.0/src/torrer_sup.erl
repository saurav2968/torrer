-module(torrer_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	lager:info("Starting torrer_sup..."),

	SupFlags = #{strategy => one_for_one, intensity => 2, period => 5},
	BootstrapFsmSpecs = #{id => bootstrap_fsm,
												start => {bootstrap_fsm, start_link, []},
												restart => permanent,
												shutdown => 2000,
												type => worker,
												modules => [bootstrap_fsm]},

	RequestListenerSpecs = #{id => request_listener,
		start => {request_listener, start_link, []},
		restart => permanent,
		shutdown => 2000,
		type => worker,
		modules => [request_listener]},

	RequestFsmSupSpecs = #{id => request_fsm_sup,
		start => {request_fsm_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [request_fsm_sup]},

	ChildSpecs = [BootstrapFsmSpecs, RequestFsmSupSpecs, RequestListenerSpecs],
	{ok, {SupFlags, ChildSpecs}}.
