-module(torrer_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

stop_mnesia() ->
	case mnesia:system_info(is_running) of
		yes ->
			application:stop(mnesia),
			timer:sleep(100),
			stop_mnesia();
		_ -> ok
	end.

start(_Type, _Args) ->
	lager:info("Starting torrer..."),
	Rules = [{'_', [{"/", torrer_web, []},
						%% js files
					 {"/priv/js/[...]", cowboy_static, {priv_dir, torrer, "js/"}},
						%% rest endpoint
					 {"/rest/new", torrer_rest, []}
		]}],

	Dispatch = cowboy_router:compile(Rules),
	{ok, _} = cowboy:start_http(product_http, 5, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]),
	lager:info("Started cowboy server on 8080."),

	%% Below is a hack for now
	stop_mnesia(),
	torrer_sup:start_link().

stop(_State) ->
	ok.
