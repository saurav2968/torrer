-module(torrer_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	lager:info("Starting torrer..."),
	%% Below is a hack for now
	application:ensure_started(lager),
	application:ensure_started(gproc),
	application:ensure_started(gun),
	application:ensure_started(crypto),
	application:ensure_started(mnesia),
	case mnesia:system_info(is_running) of
		yes ->
			application:stop(mnesia),
			timer:sleep(2000);
		_ -> ok
	end,
	torrer_sup:start_link().

stop(_State) ->
	ok.
