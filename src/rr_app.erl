-module(rr_app).
-behaviour(application).

-export([start/0, start/2, stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [{'_', rr_handler, []}]}
	]),
	cowboy:start_http(rr_http_listener, 100, [{port, 9090}],
		[{env, [{dispatch, Dispatch}]}]
	).

start() ->
	application:start(lager),
	application:start(cowboy),
	ok.

stop(_State) ->
	ok.
