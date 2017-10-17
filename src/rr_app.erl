-module(rr_app).
-behaviour(application).

-export([start/2, stop/1]).

dispatch_rules() ->
	cowboy_router:compile([
		{'_', [
			{'_', rr_handler, []}
		]}
	]).

start(_Type, _Args) ->
	cowboy:start_clear(rr_http_listener,
		[{port, 9090}],
		#{env => #{dispatch => dispatch_rules()}}
	).

stop(_State) ->
	ok.
