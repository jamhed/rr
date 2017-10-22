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
	{ok, _} = rr_swipe:start_link(),
	{ok, _} = rr_shout:start_link(9091, {rr_shout_handler, loop}),
	cowboy:start_clear(rr_http_listener,
		[{port, 9090}],
		#{env => #{dispatch => dispatch_rules()}}
	).

stop(_State) ->
	ok.
