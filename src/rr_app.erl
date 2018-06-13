-module(rr_app).
-behaviour(application).

-define(HTTP_PORT, application:get_env(rr, http_port, 9090)).
-define(SHOUTCAST_PORT, application:get_env(rr, shoutcast_port, 9091)).
-define(BASE_PATH, erlang:list_to_binary(application:get_env(rr, base_path, "data"))).

-export([start/2, stop/1]).

dispatch_rules() ->
	cowboy_router:compile([
		{'_', [
			{'_', rr_handler, [?BASE_PATH]}
		]}
	]).

start(_Type, _Args) ->
	{ok, _} = rr_swipe:start_link(),
	{ok, _} = rr_shout:start_link(?SHOUTCAST_PORT, {rr_shout_handler, loop}),
	cowboy:start_clear(rr_http_listener,
		[{port, ?HTTP_PORT}],
		#{env => #{dispatch => dispatch_rules()}}
	).

stop(_State) ->
	ok.
