-module(rr_handler).
-export([init/3, handle/2, terminate/3]).

init({_, http}, Req, _Opts) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	try handle_method(Method, Req) of
		{ok, Re} -> {ok, Re, State}
	catch
		C:E ->
			{Path, _} = cowboy_req:path(Req),
			lager:error("~p:~p path:~p ~p", [C, E, Path, erlang:get_stacktrace()]),
			{ok, Re} = reply(404, <<"not_found">>, Req),
			{ok, Re, State}
	end.

terminate(_Reason,_Req, _State) ->
	ok.

reply(Code, Req) ->
	reply(Code, <<"ok">>, Req).
reply(Code, Msg, Req) ->
	cowboy_req:reply(Code, [{<<"connection">>, <<"close">>}, {<<"content-type">>, <<"text/plain">>}], Msg, Req).

handle_method(<<"PUT">>, Req) ->
	maybe_write(Req, cowboy_req:path(Req));
handle_method(<<"GET">>, Req) ->
	read(cowboy_req:path(Req)).

maybe_write(Req, {<<"/", Path/binary>>, _}=P) ->
	case filelib:is_file(Path) of
		true ->
			reply(409, <<"file_exists">>, Req);
		false ->
			write(cowboy_req:body(Req), P)
	end.

write(_, {<<"/">>, Req}) -> reply(403, <<"path required">>, Req);
write({ok, Data, Req}, {<<"/", Path/binary>>, _}) ->
	lager:info("write file:~p size:~p", [Path, get_size(cowboy_req:body_length(Req))]),
	ensure_folder(filename:dirname(Path)),
	ok = file:write_file(Path, Data),
	reply(200, Req);
write({more, Data, Req}, {<<"/", Path/binary>>, _}) ->
	lager:info("write file:~p size:~p", [Path, get_size(cowboy_req:body_length(Req))]),
	ensure_folder(filename:dirname(Path)),
	reply(200, write_append(Path, {more, Data, Req})).

write_append(Path, {ok, Data, Req}) ->
	ok = file:write_file(Path, Data, [append]),
	Req;
write_append(Path, {more, Data, Req}) ->
	ok = file:write_file(Path, Data, [append]),
	write_append(Path, cowboy_req:body(Req)).

read({<<"/", Path/binary>>, Req}) ->
	{ok, Binary} = file:read_file(Path),
	cowboy_req:reply(200, [{<<"content-length">>, erlang:integer_to_binary(erlang:size(Binary))}], Binary, Req).

get_size({Size, _}) -> Size.

ensure_folder(Dir) ->
	ok = filelib:ensure_dir(<<Dir/binary, "/">>).
