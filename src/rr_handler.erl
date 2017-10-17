-module(rr_handler).
-export([init/2]).

init(#{ method := Method, path := Path }=Req0, State) ->
	try handle_method(Method, Req0) of
		{ok, Re} -> {ok, Re, State}
	catch
		C:E ->
			lager:error("~p:~p path:~p ~p", [C, E, Path, erlang:get_stacktrace()]),
			{ok, Re} = reply(404, <<"not_found">>, Req0),
			{ok, Re, State}
	end.

reply(Code, Req) ->
	reply(Code, <<"ok">>, Req).
reply(Code, Msg, Req) ->
	cowboy_req:reply(Code, #{ <<"connection">> => <<"close">>, <<"content-type">> => <<"text/plain">>}, Msg, Req).

handle_method(<<"PUT">>, #{ path := Path}=Req) ->
	maybe_write(Path, Req);
handle_method(<<"GET">>, #{ path := Path }=Req) ->
	read(Path, Req).

maybe_write(<<"/", Path/binary>>=P, Req) ->
	case filelib:is_file(Path) of
		true ->
			reply(409, <<"file_exists">>, Req);
		false ->
			write(cowboy_req:read_body(Req), P)
	end.

write(<<"/">>, Req) -> reply(403, <<"path required">>, Req);
write({ok, Data, Req},  <<"/", Path/binary>>) ->
	lager:info("write file:~p size:~p", [Path, cowboy_req:body_length(Req)]),
	ensure_folder(filename:dirname(Path)),
	ok = file:write_file(Path, Data),
	{ok, reply(200, Req)};
write({more, Data, Req}, <<"/", Path/binary>>) ->
	lager:info("write file:~p size:~p", [Path, cowboy_req:body_length(Req)]),
	ensure_folder(filename:dirname(Path)),
	{ok, reply(200, write_append(Path, {more, Data, Req}))}.

write_append(Path, {ok, Data, Req}) ->
	ok = file:write_file(Path, Data, [append]),
	Req;
write_append(Path, {more, Data, Req}) ->
	ok = file:write_file(Path, Data, [append]),
	write_append(Path, cowboy_req:body(Req)).

read(<<"/", Path/binary>>, Req) ->
	{ok, Binary} = file:read_file(Path),
	cowboy_req:reply(200, [{<<"content-length">>, erlang:integer_to_binary(erlang:size(Binary))}], Binary, Req).

ensure_folder(Dir) ->
	ok = filelib:ensure_dir(<<Dir/binary, "/">>).
