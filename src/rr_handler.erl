-module(rr_handler).
-export([init/2]).

init(#{ method := Method, path := Path }=Req0, State) ->
	try handle_method(Method, Req0) of
		{ok, Re} ->
			{ok, Re, State};
		path_required ->
			{ok, reply(403, <<"path required">>, Req0), State};
		file_exists ->
			{ok, reply(409, <<"file exists">>, Req0), State};
		ok ->
			{ok, reply(200, <<"ok">>, Req0), State}
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

handle_method(<<"PUT">>, #{ path := Path }=Req) ->
	maybe_write(Path, Req);
handle_method(<<"GET">>, #{ path := Path }=Req) ->
	read(Path, Req);
handle_method(<<"POST">>, #{ path := <<"/", Path/binary>> }=_Req) ->
	lager:info("keep file:~s", [Path]),
	rr_swipe:keep(Path),
	ok.

maybe_write(<<"/", Path/binary>>=P, Req) ->
	case filelib:is_file(Path) of
		true ->
			file_exists;
		false ->
			write(cowboy_req:read_body(Req), P)
	end.

write(_, <<"/">>) -> path_required;

write({ok, Data, Req},  <<"/", Path/binary>>) ->
	lager:info("write file:~s size:~p", [Path, cowboy_req:body_length(Req)]),
	ensure_folder(filename:dirname(Path)),
	ok = file:write_file(Path, Data),
	rr_swipe:swipe(Path),
	{ok, reply(200, Req)};

write({more, Data, Req}, <<"/", Path/binary>>) ->
	lager:info("write file:~s size:~p", [Path, cowboy_req:body_length(Req)]),
	ensure_folder(filename:dirname(Path)),
	{ok, reply(200, write_append(Path, {more, Data, Req}))}.

write_append(Path, {ok, Data, Req}) ->
	ok = file:write_file(Path, Data, [append]),
	Req;
write_append(Path, {more, Data, Req}) ->
	ok = file:write_file(Path, Data, [append]),
	write_append(Path, cowboy_req:read_body(Req)).

read(<<"/", Path/binary>>, Req) ->
	lager:info("read file:~s", [Path]),
	{ok, Binary} = file:read_file(Path),
	{ok, cowboy_req:reply(200, #{ <<"content-length">> => erlang:integer_to_binary(erlang:size(Binary)) }, Binary, Req)}.

ensure_folder(Dir) ->
	ok = filelib:ensure_dir(<<Dir/binary, "/">>).
