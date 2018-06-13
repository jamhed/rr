-module(rr_handler).
-export([init/2]).

uri_to_path(_, <<"/">>) -> erlang:error(path_required);
uri_to_path(Base, Uri) -> <<Base/binary, Uri/binary>>.

init(#{ method := Method, path := Path }=Req0, [BasePath]=State) ->
	try handle_method(Method, Req0, BasePath) of
		{ok, Re} ->
			{ok, Re, State};
		path_required ->
			{ok, reply(403, <<"path required">>, Req0), State};
		file_exists ->
			{ok, reply(409, <<"file exists">>, Req0), State};
		ok ->
			{ok, reply(200, <<"ok">>, Req0), State}
	catch
		error:{badmatch,{error,enoent}} ->
			lager:notice("no file:~s", [Path]),
			{ok, reply(404, <<"no file">>, Req0), State};
		C:E ->
			lager:error("~p:~p path:~p ~p", [C, E, Path, erlang:get_stacktrace()]),
			{ok, Re} = reply(404, <<"not_found">>, Req0),
			{ok, Re, State}
	end.

reply(Code, Req) ->
	reply(Code, <<"ok">>, Req).
reply(Code, Msg, Req) ->
	cowboy_req:reply(Code, #{ <<"connection">> => <<"close">>, <<"content-type">> => <<"text/plain">>}, Msg, Req).

handle_method(<<"PUT">>, #{ path := Path }=Req, Base) ->
	maybe_write(uri_to_path(Base, Path), Req);
handle_method(<<"GET">>, #{ path := Path }=Req, Base) ->
	read(uri_to_path(Base, Path), Req);
handle_method(<<"DELETE">>, #{ path := Path }=Req, Base) ->
	delete(uri_to_path(Base, Path), Req);
handle_method(<<"POST">>, #{ path := Path }=_Req, Base) ->
	lager:info("keep file:~s", [Path]),
	rr_swipe:keep(uri_to_path(Base, Path)),
	ok.

maybe_write(Path, Req) ->
	case filelib:is_file(Path) of
		true ->
			file_exists;
		false ->
			write(Path, cowboy_req:read_body(Req))
	end.

write(Path, {ok, Data, Req}) ->
	lager:info("write file:~s size:~p", [Path, cowboy_req:body_length(Req)]),
	ensure_folder(filename:dirname(Path)),
	ok = file:write_file(Path, Data),
	rr_swipe:swipe(Path),
	{ok, reply(200, Req)};

write(Path, {more, Data, Req}) ->
	lager:info("write file:~s size:~p", [Path, cowboy_req:body_length(Req)]),
	ensure_folder(filename:dirname(Path)),
	{ok, reply(200, write_append(Path, {more, Data, Req}))}.

write_append(Path, {ok, Data, Req}) ->
	ok = file:write_file(Path, Data, [append]),
	Req;
write_append(Path, {more, Data, Req}) ->
	ok = file:write_file(Path, Data, [append]),
	write_append(Path, cowboy_req:read_body(Req)).

read(Path, Req) ->
	lager:info("read file:~s", [Path]),
	{ok, Binary} = file:read_file(Path),
	{ok, cowboy_req:reply(200, #{ <<"content-length">> => erlang:integer_to_binary(erlang:size(Binary)) }, Binary, Req)}.

delete(Path, _Req) ->
	lager:info("delete file:~s", [Path]),
	ok = file:delete(Path).

ensure_folder(Dir) ->
	ok = filelib:ensure_dir(<<Dir/binary, "/">>).
