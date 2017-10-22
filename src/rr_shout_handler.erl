-module(rr_shout_handler).
-export([loop/1]).

-define(CHUNK_SIZE, 96).

-record(state, {
	uri
}).

loop(Socket) -> loop(Socket, #state{}).

loop(Socket, S=#state{uri=Uri}) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, http_eoh} ->
			send_file(Socket, Uri);
		{ok, {http_request,'GET', {abs_path, NewUri}, _}} ->
			loop(Socket, #state{uri=NewUri});
		{ok, _Header} ->
			loop(Socket, S);
		{error, closed} ->
			ok
	end.

response() -> [
	"ICY 200 OK\r\n",
	"content-type: audio/mpeg\r\n",
	"icy-pub: 1\r\n",
	"icy-metaint: ",integer_to_list(?CHUNK_SIZE),"\r\n",
	"\r\n"
].

shout_header() ->
	shout_header(<<"StreamTitle='H';">>).

shout_header(Info) ->
	Nblocks = ((size(Info) - 1) div 16) + 1,
	NPad = Nblocks*16 - size(Info),
	Extra = lists:duplicate(NPad, 0),
	list_to_binary([Nblocks, Info, Extra]).

send_file(Socket, <<"/", Path/binary>>) ->
	lager:info("shoutcast file:~p", [Path]),
	ok = gen_tcp:send(Socket, [response()]),
	{ok, Fh} = file:open(Path, [read, binary, raw]),
	stream_file(Socket, Fh).

stream_file(Socket, Fh) -> stream_file(Socket, Fh, 0, <<>>).

stream_file(Socket, Fh, Offset, Tail) ->
	ChunkSize = ?CHUNK_SIZE - byte_size(Tail),
	case file:pread(Fh, Offset, ChunkSize) of
		{ok, FileData} when byte_size(<<Tail/binary, FileData/binary>>) =:= ?CHUNK_SIZE ->
			ok = gen_tcp:send(Socket, [Tail, FileData, shout_header()]),
			stream_file(Socket, Fh, Offset+ChunkSize, <<>>);
		{ok, FileData} ->
			stream_file(Socket, Fh, 0, <<Tail/binary, FileData/binary>>);
		eof when Offset =:= 0, byte_size(Tail) =:= 0 ->
			lager:notice("zero file");
		eof when byte_size(Tail) =:= ?CHUNK_SIZE ->
			ok = gen_tcp:send(Socket, [Tail, shout_header()]),
			stream_file(Socket, Fh, 0, <<>>);
		eof ->
			stream_file(Socket, Fh, 0, Tail);
		{error, Reason} ->
			lager:notice("file error:~p", [Reason])
	end.

