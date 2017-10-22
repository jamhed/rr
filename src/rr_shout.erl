-module(rr_shout).
-behaviour(gen_server).

-export([start_link/2, accept_loop/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	socket,
	handler
}).

start_link(Port, Handler) ->
	gen_server:start_link(?MODULE, [Port, Handler], []).

init([Port, Handler]) ->
	lager:notice("start", []),
	{ok, Listen} = gen_tcp:listen(Port, [binary, {packet, http_bin}, {reuseaddr, true}, {active, false}]),
	{ok, accept(#state{socket=Listen, handler=Handler})}.

accept_loop(Pid, Socket, {M, F}) ->
	{ok, Conn} = gen_tcp:accept(Socket),
	gen_server:cast(Pid, {accepted, self()}),
	M:F(Conn).

accept(State = #state{socket=Socket, handler=Handler}) ->
	proc_lib:spawn(?MODULE, accept_loop, [self(), Socket, Handler]),
	State.

handle_cast({accepted, _Pid}, S=#state{}) ->
	{noreply, accept(S)};

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S) ->
	lager:notice("terminate, reason:~p", [_Reason]),
	ok.

code_change(_OldVsn, S=#state{}, _Extra) ->
	{ok, S}.
