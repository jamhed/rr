-module(rr_swipe).
-behaviour(gen_server).

-define(SWIPE_TIMEOUT, 3600).
-define(KEEP_TIMEOUT, 3600).

-export([
	start_link/0, keep/1, swipe/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	keep = #{},
	swipe = #{}
}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

keep(File) -> gen_server:call(?MODULE, {keep, File}).
swipe(File) -> gen_server:call(?MODULE, {swipe, File}).

init([]) ->
	lager:notice("start", []),
	erlang:send_after(1000, self(), swipe),
	{ok, #state{}}.

handle_cast(_Msg, S=#state{}) ->
	lager:error("unhandled cast:~p", [_Msg]),
	{noreply, S}.

handle_info(swipe, S=#state{swipe=Swipe, keep=Keep}) ->
	erlang:send_after(1000, self(), swipe),
	{noreply, S#state{swipe=swipe_map(Swipe), keep=swipe_keep(Keep)}};

handle_info(_Info, S=#state{}) ->
	lager:error("unhandled info:~p", [_Info]),
	{noreply, S}.

handle_call({keep, File}, _, S=#state{keep=Keep, swipe=Swipe}) ->
	case maps:get(File, Swipe, undefined) of
		undefined ->
			{reply, keep, S#state{keep=Keep#{ File => ts() }}};
		_ ->
			{reply, remove, S#state{swipe=maps:remove(File, Swipe)}}
	end;

handle_call({swipe, File}, _, S=#state{keep=Keep, swipe=Swipe}) ->
	case maps:get(File, Keep, undefined) of
		undefined ->
			{reply, swipe, S#state{swipe=Swipe#{ File => ts() }}};
		_ ->
			{reply, keep, S}
	end;

handle_call(_Request, _From, S=#state{}) ->
	lager:error("unhandled call:~p", [_Request]),
	{reply, ok, S}.

terminate(_Reason, _S) ->
	lager:notice("terminate, reason:~p", [_Reason]),
	ok.

code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

ts() -> erlang:monotonic_time(second).

swipe_keep(M) ->
	Keys = [ K || {K, V} <- maps:to_list(M), ts() - V > ?KEEP_TIMEOUT ],
	lists:foldl(fun(K, Acc) -> maps:remove(K, Acc) end, M, Keys).

swipe_map(M) ->
	Keys = [ K || {K, V} <- maps:to_list(M), ts() - V > ?SWIPE_TIMEOUT ],
	swipe_files(Keys),
	lists:foldl(fun(K, Acc) -> maps:remove(K, Acc) end, M, Keys).

swipe_files([]) -> ok;
swipe_files([File|Rest]) ->
	lager:info("swipe:~p", [File]),
	file:delete(File),
	swipe_files(Rest).
