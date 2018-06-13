devel:
	rebar3 as devel release

release:
	rebar3 as prod release

clean:
	rebar3 clean

clean-all:
	rebar3 clean -a
	rm -f rebar.lock
	rm -rf _build/

dialyzer: 
	rebar3 dialyzer

xref: 
	rebar3 xref

test:
	rebar3 eunit

console: devel
	_build/devel/rel/rr/bin/rr console
