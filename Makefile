all: compile

compile: getdeps
	rebar compile

getdeps:
	rebar get-deps

clean:
	rebar clean

test:
	rebar eunit skip_deps=true

demo: compile
	erl -pa deps/*/ebin apps/*/ebin -boot start_sasl -s twitterlinks -config test

rel:
	rebar generate
