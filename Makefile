all: compile

compile: deps
	rebar compile

deps: rebar.config
	rebar get-deps

clean:
	rebar clean

test:
	rebar eunit skip_deps=true

demo: compile
	erl -pa deps/*/ebin apps/*/ebin -boot start_sasl -s twitterlinks -config test

rel: compile
	rebar generate

relclean:
	rm -rf rel/twitterlinks
