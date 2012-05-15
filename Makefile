all: compile

compile: deps
	./rebar compile

deps:
	./rebar get-deps

release: compile
	# I'm not sure why I have to go into rel but I do
	cd rel && ../rebar generate

clean:
	./rebar clean

demo:
	erl -pa deps/*/ebin apps/*/ebin -boot start_sasl -s twitterlinks -config test
