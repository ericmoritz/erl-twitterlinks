all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

release: compile
	# I'm not sure why I have to go into rel but I do
	cd rel && ../rebar generate

clean:
	./rebar clean