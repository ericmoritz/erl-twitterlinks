# Twitter Link Processor

This is an application that listens for tweets from your fire hose and
publishes the links to delicious.

## Architecture
   
The twitterlinks_middleman is responsible for being the liaison
between the Twitter stream listener service and the Delicious service.

### Supervisor tree

Each twitter/delicious account has it's own middleman and middleman
supervisor.  Linked to the middleman is a twitter stream listener and 
a delicious gen_server, whose job is to post links to delicious.


    [MM_sup]: twitterlinks_middleman_sub
    (MM): twitterlinks_middleman
    (TW): twitterlinks_stream_listener
    (D): twitterlinks_delicious_service


     [MM_sup]
        | <one_for_one>
      (MM)
      /  \ 
    (TW) (D)

The delicious and twitter services are linked to their middleman, if
they die, they kill their middleman and its supervisor restarts the
middleman.

## Running

At the moment, there is no easy way to launch these application.  I
have to investigate how to package up an Erlang application.

For now, the process is manual.  First, copy sample.config and fill in
your values.

Second, build the application:

    ./rebar get-deps
    ./rebar compile

Third, run the Erlang console:

    erl -pa ebin -pa deps/*/ebin -s ssl -s inets -config your.config

At the prompt, execute:
    
    application:start(twitterlinks).



