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

## Creating a release

    make release

This creates a release of twitterlinks into `rel/twitterlinks`

## Configuring

After creating a release, you will need to configure it

    $EDITOR rel/twitterlinks/releases/1/sys.config 

Fill in the stream_url and delicious properties with your own values.

## Running

    cd rel/twitterlinks
    ./bin/twitterlinks start
