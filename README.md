# Twitter Link Processor

This is an application that listens for tweets from your fire hose and
publishes the links to delicious.

## Architecture
   
Each account has one twitterlinks_twitter process and one
twitterlinks_delicious process assigned to it.  Each account is
identified by an account id and this account id is used to look up the
PIDs by account id.

### Supervisor Tree

    [TL] = twitterlinks_sup supervisor
    [TW] = twitterlinks_twitter_sup supervisor
    [DL] = twitterlinks_delicious_sup supervisor
    (TW) = twitterlinks_twitter worker
    (DL) = twitterlinks_delicious worker

      [TL]
      /  \
    [DL] [TW]
     |    |
     |*   |*
    (DL) (TW)


## Creating a release

    make release

This creates a release of twitterlinks into `rel/twitterlinks`

## Configuring

After creating a release, you will need to configure it

    $EDITOR rel/twitterlinks/releases/1/sys.config 

Fill in the appropriate values

## Running

    cd rel/twitterlinks
    ./bin/twitterlinks start
