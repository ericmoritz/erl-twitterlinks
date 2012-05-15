# Twitter Link Processor

This is an application that listens for tweets from your firehose and
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
    (TW) = twitterlinks_twitter workers
    (DL) = twitterlinks_delicious workers

      [TL]
      /  \
    [DL] [TW]
     |    |
     |*   |*
    (DL) (TW)


## Run demo

   make demo

## Build release

   make rel
   $EDITOR rel/twitterlinks/releases/1/sys.config

Add your account to the sys config.
