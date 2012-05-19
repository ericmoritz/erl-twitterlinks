-module(twitterlinks).

-export([start/0, publish_tweet/3]).

%% public API

start() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(inets),
    application:start(gproc),
    application:start(twitterlinks).

publish_tweet(AccountId, UserId, TweetJSONBin) ->
    Links = twitterlinks_misc:translate_tweet(UserId, TweetJSONBin),
    publish_links(AccountId, Links).

%% Internal functions

publish_links(_AccountId, []) ->
    ok;
publish_links(AccountId, [Link|Rest]) ->
    {Url, Description, TagList} = Link,
    twitterlinks_delicious:publish_url(AccountId, Url, Description, TagList),
    publish_links(AccountId, Rest).

