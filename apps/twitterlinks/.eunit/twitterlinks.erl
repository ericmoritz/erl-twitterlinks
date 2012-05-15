-module(twitterlinks).

-export([start/0, add_account/3, del_account/1, publish_tweet/3]).

%% public API

start() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(inets),
    application:start(gproc),
    application:start(twitterlinks).

add_account(AccountId, {TwitUser, TwitPass, TwitUID},
            {DelUser, DelPass}) ->
    twitterlinks_delicious:create(AccountId, DelUser, DelPass),
    twitterlinks_twitter:create(AccountId, TwitUser, TwitPass, TwitUID),
    ok.

del_account(AccountId) ->
    twitterlinks_twitter:stop(AccountId),
    twitterlinks_delicious:stop(AccountId),
    ok.

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

