-module(twitterlinks).

-export([add_account/3, del_account/1, publish_tweet/2]).

%% public API

add_account(AccountId, {TwitUser, TwitPass, TwitUID},
            {DelUser, DelPass}) ->
    twitterlinks_delicious:create(AccountId, DelUser, DelPass),
    twitterlinks_twitter:create(AccountId, TwitUser, TwitPass, TwitUID),
    ok.

del_account(AccountId) ->
    twitterlinks_twitter:stop(AccountId),
    twitterlinks_delicious:stop(AccountId),
    ok.

publish_tweet(AccountId, TweetJSONBin) ->
    Links = twitterlinks_misc:translate_tweet(TweetJSONBin),
    publish_links(AccountId, Links).

%% Internal functions

publish_links(_AccountId, []) ->
    ok;
publish_links(AccountId, [Link|Rest]) ->
    {Url, Description, TagList} = Link,
    twitterlinks_delicious:publish_url(AccountId, Url, Description, TagList),
    publish_links(AccountId, Rest).
    
