-module(twitterlinks_misc).
-export([http_auth_header/3, translate_tweet/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Public API

http_auth_header(basic, Username, Password) ->
    Encoded = base64:encode_to_string(lists:append([Username, ":", Password])),
    {"Authorization", "Basic " ++ Encoded}. 

translate_tweet(UserId, TweetBin) when is_binary(TweetBin) ->
    translate_tweet(UserId, mochijson:decode(TweetBin));
translate_tweet(UserId, Tweet) ->
    case struct:get_value({"user", "id"}, Tweet) of
        UserId ->
            % If the tweet's author id matches the user id we are
            % listening too, process the tweet for links
            Description = struct:get_value("text", Tweet),
            Tags = tweet_to_taglist(Tweet),
            Links = tweet_to_linklist(Tweet),
            
            [{Url, Description, Tags} || Url <- Links];
        _ ->
            % Ignore links tweeted by anyone but the User we're
            % listening to
            []
    end.

%% Internal functions

tweet_to_taglist(Tweet) ->
    case struct:get_value({"entities", "hashtags"}, Tweet) of
        {array, Tags} ->
            [struct:get_value("text", Item) || Item <- Tags];
        undefined ->
            []
    end.

tweet_to_linklist(Tweet) ->
    case struct:get_value({"entities", "urls"}, Tweet) of
        {array, Tags} ->
            [struct:get_value("expanded_url", Item) || Item <- Tags];
        undefined ->
            []
    end.


-ifdef(TEST).

-define(FIXTURE, <<"{
 \"text\": \"https://t.co/ZkG6Whj0 #json #erlang Jiffy JSON encoder, decoder.\",  
 \"user\": {\"id\": 12345},
 \"entities\": {
  \"user_mentions\": [], 
  \"hashtags\": [
   {
    \"indices\": [
     22, 
     27
    ], 
    \"text\": \"json\"
   }, 
   {
    \"indices\": [
     28, 
     35
    ], 
    \"text\": \"erlang\"
   }
  ], 
  \"urls\": [
   {
    \"indices\": [
     0, 
     21
    ], 
    \"url\": \"https://t.co/ZkG6Whj0\", 
    \"expanded_url\": \"https://github.com/davisp/jiffy\", 
    \"display_url\": \"github.com/davisp/jiffy\"
   }
  ]
 }, 
}">>).

translate_tweet_test() ->
    Expected = [{"https://github.com/davisp/jiffy",
                 "https://t.co/ZkG6Whj0 #json #erlang Jiffy JSON encoder, decoder.",
                  ["json", "erlang"]}],

    Result = translate_tweet(12345, ?FIXTURE),
    ?assertEqual(Expected, Result),

    Result2 = translate_tweet(12346, ?FIXTURE),
    ?assertEqual([], Result2),

    ok.

-endif.
