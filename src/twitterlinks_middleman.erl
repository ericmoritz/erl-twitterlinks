-module(twitterlinks_middleman).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3, new_tweet/2]).

-record(state, {add_url_callback}).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(StreamUrl, DelUsername, DelPassword) ->
    gen_server:start_link(?MODULE, [StreamUrl, DelUsername, DelPassword], []).

new_tweet(ServerPid, TweetJSONBin) ->
   gen_server:call(ServerPid, {tweet, TweetJSONBin}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([StreamUrl, DelUsername, DelPassword]) ->
    {ok, DelPid} = twitterlinks_delicious_service:start_link(DelUsername, DelPassword),
    twitterlinks_stream_listener:start_link(self(), StreamUrl),

    AddUrlCallback = fun(Url, Description, Tags) ->
                             twitterlinks_delicious_service:add_url(DelPid, Url, Description, Tags)
                     end,

    {ok, #state{add_url_callback=AddUrlCallback}}.

handle_call({tweet, TweetJSONBin}, _From, State) ->
    io:format("Got new tweet! ~n", []),

    % translate the JSON
    Tweet = parse_tweet(TweetJSONBin),
    Urls = translate_tweet(Tweet),

    add_urls(State#state.add_url_callback, Urls),

    {reply, Urls, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
add_urls(Callback, [{Url, Description, TagList}|Links]) ->
    Callback(Url, Description, TagList),
    add_urls(Callback, Links);
add_urls(_, []) ->
    ok.
                                            

parse_tweet(TweetJSONBin) ->
    mochijson:decode(TweetJSONBin).

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
    

translate_tweet(Tweet) ->
    Description = struct:get_value("text", Tweet),
    Tags = tweet_to_taglist(Tweet),
    Links = tweet_to_linklist(Tweet),

    [{Url, Description, Tags} || Url <- Links].
    

-ifdef(TEST).

-define(FIXTURE, <<"{
 \"text\": \"https://t.co/ZkG6Whj0 #json #erlang Jiffy JSON encoder, decoder.\",  
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

incoming_tweet_test() ->
    ExpectedReply = [{"https://github.com/davisp/jiffy",
                      "https://t.co/ZkG6Whj0 #json #erlang Jiffy JSON encoder, decoder.",
                      ["json", "erlang"]}],
    
    AddUrlCallback = fun(Url, Description, Tags) ->
                             ?assertEqual("https://github.com/davisp/jiffy", Url),
                             ?assertEqual("https://t.co/ZkG6Whj0 #json #erlang Jiffy JSON encoder, decoder.", Description),
                             ?assertEqual(Tags, ["json", "erlang"])
                     end,

    State = #state{add_url_callback=AddUrlCallback},

    ?assertEqual({reply, ExpectedReply,State},
                 handle_call({tweet, ?FIXTURE}, from, State)).
                  

translate_tweet_test() ->
    Expected = [{"https://github.com/davisp/jiffy",
                 "https://t.co/ZkG6Whj0 #json #erlang Jiffy JSON encoder, decoder.",
                  ["json", "erlang"]}],

    Tweet = parse_tweet(?FIXTURE),
    Result = translate_tweet(Tweet),
    ?assertEqual(Expected, Result).

-endif.
