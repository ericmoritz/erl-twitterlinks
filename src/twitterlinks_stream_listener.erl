-module(twitterlinks_stream_listener).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2, start_stream/3]).
-record(state, {middleman, url, request_id, new_tweet_callback}).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(MiddlemanPid, {Username, Password, UserId}) ->
    gen_server:start_link(?MODULE, [MiddlemanPid, Username, Password, UserId], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([MiddlemanPid, Username, Password, UserId]) ->
    NewTweetCallback = fun(TweetJSONBin) ->
                               twitterlinks_middleman:new_tweet(MiddlemanPid,
                                                                      TweetJSONBin)
                       end,
    {ok, ReqId} = start_stream(Username, Password, UserId),
    {ok, #state{request_id=ReqId, new_tweet_callback=NewTweetCallback}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({http, {_ReqId, stream_start, _Headers}}, State) ->
    io:format("stream listener: stream started~n", []),
    {noreply, State};
handle_info({http, {_, stream, <<"\r\n">>}}, State) ->
    % Twitter Heartbeat
    {noreply, State};
handle_info({http, {_, stream, BinBodyPart}}, State=#state{new_tweet_callback=NewTweetCallback}) ->
    io:format("stream listener: new tweet~n", []),
    NewTweetCallback(BinBodyPart),
    {noreply, State};
handle_info({http, {_, stream_end, _}}, State) ->
    % TODO: start the stream again
    {stop, stream_end, State};
handle_info(Msg, State) ->
    io:format("Unknown Message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    % TODO: close the httpc connection
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_stream(Username, Password, UserId) when is_integer(UserId) ->
    Url = "https://stream.twitter.com/1/statuses/filter.json?follow=" ++ integer_to_list(UserId),
    Headers = [twitterlinks_misc:http_auth_header(basic, Username, Password)],

    io:format("Starting Stream ~s ~p ~n", [Url, Headers]),
    
    % start the stream
    httpc:request(get, {Url, Headers},
                  [], [{sync, false}, {stream, self}]).


-ifdef(TEST).

stream_start_test() ->
    Expected = {noreply, state},
    Result = handle_info({http, {reqid, stream_start, headers}}, state),
    ?assertEqual(Expected, Result).

stream_test() ->
    MockCallback = fun(Data) -> ?assertEqual("dummy tweet", Data) end,
    State = #state{new_tweet_callback=MockCallback},
    Result = handle_info({http, {reqid, stream, "dummy tweet"}}, State),
    ?assertEqual(Result, {noreply, State}).

stream_end_test() ->
    Expected = {stop, stream_end, state},
    Result = handle_info({http, {reqid, stream_end, headers}}, state),
    ?assertEqual(Expected, Result).

-endif.
