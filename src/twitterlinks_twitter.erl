-module(twitterlinks_twitter).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/4, create/4, start_stream/3]).
-record(state, {account_id, request_id}).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(AccountId, Username, Password, UserId) ->
    gen_server:start_link(?MODULE, [AccountId, Username, Password, UserId], []).

create(AccountId, Username, Password, UserId) ->
    {ok, Pid} = twitterlinks_twitter_sup:start_child(AccountId, Username, Password, UserId),
    twitterlinks_twitter_store:insert(AccountId, Pid),
    {ok, Pid}.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([AccountId, Username, Password, UserId]) ->
    {ok, ReqId} = start_stream(Username, Password, UserId),
    {ok, #state{account_id=AccountId, request_id=ReqId}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({http, {_ReqId, stream_start, _Headers}}, State) ->
    error_logger:info_report("stream listener: stream started~n", []),
    {noreply, State};
handle_info({http, {_, stream, <<"\r\n">>}}, State) ->
    % Twitter Heartbeat
    {noreply, State};
handle_info({http, {_, stream, TweetJSON}}, State) ->
    error_logger:info_report("stream listener: new tweet~n"),
    twitterlinks:publish_tweet(State#state.account_id, TweetJSON),
    {noreply, State};
handle_info({http, {_, stream_end, _}}, State) ->
    % TODO: start the stream again
    error_logger:info_report("stream stopped."),
    {stop, stream_end, State};
handle_info(Msg, State) ->
    error_logger:warning_report("Unknown Message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, State) ->
    twitterlinks_twitter_store:delete(self()),
    httpc:cancel_request(State#state.request_id),
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

-endif.
