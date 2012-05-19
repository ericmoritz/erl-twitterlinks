-module(twitterlinks_twitter).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/4, stop/1, create/4, start_stream/3, pid_for/1]).
-record(state, {account_id, request_id, user_id, username, password}).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(AccountId, Username, Password, UserId) ->
    gen_server:start_link(?MODULE, [AccountId, Username, Password, UserId],
                          []).

create(AccountId, Username, Password, UserId) ->
    twitterlinks_twitter_sup:start_child(AccountId,
                                         Username, Password, UserId).

pid_for(AccountId) ->
    gproc:lookup_pid({n,l, {twitter, account_id, AccountId}}).
    
stop(AccountId) ->
    case pid_for(AccountId) of
        undefined ->
            {error, not_found};
        Pid ->
            gen_server:call(Pid, stop)
    end.
            
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([AccountId, Username, Password, UserId]) ->
    gproc:reg({n, l, {twitter, account_id, AccountId}}, self()),
    {ok, #state{account_id=AccountId, user_id=UserId,
                username=Username, password=Password}, 0}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({http, {_ReqId, stream_start, _Headers}}, State) ->
    error_logger:info_msg("stream listener: stream started~n", []),
    {noreply, State};
handle_info({http, {_, stream, <<"\r\n">>}}, State) ->
    % Twitter Heartbeat
    {noreply, State};
handle_info({http, {_, stream, TweetJSON}}, State) ->
    error_logger:info_msg("stream listener: new tweet:~n ~s~n", [TweetJSON]),
    twitterlinks:publish_tweet(State#state.account_id,
                               State#state.user_id,
                               TweetJSON),
    {noreply, State};
handle_info({http, {_, stream_end, _}}, State) ->
    error_logger:info_msg("stream stopped.~n"),
    {noreply, State, 0};
handle_info({http, {_, {error, Reason}}}, State) ->
    error_logger:info_msg("stream error ~p~n", [Reason]),
    {noreply, State, 1000};
handle_info(timeout, #state{username = Username,
                            password = Password,
                            user_id = UserId} = State) ->
    {ok, ReqId} = start_stream(Username, Password, UserId),
    {noreply, State#state{request_id=ReqId}};
handle_info(Msg, State) ->
    error_logger:warning_msg("Unknown Message: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_stream(Username, Password, UserId) when is_integer(UserId) ->
    Url = "https://stream.twitter.com/1/statuses/filter.json?follow="
        ++ integer_to_list(UserId),
    Headers = [twitterlinks_misc:http_auth_header(basic, Username, Password)],

    io:format("Starting Stream ~s ~p ~n", [Url, Headers]),
    
    % start the stream
    httpc:request(get, {Url, Headers},
                  [], [{sync, false}, {stream, self}]).


-ifdef(TEST).

pid_for_test_() ->
    {setup,
     fun() -> 
             meck:new(httpc),
             meck:expect(httpc, request, fun(_,_,_,_) -> {ok, request_id} end),
             application:start(gproc)
     end,
     fun(_) ->
             meck:unload(httpc),
             application:stop(gproc)
     end,
     ?_test(begin
                {ok, Pid} = start_link(account_id, "username", "password", 12345),
                Pid = pid_for(account_id)
                end)}.

-endif.
