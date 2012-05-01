-module(twitterlinks_stream_listener).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).
-record(state, {middleman, url, request_id}).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(MiddlemanPid, Url) ->
    gen_server:start_link(?MODULE, [MiddlemanPid, Url], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([MiddlemanPid, Url]) ->
    {ok, ReqId} = start_stream(Url),
    {ok, #state{middleman=MiddlemanPid,url=Url,request_id=ReqId}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({http, {_ReqId, stream_start, _Headers}}, State) ->
    {noreply, State};
handle_info({http, {_, stream, BinBodyPart}}, State=#state{middleman=Pid}) ->
    Pid ! {tweet, BinBodyPart},
    {noreply, State};
handle_info({http, {_, stream_end, _}}, State) ->
    {stop, stream_end, State};
handle_info(Msg, State) ->
    io:format("Unknown Message: ~w", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    % TODO: close the httpc connection
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_stream(Url) ->
    % start the stream
    httpc:request(get, {Url, []},
                  [], [{sync, false}, {stream, self}]).
