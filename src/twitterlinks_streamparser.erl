-module(twitterlinks_streamparser).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start_stream/1]).
-record(state, {clients, requestbuffer}).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_stream(Url) ->
    gen_server:call(?MODULE, {stream, Url}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{clients=orddict:new(),
                requestbuffer=[]}}.


handle_call({stream, URL}, {Pid, _}, State=#state{clients=Clients}) ->
    % start the stream
    {ok, ReqId} = httpc:request(get, {URL, []},
                                [], [{sync, false}, {stream, self}]),

    % register the listener with it's ReqId
    Clients1 = orddict:store(ReqId, Pid, Clients),

    % reply.
    {reply, {ok, ReqId}, State#state{clients=Clients1}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({http, {_ReqId, stream_start, _Headers}}, State) ->
    {noreply, State};
handle_info({http, {ReqId, stream, BinBodyPart}},
            State=#state{clients=Clients}) ->
    Pid = orddict:fetch(ReqId, Clients),

    % TODO: Process BinBodyPart for lines
    Pid ! {tweet, BinBodyPart},

    {noreply, State};

handle_info({http, {ReqId, stream_end, _Headers}}, State=#state{clients=Clients}) ->
    {noreply, State#state{clients=orddict:erase(ReqId, Clients)}};
handle_info(Msg, State) ->
    io:format("Unknown Message: ~w", [Msg]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

