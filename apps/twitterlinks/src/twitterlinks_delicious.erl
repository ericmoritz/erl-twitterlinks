-module(twitterlinks_delicious).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3, stop/1, create/3, publish_url/4, pid_for/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {account_id, username, password}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(AccountId, Username, Password) ->
    gen_server:start_link(?MODULE, [AccountId, Username, Password], []).

create(AccountId, Username, Password) ->
    twitterlinks_delicious_sup:start_child(AccountId, Username, Password).

publish_url(AccountId, Url, Description, TagList) ->
    Pid = pid_for(AccountId),
    gen_server:cast(Pid, {publish_url, {Url, Description, TagList}}).

stop(AccountId) ->
    case pid_for(AccountId) of
        undefined ->
            {error, not_found};
        Pid ->
            gen_server:call(Pid, stop)
    end.

pid_for(AccountId) ->
    gproc:lookup_pid({n,l, {delicious, account_id, AccountId}}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([AccountId, Username, Password]) ->
    gproc:reg({n, l, {delicious, account_id, AccountId}}, self()),
    {ok, #state{account_id=AccountId, username=Username, password=Password}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(state, _From, State) ->
    {reply, State, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({publish_url, {Url, Description, TagList}}, State) ->
    #state{username = Username, password = Password} = State,
    error_logger:info_msg("Publishing URL: ~p~n",
                          [{Url, Description, TagList}]),

    %% spawn a new request server. It will either succeed or
    %% retry every second.  If the two retries fail, it will die
    %% taking us with it.
    twitterlinks_delicious_request:start_link(Username,
                                              Password,
                                              Url,
                                              Description,
                                              TagList,
                                              3),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-ifdef(TEST).

pid_for_test_() ->
    {setup,
     fun() -> application:start(gproc) end,
     fun(_) -> application:stop(gproc) end,
     ?_test(begin
                {ok, Pid} = start_link(account_id, "username", "password"),
                Pid = pid_for(account_id)
                end)}.


-endif.
