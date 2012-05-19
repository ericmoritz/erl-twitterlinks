-module(twitterlinks_accounts).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {accounts}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_account/3, stop_account/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Accounts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Accounts], []).

add_account(AccountId, TwitCred, DelCred) ->
    gen_server:call(?SERVER, {add_account, AccountId, TwitCred, DelCred}).

stop_account(AccountId) ->
    gen_server:call(?SERVER, {stop_account, AccountId}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Accounts]) ->
    {ok, #state{accounts=Accounts}, 0}.

handle_call({add_account, AccountId, TwitCred, DelCred}, _From, State) ->
    {DelUser, DelPass} = DelCred,
    {TwitUser, TwitPass, TwitUID} = TwitCred,
    twitterlinks_delicious:create(AccountId, DelUser, DelPass),
    twitterlinks_twitter:create(AccountId, TwitUser, TwitPass, TwitUID),
    {reply, ok, State};
handle_call({stop_account, AccountId}, _From, State) ->
    twitterlinks_twitter:stop(AccountId),
    twitterlinks_delicious:stop(AccountId),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.
    
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{accounts=Accounts} = State) ->
    lists:foreach(fun(Account) ->
                          {AccountId, Props} = Account,
                          TwitCred = proplists:get_value(twitter, Props),
                          DelCred = proplists:get_value(delicious, Props),

                          handle_call({add_account,
                                       AccountId,
                                       TwitCred,
                                       DelCred}, self(), State)

                  end, Accounts),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

