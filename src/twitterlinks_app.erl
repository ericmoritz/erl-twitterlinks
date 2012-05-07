-module(twitterlinks_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    twitterlinks_delicious_store:init(),
    twitterlinks_twitter_store:init(),

    {ok, Pid} = twitterlinks_sup:start_link(),
    {ok, Accounts} = application:get_env(accounts),

    % starts the configured account pairs
    start_accounts(Accounts),

    {ok, Pid}.

start_accounts([]) ->
    ok;
start_accounts([Account|Rest]) ->
    {AccountId, TwitterConfig, DeliciousConfig} = Account,
    twitterlinks:add_account(AccountId, TwitterConfig, DeliciousConfig),
    start_accounts(Rest).

stop(_State) ->
    ok.
