-module(twitterlinks_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = twitterlinks_sup:start_link(),
    {ok, Accounts} = application:get_env(twitterlinks, accounts),

    % starts the configured account pairs
    start_accounts(Accounts),

    {ok, Pid}.

start_accounts(Accounts) ->
    lists:foreach(fun(Account) ->
                          {AccountId, Props} = Account,
                          TwitterConfig = proplists:get_value(twitter, Props),
                          DeliciousConfig = proplists:get_value(delicious,
                                                                Props),
                          
                          twitterlinks:add_account(AccountId, TwitterConfig, 
                                                   DeliciousConfig)
                  end, Accounts).

stop(_State) ->
    ok.
