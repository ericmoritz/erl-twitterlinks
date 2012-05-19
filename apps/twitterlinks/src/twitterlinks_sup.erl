
-module(twitterlinks_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Accounts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Accounts]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Accounts]) ->
    DeliciousSup = ?CHILD(twitterlinks_delicious_sup, supervisor, []),
    TwitterSup = ?CHILD(twitterlinks_twitter_sup, supervisor, []),
    AccountsWorker = ?CHILD(twitterlinks_accounts, worker, [Accounts]),

    Children = [DeliciousSup, TwitterSup, AccountsWorker],
    RestartStrategy = {one_for_all, 4, 3600},
    {ok, {RestartStrategy, Children}}.
