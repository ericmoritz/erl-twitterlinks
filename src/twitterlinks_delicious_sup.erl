-module(twitterlinks_delicious_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/3, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(AccountId, Username, Password) ->
    supervisor:start_child(?SERVER, [AccountId, Username, Password]).

init([]) ->
    Delicious = {twitterlinks_delicious, {twitterlinks_delicious, start_link, []},
               transient, 1000, worker, [twitterlinks_delicious]},
    Children = [Delicious],
    RestartStrategy = {simple_one_for_one, 4, 3600},
    {ok, {RestartStrategy, Children}}.


