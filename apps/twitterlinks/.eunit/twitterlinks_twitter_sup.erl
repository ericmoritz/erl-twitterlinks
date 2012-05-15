-module(twitterlinks_twitter_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/4, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(AccountId, Username, Password, UserId) ->
    supervisor:start_child(?SERVER, [AccountId, Username, Password, UserId]).

init([]) ->
    Twitter = {twitterlinks_twitter, {twitterlinks_twitter, start_link, []},
               transient, 1000, worker, [twitterlinks_twitter]},
    Children = [Twitter],
    RestartStrategy = {simple_one_for_one, 4, 3600},
    {ok, {RestartStrategy, Children}}.


