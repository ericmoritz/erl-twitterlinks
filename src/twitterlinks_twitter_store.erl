-module(twitterlinks_twitter_store).

-export([init/0, insert/2, lookup/1, delete/1]).

-define(TABLE_ID, ?MODULE).

init() ->
    ets:new(?TABLE_ID, [public, named_table]).

insert(AccountId, Pid) ->
    ets:insert(?TABLE_ID, {AccountId, Pid}).

lookup(AccountId) ->
    case ets:lookup(?TABLE_ID, AccountId) of
        [{AccountId, Pid}] -> {ok, Pid};
        [] -> {error, not_found}
    end.

delete(Pid) ->
    ets:match_delete(?TABLE_ID, {'_', Pid, '_'}).

