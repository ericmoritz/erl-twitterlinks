-module(twitterlinks_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Accounts} = application:get_env(twitterlinks, accounts),
    twitterlinks_sup:start_link(Accounts).

stop(_State) ->
    ok.
