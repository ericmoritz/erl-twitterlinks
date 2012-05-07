
-module(twitterlinks_middleman_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(TwitterSettings, DeliciousSettings) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [TwitterSettings, DeliciousSettings]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([TwitterSettings, DeliciousSettings]) ->
    {ok, { {one_for_one, 5, 10}, [
                                  ?CHILD(twitterlinks_middleman, worker, [TwitterSettings, DeliciousSettings])
                                 ]}}.

