
-module(twitterlinks_middleman_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(StreamUrl, DelUsername, DelPassword) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [StreamUrl, DelUsername, DelPassword]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([StreamUrl, DelUsername, DelPassword]) ->
    {ok, { {one_for_one, 5, 10}, [
                                  ?CHILD(twitterlinks_middleman, worker, [StreamUrl, DelUsername, DelPassword])
                                 ]}}.

