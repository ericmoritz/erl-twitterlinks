
-module(twitterlinks_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_) ->
    {ok, StreamUrl} = application:get_env(twitterlinks, stream_url),
    {ok, {DelUsername, DelPassword}} = application:get_env(twitterlinks, delicious),

    {ok, { {one_for_one, 5, 10}, [
                                  ?CHILD(twitterlinks_middleman_sup, supervisor,
                                         [StreamUrl, DelUsername, DelPassword])

                                 ] }}.

