-module(twitterlinks_delicious_service).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_url/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {username, password}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link({Username, Password}) ->
    gen_server:start_link(?MODULE, [Username, Password], []).

add_url(ServerPid, Url, Description, TagList) ->
    gen_server:call(ServerPid, {add_url, {Url, Description, TagList}}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Username, Password]) ->
    {ok, #state{username=Username, password=Password}}.

handle_call({add_url, {Url, Description, TagList}}, _From, State) ->
    Request = build_add_request(Url, Description, TagList,
                                State#state.username, State#state.password),
    Result = httpc:request(post, Request, [],[]),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

urlencode_list(Params) ->
    string:join(lists:map(fun({Key, Value}) -> Key ++ "=" ++ http_uri:encode(Value) end,
                            Params), "&").

build_add_request(Url, Description, TagList, Username, Password) ->
    ServiceUrl = "https://" ++ Username ++ ":" ++ Password ++
        "@api.del.icio.us/v1/posts/add",
    Body = urlencode_list([{"url", Url},
                           {"description", Description},
                           {"tags", string:join(TagList, ",")}]),
    {ServiceUrl, [], "application/x-www-form-urlencoded", Body}.


-ifdef(TEST).

urlencode_list_test() ->
    Expected = "foo=this%20is%20a%20test&bar=other",
    Result = urlencode_list([{"foo", "this is a test"},
                             {"bar", "other"}]),
    ?assertEqual(Expected, Result).

build_add_request_test() ->
    Expected = {"https://ericmoritz:test@api.del.icio.us/v1/posts/add",
                [], "application/x-www-form-urlencoded",
                "url=http%3A%2F%2Fexample.com&description=this%20is%20the%20description&tags=foo%2Cbar"},

    Result = build_add_request("http://example.com",
                               "this is the description",
                               ["foo", "bar"], "ericmoritz", "test"),
    ?assertEqual(Expected, Result).

-endif.
