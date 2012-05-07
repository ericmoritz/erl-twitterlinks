-module(twitterlinks_delicious).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3, create/3, publish_url/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {account_id, username, password}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(AccountId, Username, Password) ->
    gen_server:start_link(?MODULE, [AccountId, Username, Password], []).

create(AccountId, Username, Password) ->
    {ok, Pid} = twitterlinks_delicious_sup:start_child(AccountId, Username, Password),
    twitterlinks_delicious_store:insert(AccountId, Pid),
    {ok, Pid}.

publish_url(AccountId, Url, Description, TagList) ->
    case twitterlinks_delicious_store:lookup(AccountId) of
        {ok, Pid} ->
            gen_server:cast(Pid, {publish_url, {Url, Description, TagList}});
        {error, not_found} ->
            % This could happen if the delicous service dies
            {error, delicious_service_down}
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([AccountId, Username, Password]) ->
    {ok, #state{account_id=AccountId, username=Username, password=Password}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({publish_url, {Url, Description, TagList}}, State) ->
    Request = build_add_request(Url, Description, TagList,
                                State#state.username, State#state.password),
    _Result = httpc:request(post, Request, [],[]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    twitterlinks_delicious_store:delete(self()),
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
    ServiceUrl = "https://api.del.icio.us/v1/posts/add",
    Headers = [twitterlinks_misc:http_auth_header(basic, Username, Password)],    
    Body = urlencode_list([{"url", Url},
                           {"description", Description},
                           {"tags", string:join(TagList, ",")}]),
    {ServiceUrl, Headers, "application/x-www-form-urlencoded", Body}.


-ifdef(TEST).

urlencode_list_test() ->
    Expected = "foo=this%20is%20a%20test&bar=other",
    Result = urlencode_list([{"foo", "this is a test"},
                             {"bar", "other"}]),
    ?assertEqual(Expected, Result).

build_add_request_test() ->
    Expected = {"https://api.del.icio.us/v1/posts/add",
                [{"Authentication", "Basic ZXJpY21vcml0ejp0ZXN0"}],
                "application/x-www-form-urlencoded",
                "url=http%3A%2F%2Fexample.com&description=this%20is%20the%20description&tags=foo%2Cbar"},

    Result = build_add_request("http://example.com",
                               "this is the description",
                               ["foo", "bar"], "ericmoritz", "test"),
    ?assertEqual(Expected, Result).

-endif.
