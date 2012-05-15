-module(twitterlinks_delicious).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3, create/3, publish_url/4, pid_for/1]).

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
    twitterlinks_delicious_sup:start_child(AccountId, Username, Password).

publish_url(AccountId, Url, Description, TagList) ->
    case pid_for(AccountId) of
        {ok, Pid} ->
            gen_server:call(Pid, {publish_url, {Url, Description, TagList}});
        {error, not_found} ->
            % This could happen if the delicous service dies
            {error, delicious_service_down}
    end.

pid_for(AccountId) ->
    case gproc:lookup_pid({n,l, {account_id, AccountId}}) of
        undefined ->
            {error, not_found};
        Pid ->
            {ok, Pid}
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([AccountId, Username, Password]) ->
    gproc:reg({n, l, {account_id, AccountId}}, self()),
    {ok, #state{account_id=AccountId, username=Username, password=Password}}.

handle_call({publish_url, {Url, Description, TagList}}, _From, State) ->
    Request = build_add_request(Url, Description, TagList,
                                State#state.username, State#state.password),
    Reply = case httpc:request(post, Request, [],[]) of
                {ok, {{_HTTPVsn, 200, _Reason}, _Headers, _Body}} ->
                    ok;
                {ok, {{_HTTPVsn, Status, Reason}, _Headers, _Body}} ->
                    {error, {http, Status, Reason}};
                {error, Reason} -> {error, Reason}
            end,
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

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
                [{"Authorization", "Basic ZXJpY21vcml0ejp0ZXN0"}],
                "application/x-www-form-urlencoded",
                "url=http%3A%2F%2Fexample.com&description=this%20is%20the%20description&tags=foo%2Cbar"},

    Result = build_add_request("http://example.com",
                               "this is the description",
                               ["foo", "bar"], "ericmoritz", "test"),
    ?assertEqual(Expected, Result).

init_test_() ->
    {setup,
     fun() -> application:start(gproc) end,
     fun(_Arg) -> application:stop(gproc) end,
     ?_test(begin
                {ok, Result} = init([ericmoritz, username, password]),
    
                Expected = #state{account_id=ericmoritz,
                                  username=username, password=password},

                ?assertEqual(Expected, Result),
    
                ?assertEqual(gproc:get_value({n, l, {account_id, ericmoritz}}),
                             self()),
                ok end)}.

handle_publish_url_200_test_() ->
    {setup,
     fun() -> meck:new(httpc),
              meck:expect(httpc, request, fun(post, _Request, [], []) ->
                                                  {ok, {{'_HTTPVsn',
                                                         200,
                                                         '_Reason'},
                                                        '_Headers',
                                                        '_Body'}}
                                          end) end,
     fun(_Arg) -> meck:unload(httpc) end,
     ?_test(begin
                State = #state{username="username",
                               password="password"},
                UrlTuple = {"http://example.com",
                            "this is the description", ["foo", "bar"]},

                ?assertEqual({reply, ok, State},
                             handle_call({publish_url, UrlTuple},
                                         self(), State)),
                ok
            end)
     }.

handle_publish_url_non200_test_() ->
    {setup,
     fun() -> meck:new(httpc),
              meck:expect(httpc, request, fun(post, _Request, [], []) ->
                                                  {ok, {{'_HTTPVsn',
                                                         404,
                                                         "Not Found"},
                                                        '_Headers',
                                                        '_Body'}}
                                          end) end,
     fun(_Arg) -> meck:unload(httpc) end,
     ?_test(begin
                State = #state{username="username",
                               password="password"},
                UrlTuple = {"http://example.com",
                            "this is the description", ["foo", "bar"]},

                ?assertEqual({reply, {error, {http, 404, "Not Found"}}, State},
                             handle_call({publish_url, UrlTuple},
                                         self(), State)),
                ok
            end)
     }.

handle_publish_url_httpc_error_test_() ->
    {setup,
     fun() -> meck:new(httpc),
              meck:expect(httpc, request, fun(post, _Request, [], []) ->
                                                  {error, reason}
                                          end) end,
     fun(_Arg) -> meck:unload(httpc) end,
     ?_test(begin
                State = #state{username="username",
                               password="password"},
                UrlTuple = {"http://example.com",
                            "this is the description", ["foo", "bar"]},

                ?assertEqual({reply, {error, reason}, State},
                             handle_call({publish_url, UrlTuple},
                                         self(), State)),
                ok
            end)
     }.

pid_for_test_() ->
    {setup,
     fun() -> application:start(gproc) end,
     fun(_) -> application:stop(gproc) end,
     ?_test(begin
                {ok, Pid} = start_link(account_id, "username", "password"),
                {ok, Pid} = pid_for(account_id)
                end)}.

-endif.
