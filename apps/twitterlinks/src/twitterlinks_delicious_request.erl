-module(twitterlinks_delicious_request).

-behavior(gen_server).

-record(request, {username, password, url, description, tags, tries}).

-export([start_link/6]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


start_link(Username, Password, Url, Description, Tags, MaxRetries) ->
    Request = #request{username=Username,
                       password=Password,
                       url=Url,
                       description=Description,
                       tags=Tags,
                       tries=MaxRetries},

    gen_server:start_link(?MODULE, [Request], []).

init([Request]) ->
    process_flag(trap_exit, true),
    {ok, Request, 0}.

handle_call(Msg, _From, State) ->
    {stop, {unknown_message, Msg}, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_message, Msg}, State}.

handle_info(timeout, Request) ->
    case spawn_request(Request) of
        max_retries ->
            {stop, max_retries, Request};
        {ok, Request1} ->
            {noreply, Request1}
    end;
handle_info({request, ok}, State) ->
    {stop, normal, State};
handle_info({'EXIT', _From, Reason}, State) ->
    error_logger:error_msg("Request failed, retrying ~p~n", [Reason]),
    % timeout after 1000 ms and retry the request
    {noreply, State, 1000}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% ---
%% Internal Function Definitions
%% ---

spawn_request(#request{tries=0}) ->
    max_retries;
spawn_request(#request{tries=Tries} = Request) ->
    Self = self(),
    spawn_link(fun() ->
                       Self ! {request, make_request(Request)}
               end),
    {ok, Request#request{tries=Tries - 1}}.


make_request(#request{username=Username,
                      password=Password,
                      url=Url,
                      description=Description,
                      tags=TagList}) ->

    HTTPRequest = build_add_request(Url, Description, TagList, Username,
                                    Password),

    % Crash on anything but 200, this service will get the 'EXIT' message
    % and retry the request
    Result = httpc:request(post, HTTPRequest, [{timeout, 4000}],[]),
    case Result of 
        {ok, {{_HTTPVsn, 200, _Reason}, _Headers, _Body}} ->
            ok;
        _ ->
            exit({request_failed, Result})
    end.



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

setup_meck(http, {error, Reason}) ->
    setup_meck({error, Reason});
setup_meck(http, Status) ->
    setup_meck({ok, {{'_HTTPVsn',
                 Status,
                 '_Reason'},
                '_Headers',
                '_Body'}}).

setup_meck(Response) ->
    meck:new(httpc),
    meck:expect(httpc, request, fun(post, _Request, 
                                    [{timeout, 4000}], _HttpOpts) ->
                                        Response
                                end).


make_request_200_test_() ->
    {setup,
     fun() -> setup_meck(http, 200),
              #request{username = "username",
                       password = "password",
                       url = "http://example.com/",
                       description = "this is the description",
                       tags = ["foo", "bar"],
                       tries = 3}
              end,
     fun(_Request) -> meck:unload(httpc) end,
     fun(Request) ->
             [?_assertEqual(ok, make_request(Request))]
     end}.

make_request_500_test_() ->
    {setup,
     fun() -> setup_meck(http, 500),
              #request{username = "username",
                       password = "password",
                       url = "http://example.com/",
                       description = "this is the description",
                       tags = ["foo", "bar"],
                       tries = 3}
              end,
     fun(_Request) -> meck:unload(httpc) end,
     fun(Request) ->
             [?_assertExit({request_failed, _}, make_request(Request))]
     end}.

make_request_net_error_test_() ->
    {setup,
     fun() -> setup_meck(http, {error, timeout}),
              #request{username = "username",
                       password = "password",
                       url = "http://example.com/",
                       description = "this is the description",
                       tags = ["foo", "bar"],
                       tries = 3}
              end,
     fun(_Request) -> meck:unload(httpc) end,
     fun(Request) ->
             [?_assertExit({request_failed, {error, timeout}}, make_request(Request))]
     end}.
 

spawn_request_good_test_() ->
    {setup,
     fun() ->
             setup_meck(http, 200),
             #request{username = "username",
                      password = "password",
                      url = "http://example.com/",
                      description = "this is the description",
                      tags = ["foo", "bar"],
                      tries = 3}
     end,
     fun(_Request) -> 
             meck:unload(httpc)
     end,
     fun(Request) ->
             {timeout, 1,
              [?_test(begin 
                          {ok, _Request1} = 
                              spawn_request(Request),
                          io:format("~p~n", [self()]),
                          Message = receive M -> M end,
                          ?assertEqual({request, ok}, Message)
                          end)]}
     end}.

spawn_request_bad_test_() ->
    {setup,
     fun() ->
             setup_meck(http, 500),
             #request{username = "username",
                      password = "password",
                      url = "http://example.com/",
                      description = "this is the description",
                      tags = ["foo", "bar"],
                      tries = 3}
     end,
     fun(_Request) -> 
             meck:unload(httpc)
     end,
     fun(Request) ->
             {timeout, 1,
              [?_test(begin 
                          process_flag(trap_exit, true),
                          {ok, _Request1} = 
                              spawn_request(Request),
                          io:format("~p~n", [self()]),
                          Message = receive M -> M end,
                          ?assertMatch({'EXIT', _Pid, {request_failed, _}}, Message)
                          end)]}
     end}.

start_link_good_test_() ->
    {setup,
     fun() ->
             setup_meck(http, 200),
             ok
     end,
     fun(_Arg) -> 
             meck:unload(httpc)
     end,
     fun(_Arg) ->
             {timeout, 1,
              [?_test(begin 
                          process_flag(trap_exit, true),
                          {ok, _Pid} = start_link("username", "password", "url",
                                                 "description", [], 3),
                          Message = receive M -> M end,
                          ?assertMatch({'EXIT', _Pid, normal}, Message)
                          end)]}

     end}.

start_link_bad_test_() ->
    {setup,
     fun() ->
             setup_meck(http, 500),
             ok
     end,
     fun(_Arg) -> 
             meck:unload(httpc)
     end,
     fun(_Arg) ->
             ?debugMsg("Ignore the request failed messages and server termination"),

             {timeout, 4,
              [?_test(begin 
                          process_flag(trap_exit, true),
                          {ok, _Pid} = start_link("username", "password", "url",
                                                 "description", [], 3),
                          Message = receive M -> M end,
                          ?assertMatch({'EXIT', _Pid, max_retries}, Message)
                          end)]}

     end}.

-endif.
