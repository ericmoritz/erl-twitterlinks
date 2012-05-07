-module(twitterlinks_misc).
-export([http_auth_header/3]).

http_auth_header(basic, Username, Password) ->
    Encoded = base64:encode_to_string(lists:append([Username, ":", Password])),
    {"Authorization", "Basic " ++ Encoded}. 
