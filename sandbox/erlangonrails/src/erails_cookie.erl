-module(erails_cookie).
-include("erlangonrails.hrl").
-export([gen_cookie_of_session/1,
         gen_cookie/2,
         gen_cookie/3]).

%% @doc
%% 产生一个Cookie -> {_erails_session_id_, Sid} -> 准备发送给客户端
gen_cookie_of_session(Env) ->
    mochiweb_cookies:cookie(?ERAILS_SID, erails_var:get_session_id(Env), [{path, "/"}]).

gen_cookie(Key, Value) ->
    gen_cookie(Key, Value, [{path, "/"}]).

gen_cookie(Key, Value, Options) ->
    mochiweb_cookies:cookie(Key, Value, Options).

