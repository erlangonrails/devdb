-module(erails_cookie).
-include("erlangonrails.hrl").
-export([gen_cookie_of_session/1]).

%% @doc
%% 产生一个Cookie -> {_erails_session_id_, Sid} -> 准备发送给客户端
gen_cookie_of_session(Env) ->
     mochiweb_cookies:cookie(?ERAILS_SID, erails_var:get_session_id(Env), [{path, "/"}]).

