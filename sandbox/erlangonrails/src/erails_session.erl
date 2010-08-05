-module(erails_session).
-include("erlangonrails.hrl").
-export([setup/1]).

%% <1> 如果Cookie中包含sid, 则首先判断这个sid是否在session server中存在, 如果是, 则继续使用;
%%     否则创建一个新的sid.
%% <2> 更新Env, 增加{erails_sid, Sid}
setup(Env) ->
    %% erails_var:get_cookie/2 返回sid或者undefined
    SessionKey = erails_session_server:new_session(erails_var:get_cookie(?ERAILS_SID, Env)),
    %% 增加 {erails_sid, SessionKey} 到 Env
    erails_var:set_session_id(SessionKey, Env).


