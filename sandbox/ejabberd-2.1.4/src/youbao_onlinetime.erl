-module(youbao_onlinetime).
-include("ejabberd.hrl").
-export([get_onlinetime/2, get_onlinetime_el/2, set_onlinetime/3, get_logintime/2]).

-define(YB_ONLINETIME, "YB_ONLINETIME").

%% #jid.lserver, #jid.luser
get_onlinetime(Server, User) ->
    Delta = case get_logintime(Server, User) of
	        undefined ->
	            0;
	        LoginTime ->
	            OldSec = calendar:datetime_to_gregorian_seconds(LoginTime),
                    NowSec = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
                    (NowSec - OldSec) div 60 %% 本次登录的在线时间(分钟)
            end,
    Delta1 = (mod_ping:get_onlinetime(User) div 60) + Delta, %% 多倍积分的时间
    case odbc_queries:get_onlinetime(Server, User) of
	{selected,["onlinetime"],[]} ->
	    odbc_queries:add_onlinetime(Server, User, "0"),
	    integer_to_list(Delta1);
	{selected,["onlinetime"],[{Time}]} ->
	    NewOnlinetime = list_to_integer(Time) + Delta1,
            integer_to_list(NewOnlinetime);
	BadVal ->
	    ?ERROR_MSG("#youbao# query return bad value:~p", [BadVal]),
	    integer_to_list(Delta1)
    end.

get_onlinetime_el(Server, User) ->
    YB_onlinetime = get_onlinetime(Server, User),
    [{xmlelement, ?YB_ONLINETIME, [], [{xmlcdata, YB_onlinetime}]}].

set_onlinetime(Server, User, LoginTime) ->
    OldSec = calendar:datetime_to_gregorian_seconds(LoginTime),
    NowSec = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    AddMin = (NowSec - OldSec) div 60, %% 本次登录的在线时间(分钟)
    AddMin1 = AddMin + (mod_ping:get_onlinetime(User) div 60), %% 多倍积分的时间
    mod_ping:reset_onlinetime(User), %% 时间设置之后, 需要清空:
    case odbc_queries:get_onlinetime(Server, User) of
	{selected, ["onlinetime"], []} ->
	    odbc_queries:add_onlinetime(Server, User, integer_to_list(AddMin1));
	{selected, ["onlinetime"], [{Time}]} ->
	    NewOnlinetime = list_to_integer(Time) + AddMin1,
	    odbc_queries:set_onlinetime_t(Server, User, integer_to_list(NewOnlinetime));
	BadVal ->
	    ?ERROR_MSG("#youbao# query return bad value:~p", [BadVal]), 
	    error
    end.


get_logintime(Server, User) ->
    case ejabberd_sm:get_session_pid(User, Server) of
        none ->
	    undefined;
        C2SPid ->
            ejabberd_c2s:get_logintime(C2SPid)
    end.
