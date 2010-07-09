-module(youbao_onlinetime).
-include("ejabberd.hrl").
-export([get_onlinetime/2, get_onlinetime_el/2, set_onlinetime/3]).

-define(YB_ONLINETIME, "YB_ONLINETIME").

%% #jid.lserver, #jid.luser
get_onlinetime(Server, User) ->
    case youbao_odbc_queries:get_onlinetime(Server, User) of
	{selected,["onlinetime"],[]} ->
	    youbao_odbc_queries:add_onlinetime(Server, User, "0"),
	    "0";
	{selected,["onlinetime"],[{Time}]} ->
	    Time;
	BadVal ->
	    ?ERROR_MSG("#youbao# query return bad value:~p", [BadVal]),
	    "0"
    end.

get_onlinetime_el(Server, User) ->
    YB_onlinetime = get_onlinetime(Server, User),
    [{xmlelement, ?YB_ONLINETIME, [], [{xmlcdata, YB_onlinetime}]}].

set_onlinetime(Server, User, LoginTime) ->
    OldSec = calendar:datetime_to_gregorian_seconds(LoginTime),
    NowSec = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    AddMin = (NowSec - OldSec) div 60, %% 本次登录的在线时间(分钟)
    case youbao_odbc_queries:get_onlinetime(Server, User) of
	{selected, ["onlinetime"], []} ->
	    youbao_odbc_queries:add_onlinetime(Server, User, integer_to_list(AddMin));
	{selected, ["onlinetime"], [{Time}]} ->
	    NewOnlinetime = list_to_integer(Time) + AddMin,
	    youbao_odbc_queries:set_onlinetime_t(Server, User, integer_to_list(NewOnlinetime));
	BadVal ->
	    ?ERROR_MSG("#youbao# query return bad value:~p", [BadVal]), 
	    error
    end.
