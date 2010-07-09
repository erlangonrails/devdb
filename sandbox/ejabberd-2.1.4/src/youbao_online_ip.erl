-module(youbao_online_ip).
-include("ejabberd.hrl").
-export([get_online_ip/2, set_online_ip/4]).

%% #jid.lserver, #jid.luser
get_online_ip(Server, User) ->
    case odbc_queries:get_online_ip(Server, User) of
	{selected,["ip"],[]} ->
	    [];
	{selected,["ip"],[{Ip}]} ->
	    Ip;
	BadVal ->
	    ?ERROR_MSG("#youbao# query return bad value:~p", [BadVal]),
	    []
    end.

%% 同时更新Ip & State
%% State = login | logout
set_online_ip(Server, User, Ip, State) ->
    StateVal = case State of
	           login ->
        	       "1";
		   logout ->
		       "0";
		   _ ->
		       "0"
               end,
    case odbc_queries:get_online_ip(Server, User) of
	{selected,["ip"],[]} ->
	    odbc_queries:add_online_ip(Server, User, Ip, StateVal);
	{selected,["ip"],[{OldIp}]} ->
	    odbc_queries:set_online_ip_t(Server, User, Ip, StateVal);
	BadVal ->
	    ?ERROR_MSG("#youbao# query return bad value:~p", [BadVal]),
            error
    end.
