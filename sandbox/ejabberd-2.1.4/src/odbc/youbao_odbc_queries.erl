-module(youbao_odbc_queries).
-export([get_onlinetime/2,  %% youbao_onlinetime
	 add_onlinetime/3,
	 del_onlinetime/2,
	 set_onlinetime_t/3,

	 get_online_ip/2,   %% youbao_online_ip
	 add_online_ip/4,
	 del_online_ip/2,
	 set_online_ip_t/4,

         get_session_log/2, %% youbao_session_log
	 add_session_log/6,
	 del_session_log/2,
	 set_session_log_t/6]).

%% Erlang ODBC返回值格式(not transaction):
%% 1. {updated, integer()}
%% 2. {selected, [string()], [tuple()]}
%% 3. {error, term()}


%% Almost a copy of string:join/2.
%% We use this version because string:join/2 is relatively
%% new function (introduced in R12B-0).
join([], _Sep) ->
    [];
join([H|T], Sep) ->
    [H, [[Sep, X] || X <- T]].

%% Safe atomic update.
update_t(Table, Fields, Vals, Where) ->
    UPairs = lists:zipwith(fun(A, B) -> A ++ "='" ++ B ++ "'" end,
			   Fields, Vals),
    case ejabberd_odbc:sql_query_t(
	   ["update ", Table, " set ",
	    join(UPairs, ", "),
	    " where ", Where, ";"]) of
	{updated, 1} ->
	    ok;
	_ ->
	    ejabberd_odbc:sql_query_t(
	      ["insert into ", Table, "(", join(Fields, ", "),
	       ") values ('", join(Vals, "', '"), "');"])
    end.

get_online_ip(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["select ip from youbao_online_ip "
       "where username='", Username, "';"]).

add_online_ip(LServer, Username, Ip, State) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["insert into youbao_online_ip(username, ip, state) "
       "values ('", Username, "', '", Ip, "', '", State, "');"]).

del_online_ip(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["delete from youbao_online_ip where username='", Username ,"';"]).

set_online_ip_t(LServer, Username, Ip, State) ->
    ejabberd_odbc:sql_transaction(
      LServer,
      fun() ->
	      update_t("youbao_online_ip", ["username", "ip", "state"],
		       [Username, Ip, State],
		       ["username='", Username ,"'"])
      end).

get_onlinetime(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["select onlinetime from youbao_onlinetime "
       "where username='", Username, "';"]).

add_onlinetime(LServer, Username, Onlinetime) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["insert into youbao_onlinetime(username, onlinetime) "
       "values ('", Username, "', '", Onlinetime, "');"]).

del_onlinetime(LServer, Username) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["delete from youbao_onlinetime where username='", Username ,"';"]).

set_onlinetime_t(LServer, Username, Onlinetime) ->
    ejabberd_odbc:sql_transaction(
      LServer,
      fun() ->
	      update_t("youbao_onlinetime", ["username", "onlinetime"],
		       [Username, Onlinetime],
		       ["username='", Username ,"'"])
      end).


get_session_log(LServer, Sid) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["select sid, username, ip, logintime, logouttime from youbao_session_log "
       "where sid='", Sid, "';"]).

add_session_log(LServer, Sid, Username, Ip, LoginTime, LogoutTime) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["insert into youbao_session_log(sid, username, ip, logintime, logouttime) "
       "values ('", Sid, "', '", Username, "', '", Ip, "','", LoginTime ,"','", LogoutTime, "');"]).

del_session_log(LServer, Sid) ->
    ejabberd_odbc:sql_query(
      LServer,
      ["delete from youbao_session_log where sid='", Sid ,"';"]).

set_session_log_t(LServer, Sid, Username, Ip, LoginTime, LogoutTime) ->
    ejabberd_odbc:sql_transaction(
      LServer,
      fun() ->
	      update_t("youbao_session_log", ["sid", "username", "ip", "logintime", "logouttime"],
		       [Sid, Username, Ip, LoginTime, LogoutTime],
		       ["sid='", Sid ,"'"])
      end).
