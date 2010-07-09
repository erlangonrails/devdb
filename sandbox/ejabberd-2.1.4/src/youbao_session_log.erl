-module(youbao_session_log).
-include("ejabberd.hrl").
-export([set_session_log/4]).


set_session_log(Server, User, Ip, Logintime) ->
    Sid = gen_sid(Logintime),
    Logouttime = calendar:local_time(),
    odbc_queries:set_session_log_t(Server, Sid, User, Ip, 
					  fmt_time(Logintime), fmt_time(Logouttime)).


%% Internal APIs:
fmt_time({{Y, M, D}, {Hour, Min, Sec}}) ->
    io_lib:format("~p-~p-~p ~p:~p:~p", [Y, M, D, Hour, Min, Sec]);
fmt_time(_) ->
    "bad-format-time".

gen_sid({{Y, M, D}, {Hour, Min, Sec}}) ->
    integer_to_list(Y) ++ integer_to_list(M) ++ integer_to_list(D) ++ 
    integer_to_list(Hour) ++ integer_to_list(Min) ++ integer_to_list(Sec) ++ randomstr(8);
gen_sid(_) ->
    gen_sid(calendar:local_time()).
	
%% @spec randomstr(Size::integer()) ->string()	

randomstr(Len) ->
    Chars =
	{$a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t,$u,$v,
	 $w,$x,$y,$z,$1,$2,$3,$4,$5,$6,$7,$8,$9,$0},
    Res = lists:map(
	    fun(_) ->
		    Idx = crypto:rand_uniform(1, 36),
		    element(Idx, Chars)
	    end, lists:seq(1, Len)),
    Res.
