-module(home_controller,[Env]).
-export([handle_request/2,before_filter/0]).

handle_request("index",[]) ->
    {render,"home/index.html",[{data, "home"}]}.

before_filter() ->
    FilterOnly = ["index"],
    case lists:member(erails_var:get_action(Env), FilterOnly) of
	true ->
	    case sysmon_acl:is_login(Env) of
		true ->
		    ok;
		false -> %% 用户没有登录, 重定向到登录页面 :)
		    {redirect, "/login"}
            end;
	false ->
	    {text, "no action!!!"}
    end.
