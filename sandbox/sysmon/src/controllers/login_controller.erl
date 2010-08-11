-module(login_controller,[Env]).

-export([handle_request/2,before_filter/0]).

handle_request("index",[]) ->
    {render,"login/index.html",[{data, "login"}]};

%% 处理POST请求:)
handle_request("do", []) ->
    Usr = erails_var:get_param("usr", Env),
    Pwd = erails_var:get_param("pwd", Env),
    %% 登录验证:
    case sysmon_acl:do_login(Usr, Pwd, Env) of
	true ->
	    %% 登录成功后重定向到主页:)
	    {redirect, "/"};
	false ->
	    %% 登录失败, 重新登录:)
	    {redirect, "/login"}
    end.

before_filter() ->
    FilterOnly = ["index", "do"],
    case lists:member(erails_var:get_action(Env), FilterOnly) of
	true ->
	    ok;
	false ->
	    {text, "no action!!!"}
    end.
