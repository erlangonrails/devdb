-module(sysmon_acl).
-export([check_acl/2, is_login/1, do_logout/1, do_login/3]).

%% @doc
%% 检测是否是授权用户
-spec check_acl(Username :: string(), Password :: string()) ->
		       true | false.
check_acl(Username, Password) ->
    Acl = sysmon:get_config(acl, []),
    lists:member({Username, Password}, Acl).


%% @doc
%% 用户登录, 如果登录成功, 会为这个session增加一条{login_ok, Username}
%% 记录.
-spec do_login(Username :: string(), Password :: string(), Env :: any()) ->
		      true | false.
do_login(Username, Password, Env) ->
    case check_acl(Username, Password) of
	false ->
	    false;
	true ->
	    erails_var:set_session_data("login_ok", Username, Env),
	    true
    end.

%% @doc
%% 清理session中的{login_ok, Username}信息.
do_logout(Env) ->
    erails_var:remove_session_data("login_ok", Env).

%% @doc
%% 判断用户是否登录
-spec is_login(Env :: any()) ->
		      false | true.
is_login(Env) ->
    case erails_var:get_session_data("login_ok", Env) of
        undefined ->
	    false;
	_A ->
	    true
    end.
    
    
