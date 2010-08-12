-module(home_controller,[Env]).
-export([handle_request/2,before_filter/0]).

handle_request("index",[]) ->
    {render,"home/index.html",[{data, "home"}, 
	                       {services, get_service()}]}.

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


%% 为每一个service增加一个Tag{Key, Val} -> {Tag, Key, Val}
get_service() ->
    {_, Ret} = lists:foldl(fun({SrvKey, SrvTag}, {Tag, AccIn}) ->
                              {not Tag, [{not Tag, SrvKey, SrvTag}|AccIn]}
                          end, {true, []}, sysmon:get_config(services, [])),
    lists:reverse(Ret).
