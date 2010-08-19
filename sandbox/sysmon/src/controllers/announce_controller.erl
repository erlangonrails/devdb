-module(announce_controller,[Env]).

-export([handle_request/2,before_filter/0]).

handle_request("index",[]) ->
    {render,"announce/index.html",[{data, "announce"}]};
handle_request("service",[Type]) ->
    %% Message = erails_var:get_param("message", Env),
    Message = "liqianggaga",
    send_announce_message(Type, Message),
    {render,"announce/index.html",[{data, "announce"}]}.

before_filter() ->
    FilterOnly = ["index", "service"],
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


send_announce_message("all", Message) ->
    xmppclient_announce:login(),
    xmppclient_announce:send_announce_all(Message),
    xmppclient_announce:logout();
send_announce_message("online", Message) ->
    xmppclient_announce:login(),
    xmppclient_announce:send_announce_online(Message),
    xmppclient_announce:logout();
send_announce_message(_, _Message) ->
    ignore.
