-module(announce_controller,[Env]).
-include("erl_logger.hrl").

-export([handle_request/2,before_filter/0]).

handle_request("index",[]) ->
    {render,"announce/index.html",[{data, "announce"}]};
handle_request("service",[Type]) ->
    Usr = erails_var:get_param("announce_usr", Env),
    Pwd = erails_var:get_param("announce_pwd", Env),
    MessageTitle = erails_var:get_param("message_title", Env),
    MessageLink = erails_var:get_param("message_link", Env),
    Message = erails_var:get_param("message", Env),
  
    case check_usr_pwd(Usr, Pwd) of
        false ->
	    {render,"announce/index.html",[{data, "announce"}, {result, "announce用户名&密码错误"}]};
        true ->
            case Message =:= "" of
	        true ->
	            {render,"announce/index.html",[{data, "announce"}, {result, "广播消息不能为空"}]};
	        false ->
                    case send_announce_message(Type, MessageTitle, MessageLink, Message) of
                        true ->
                            {render,"announce/index.html",[{data, "announce"}, {result, "发送完成"}]};
	                false ->
                            {render,"announce/index.html",[{data, "announce"}, {result, "发送过程中发生错误, 请确保格式正确"}]}
                    end
            end
    end.


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

%% Internal APIs:
check_usr_pwd(Usr, Pwd) ->
    case Usr =:= sysmon:get_config(ejabberd_announce_user, "announce") of
        true ->
            case Pwd =:= sysmon:get_config(ejabberd_announce_user_password, "user@123") of
                true -> true;
                false -> false
            end;
        false -> false
    end.

%% 为什么需要休眠5秒呢?
%%
%% 如果不进行休眠, 从ejabberd的日志判断, 存在两个问题:
%% a. logout操作没起作用, 日志里没有用户登出的记录, 这样用户会一直存在在在线用户索引列表中不能删除.
%% b. 用户登录后服务器会发送<message>消息给announce用户, sleep一段时间可以接收这个消息.
%%
%% 人工检测?
%% select * from spool where username = 'announce', 如果发现存在这样的消息, 需要手动删除,
%% delete from spool where username = 'announce', 否则这个消息会在announce登录后发送给announce,
%% 我们增加了休眠逻辑后, 会自动接收并处理掉这个消息.
send_announce_message("all", MessageTitle, MessageLink, Message) ->
    xmppclient_announce:login(),
    timer:sleep(1000*5),
    %% 处理格式错误导致的异常, 仍人需要保证announce正确logout:)
    try xmppclient_announce:send_announce_all(MessageTitle, MessageLink, Message) of
        _ ->
            timer:sleep(1000*5),
            xmppclient_announce:logout(),
	    true
    catch
	TypeErr:TypeErrVal ->
	    ?WARNING_MSG("send_announce_message exception: ~p, ~p, ~s, ~s, ~s", 
                         [TypeErr, TypeErrVal, MessageTitle, MessageLink, Message]), 
            timer:sleep(1000*5),
	    xmppclient_announce:logout(),
	    false
    end;
send_announce_message("online", MessageTitle, MessageLink, Message) ->
    xmppclient_announce:login(),
    timer:sleep(1000*5),
    %% 处理格式错误导致的异常, 仍人需要保证announce正确logout:)
    try xmppclient_announce:send_announce_online(MessageTitle, MessageLink, Message) of
        _ ->
            timer:sleep(1000*5),
            xmppclient_announce:logout(),
	    true
    catch
	TypeErr:TypeErrVal ->
	    ?WARNING_MSG("send_announce_message exception: ~p, ~p, ~s, ~s, ~s", 
                         [TypeErr, TypeErrVal, MessageTitle, MessageLink, Message]), 
            timer:sleep(1000*5),
	    xmppclient_announce:logout(),
	    false
    end;
send_announce_message(_, _MessageTitle, _MessageLink, _Message) ->
    ignore.
