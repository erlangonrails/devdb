-module(xmppclient_subscribe).
-include("xmppclient.hrl").
-include_lib("exmpp.hrl").
-include_lib("exmpp_client.hrl").

-export([login/2, login/5,
	 logout/0,
	 send_subscribe/1, send_subscribe/2,
	 process_subscribe/2, process_subscribe/3]).
-export([init/5, loop/2]).

login(Username, Password) ->
    login(Username, Password, ?VHOST, ?SERVER, ?PORT).

login(Username, Password, VHost, Server, Port) ->
    register(?MODULE,  spawn(?MODULE, init, 
			     [Username, Password, VHost, Server, Port])).
logout() ->
    ?MODULE ! stop.

%% 发送出席信息的订阅(包含了附加消息ackmessage).
send_subscribe(User) ->
    send_subscribe(User, ?VHOST).
send_subscribe(User, VHost) ->
    Jid = User ++ "@" ++ VHost,
    ?MODULE ! {send_subscribe, Jid}.

%% 处理对方发送来的出席信息订阅请求, 也就是是否同意对方的出席信息订阅.
%% Res = true | false (同意 | 拒绝)
process_subscribe(User, Res) ->
    process_subscribe(User, ?VHOST, Res).
process_subscribe(User, VHost, Res) ->
    Jid = User ++ "@" ++ VHost,
    ?MODULE ! {process_subscribe, Jid, Res}.

init(Username, Password, VHost, Server, Port) ->
    application:start(exmpp),
    MySession = xmppclient_util:login(Username, Password, VHost, Server, Port),
    loop(MySession, 0).

loop(MySession, Count) ->
    receive
        {send_subscribe, Jid} ->
	    send_subscribe_in(MySession, Jid),
	    loop(MySession, Count);
	{process_subscribe, Jid, Res} ->
	    process_subscribe_in(MySession, Jid, Res),
	    loop(MySession, Count);
        stop ->
            exmpp_session:stop(MySession);
        Record ->
	    Data = exmpp_xml:document_to_iolist(Record#received_packet.raw_packet),
            ?PRINT("(#~p)recvived xml#~s~n~n", [Count, Data]),
            loop(MySession, Count + 1)
    end.


%% Internal APIs:
%% 注意:
%% 添加好友的附加消息<status>xxxx</status>, 这个附加消息会存储在ackmessage字段
send_subscribe_in(MySession, Jid) ->
    Data = io_lib:format(
           "<presence type='subscribe' to ='~s'>" ++
	     "<status>this is ackmessage with the subscribe</status>"++ 
	   "</presence>", [Jid]),
    xmppclient_util:send_string(MySession, Data).
process_subscribe_in(MySession, Jid, Res) ->
    ResData = case Res of
		  true ->
		      "subscribed";
		  false ->
		      "unsubscribed"
              end,
    Data = io_lib:format(
           "<presence type='~s' to ='~s'>" ++
	     "<status>this is response message with the process subscribe</status>"++ 
	   "</presence>", [ResData, Jid]),
    xmppclient_util:send_string(MySession, Data).

    
