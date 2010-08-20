-module(xmppclient_announce).
-include("erl_logger.hrl").
-include_lib("exmpp.hrl").
-include_lib("exmpp_client.hrl").

-export([login/0, login/2, login/5,
	 logout/0,
	 send_announce_all/3, send_announce_online/3,
	 send_announce_all/4, send_announce_online/4]).
-export([init/5, loop/2]).


login() ->
    login(sysmon:get_config(ejabberd_announce_user, "announce"), 
	  sysmon:get_config(ejabberd_announce_user_password, "user@123") ,
	  sysmon:get_config(ejabberd_vhost, "91guoguo.com"), 
	  sysmon:get_config(ejabberd_ip, "127.0.0.1"), 
	  sysmon:get_config(ejabberd_port, 5222)).

login(Username, Password) ->
    login(Username, Password, 
	  sysmon:get_config(ejabberd_vhost, "91guoguo.com"), 
	  sysmon:get_config(ejabberd_ip, "127.0.0.1"), 
	  sysmon:get_config(ejabberd_port, 5222)).

login(Username, Password, VHost, Server, Port) ->
    register(?MODULE,  spawn(?MODULE, init, 
			     [Username, Password, VHost, Server, Port])).
logout() ->
    ?MODULE ! stop.

send_announce_all(MessageTitle, MessageLink, Message) ->
    send_announce_all(MessageTitle, MessageLink, Message, sysmon:get_config(ejabberd_vhost, "91guoguo.com")).
send_announce_all(MessageTitle, MessageLink, Message, VHost) ->
    ?MODULE ! {send_announce_all, {MessageTitle, MessageLink, Message, VHost}}.

send_announce_online(MessageTitle, MessageLink, Message) ->
    send_announce_online(MessageTitle, MessageLink, Message, sysmon:get_config(ejabberd_vhost, "91guoguo.com")).
send_announce_online(MessageTitle, MessageLink, Message, VHost) ->
    ?MODULE ! {send_announce_online, {MessageTitle, MessageLink, Message, VHost}}.

init(Username, Password, VHost, Server, Port) ->
    application:start(exmpp),
    MySession = xmppclient_util:login(Username, Password, VHost, Server, Port),
    loop(MySession, 0).

loop(MySession, Count) ->
    receive
        {send_announce_all, {MessageTitle, MessageLink, Message, VHost}} ->
	    send_announce_all(MySession, MessageTitle, MessageLink, Message, VHost),
	    loop(MySession, Count);
        {send_announce_online, {MessageTitle, MessageLink, Message, VHost}} ->
	    send_announce_online(MySession, MessageTitle, MessageLink, Message, VHost),
	    loop(MySession, Count);
        stop ->
            exmpp_session:stop(MySession);
        Record ->
	    Data = exmpp_xml:document_to_iolist(Record#received_packet.raw_packet),
            ?DEBUG("(#~p)(#~p)recvived xml#~s~n~n", [?MODULE, Count, Data]),
            loop(MySession, Count + 1)
    end.


%% Internal APIs:
send_announce_all(MySession, MessageTitle, MessageLink, Message, VHost) ->
    Data = io_lib:format(
           "<message to ='~s/announce/all' type='chat' xml:lang='en'>" ++
             "<guoguo_system_title>~s</guoguo_system_title>" ++
             "<guoguo_system_link>~s</guoguo_system_link>" ++
             "<guoguo_system>~s</guoguo_system>" ++
	   "</message>", [VHost, MessageTitle, MessageLink, Message]),
    xmppclient_util:send_string(MySession, Data).

send_announce_online(MySession, MessageTitle, MessageLink, Message, VHost) ->
    Data = io_lib:format(
           "<message to ='~s/announce/online' type='chat' xml:lang='en'>" ++
             "<guoguo_system_title>~s</guoguo_system_title>" ++
             "<guoguo_system_link>~s</guoguo_system_link>" ++
             "<guoguo_system>~s</guoguo_system>" ++ 
	   "</message>", [VHost, MessageTitle, MessageLink, Message]),
    xmppclient_util:send_string(MySession, Data).

    
