-module(xmppclient_announce).
-include("xmppclient.hrl").
-include_lib("exmpp.hrl").
-include_lib("exmpp_client.hrl").

-export([login/2, login/5,
	 logout/0,
	 send_announce_all/1, send_announce_online/1,
	 send_announce_all/2, send_announce_online/2]).
-export([init/5, loop/2]).

login(Username, Password) ->
    login(Username, Password, ?VHOST, ?SERVER, ?PORT).

login(Username, Password, VHost, Server, Port) ->
    register(?MODULE,  spawn(?MODULE, init, 
			     [Username, Password, VHost, Server, Port])).
logout() ->
    ?MODULE ! stop.

send_announce_all(Message) ->
    send_announce_all(Message, ?VHOST).
send_announce_all(Message, VHost) ->
    ?MODULE ! {send_announce_all, {Message, VHost}}.

send_announce_online(Message) ->
    send_announce_online(Message, ?VHOST).
send_announce_online(Message, VHost) ->
    ?MODULE ! {send_announce_online, {Message, VHost}}.

init(Username, Password, VHost, Server, Port) ->
    application:start(exmpp),
    MySession = xmppclient_util:login(Username, Password, VHost, Server, Port),
    loop(MySession, 0).

loop(MySession, Count) ->
    receive
        {send_announce_all, {Message, VHost}} ->
	    send_announce_all(MySession, Message, VHost),
	    loop(MySession, Count);
        {send_announce_online, {Message, VHost}} ->
	    send_announce_online(MySession, Message, VHost),
	    loop(MySession, Count);
        stop ->
            exmpp_session:stop(MySession);
        Record ->
	    Data = exmpp_xml:document_to_iolist(Record#received_packet.raw_packet),
            ?PRINT("(#~p)recvived xml#~s~n~n", [Count, Data]),
            loop(MySession, Count + 1)
    end.


%% Internal APIs:
send_announce_all(MySession, Message, VHost) ->
    Data = io_lib:format(
           "<message to ='~s/announce/all' type='chat' xml:lang='en'>" ++
             "<guoguo_system>~s</guoguo_system>" ++
	   "</message>", [VHost, Message]),
    xmppclient_util:send_string(MySession, Data).

send_announce_online(MySession, Message, VHost) ->
    Data = io_lib:format(
           "<message to ='~s/announce/online' type='chat' xml:lang='en'>" ++
             "<guoguo_system>~s</guoguo_system>" ++ 
	   "</message>", [VHost, Message]),
    xmppclient_util:send_string(MySession, Data).

    
