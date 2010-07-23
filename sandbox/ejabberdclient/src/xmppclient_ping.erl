-module(xmppclient_ping).
-include("xmppclient.hrl").
-include_lib("exmpp.hrl").
-include_lib("exmpp_client.hrl").

-export([login/2, login/5,
	 logout/0,
	 send_ping/1, send_ping/2,
         send_ping_with_onlinetime/1, send_ping_with_onlinetime/2]).
-export([init/5, loop/2]).

login(Username, Password) ->
    login(Username, Password, ?VHOST, ?SERVER, ?PORT).

login(Username, Password, VHost, Server, Port) ->
    register(?MODULE,  spawn(?MODULE, init, 
			     [Username, Password, VHost, Server, Port])).
logout() ->
    ?MODULE ! stop.

send_ping(User) ->
    send_ping(User, ?VHOST).
send_ping(User, VHost) ->
    Jid = User ++ "@" ++ VHost,
    ?MODULE ! {send_ping, Jid}.

%% youbao的私有扩展:
%% 给ping包增加了<onlinetime>N</onlinetime>字段表示在线时间的倍数
send_ping_with_onlinetime(User) ->
    send_ping_with_onlinetime(User, ?VHOST).
send_ping_with_onlinetime(User, VHost) ->
    Jid = User ++ "@" ++ VHost,
    ?MODULE ! {send_ping_with_onlinetime, Jid}.

init(Username, Password, VHost, Server, Port) ->
    application:start(exmpp),
    MySession = xmppclient_util:login(Username, Password, VHost, Server, Port),
    loop(MySession, 0).

loop(MySession, Count) ->
    receive
        {send_ping, Jid} ->
	    send_ping_in(MySession, Jid),
	    loop(MySession, Count);
        {send_ping_with_onlinetime, Jid} ->
	    send_ping_with_onlinetime_in(MySession, Jid),
	    loop(MySession, Count);
        stop ->
            exmpp_session:stop(MySession);
        Record ->
	    Data = exmpp_xml:document_to_iolist(Record#received_packet.raw_packet),
            ?PRINT("(#~p)recvived xml#~s~n~n", [Count, Data]),
            loop(MySession, Count + 1)
    end.


%% Internal APIs:
send_ping_in(MySession, Jid) ->
    Data = io_lib:format(
           "<iq from='~s' to ='~s' type='get'>" ++
	     "<ping xmlns='urn:xmpp:ping'/>"++ 
	   "</iq>", [Jid, ?VHOST]),
    xmppclient_util:send_string(MySession, Data).

send_ping_with_onlinetime_in(MySession, Jid) ->
    Data = io_lib:format(
           "<iq from='~s' to ='~s' type='get'>" ++
	     "<ping xmlns='urn:xmpp:ping'>"++
               "<onlinetime>4</onlinetime>" 
             "</ping>" ++
	   "</iq>", [Jid, ?VHOST]),
    xmppclient_util:send_string(MySession, Data).

    
