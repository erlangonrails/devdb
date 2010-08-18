-module(xmppclient_presence).
-include("xmppclient.hrl").
-include_lib("exmpp.hrl").
-include_lib("exmpp_client.hrl").

-export([login/2, login/5,
	 logout/0,
	 send_presence/1,
	 send_presence_channel/1]).
-export([init/5, loop/2]).

login(Username, Password) ->
    login(Username, Password, ?VHOST, ?SERVER, ?PORT).

login(Username, Password, VHost, Server, Port) ->
    register(?MODULE,  spawn(?MODULE, init, 
			     [Username, Password, VHost, Server, Port])).
logout() ->
    ?MODULE ! stop.

send_presence(Status) ->
    ?MODULE ! {send_presence, Status}.
send_presence_channel(Status) ->
    ?MODULE ! {send_presence_channel, Status}.

init(Username, Password, VHost, Server, Port) ->
    application:start(exmpp),
    MySession = xmppclient_util:login(Username, Password, VHost, Server, Port),
    loop(MySession, 0).

loop(MySession, Count) ->
    receive
	{send_presence_channel, Status} ->
	    send_presence_channel_in(MySession, Status),
	    loop(MySession, Count);
        {send_presence, Status} ->
	    send_presence_in(MySession, Status),
	    loop(MySession, Count);
        stop ->
            exmpp_session:stop(MySession);
        Record ->
	    Data = exmpp_xml:document_to_iolist(Record#received_packet.raw_packet),
            ?PRINT("(#~p)recvived xml#~s~n~n", [Count, Data]),
            loop(MySession, Count + 1)
    end.


%% Internal APIs:
send_presence_in(MySession, Status) ->
    Data = io_lib:format(
           "<presence>" ++
	     "<status>~s</status>"++ 
	   "</presence>", [Status]),
    xmppclient_util:send_string(MySession, Data).

send_presence_channel_in(MySession, Status) ->
    Data = io_lib:format(
           "<presence>" ++
	     "<status>" ++
               "<rtf>~s</rtf>" ++
               "<channel_ext>" ++
                 "<server_name>ServerName</server_name>" ++ 
                 "<server_id>ServerID</server_id>" ++ 
	         "<server_ip>ServerIP</server_ip>" ++
                 "<server_port>ServerPort</server_port>" ++
	         "<logo>Logo</logo>" ++
	         "<count>Count</count>" ++
	         "<capacity>Capacity</capacity>" ++
	         "<create_username>CreateUsername</create_username>" ++ 
	         "<nick_id>NickId</nick_id>" ++
	         "<isp>Isp</isp>" ++
	         "<room_type>RoomType</room_type>"
	         "<total_credit>TotalCredit</total_credit>"
               "</channel_ext>" ++
             "</status>"++ 
	   "</presence>", [Status]),
    xmppclient_util:send_string(MySession, Data).

    
