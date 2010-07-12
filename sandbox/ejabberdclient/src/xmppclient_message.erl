-module(xmppclient_message).
-include("xmppclient.hrl").
-include_lib("exmpp.hrl").
-include_lib("exmpp_client.hrl").

-export([login/2, login/5,
	 logout/0,
	 send_message/2, send_message/3]).
-export([init/5, loop/2]).

login(Username, Password) ->
    login(Username, Password, ?VHOST, ?SERVER, ?PORT).

login(Username, Password, VHost, Server, Port) ->
    register(?MODULE,  spawn(?MODULE, init, 
			     [Username, Password, VHost, Server, Port])).
logout() ->
    ?MODULE ! stop.


send_message(User, Message) ->
    send_message(User, ?VHOST, Message).
send_message(User, VHost, Message) ->
    Jid = User ++ "@" ++ VHost,
    ?MODULE ! {send_message, {Jid, Message}}.

init(Username, Password, VHost, Server, Port) ->
    application:start(exmpp),
    MySession = xmppclient_util:login(Username, Password, VHost, Server, Port),
    loop(MySession, 0).

loop(MySession, Count) ->
    receive
        {send_message, {Jid, Message}} ->
	    send_message_in(MySession, Jid, Message),
	    loop(MySession, Count);
        stop ->
            exmpp_session:stop(MySession);
        Record ->
	    Data = exmpp_xml:document_to_iolist(Record#received_packet.raw_packet),
            ?PRINT("(#~p)recvived xml#~s~n~n", [Count, Data]),
            loop(MySession, Count + 1)
    end.


%% Internal APIs:
send_message_in(MySession, Jid, Message) ->
    Data = io_lib:format(
           "<message to ='~s' type='chat' xml:lang='en'>" ++
	     "<body>~s</body>"++ 
	   "</message>", [Jid, Message]),
    xmppclient_util:send_string(MySession, Data).

    
