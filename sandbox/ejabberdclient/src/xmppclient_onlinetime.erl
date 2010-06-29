-module(xmppclient_onlinetime).
-include("xmppclient.hrl").
-include_lib("exmpp.hrl").
-include_lib("exmpp_client.hrl").

-export([login/2, login/5,
	 logout/0,
	 get_onlinetime/2, get_onlinetime/3]).
-export([init/5, loop/2]).

login(Username, Password) ->
    login(Username, Password, ?VHOST, ?SERVER, ?PORT).

login(Username, Password, VHost, Server, Port) ->
    register(?MODULE,  spawn(?MODULE, init, 
			     [Username, Password, VHost, Server, Port])).
logout() ->
    ?MODULE ! stop.

get_onlinetime(FromUser, ToUser) ->
    get_onlinetime(FromUser, ToUser,  ?VHOST).
get_onlinetime(FromUser, ToUser, VHost) ->
    FromJid = FromUser ++ "@" ++ VHost,
    ToJid = ToUser ++ "@" ++ VHost,
    ?MODULE ! {get_onlinetime, {FromJid, ToJid}}.

init(Username, Password, VHost, Server, Port) ->
    application:start(exmpp),
    MySession = xmppclient_util:login(Username, Password, VHost, Server, Port),
    loop(MySession, 0).

loop(MySession, Count) ->
    receive
	{get_onlinetime, {FromJid, ToJid}} ->
	    get_onlinetime_in(MySession, FromJid, ToJid),
	    loop(MySession, Count);
        stop ->
            exmpp_session:stop(MySession);
        Record ->
	    Data = exmpp_xml:document_to_iolist(Record#received_packet.raw_packet),
            ?PRINT("(#~p)recvived xml#~s~n~n", [Count, Data]),
            loop(MySession, Count + 1)
    end.


%% Internal APIs:
get_onlinetime_in(MySession, FromJid, ToJid) ->
    Data = io_lib:format(
           "<iq type='get' to='~s' from='~s'>" ++
	     "<query xmlns='youbao:xmpp:onlinetime'/>" ++
	   "</iq>", [ToJid, FromJid]),
    xmppclient_util:send_string(MySession, Data).

    
