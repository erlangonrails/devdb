-module(xmppclient_recommend).
-include("xmppclient.hrl").
-include_lib("exmpp.hrl").
-include_lib("exmpp_client.hrl").

-export([login/2, login/5,
	 logout/0,
	 get_recommend/1, get_recommend/2]).
-export([init/5, loop/2]).

login(Username, Password) ->
    login(Username, Password, ?VHOST, ?SERVER, ?PORT).

login(Username, Password, VHost, Server, Port) ->
    register(?MODULE,  spawn(?MODULE, init, 
			     [Username, Password, VHost, Server, Port])).
logout() ->
    ?MODULE ! stop.

get_recommend(User) ->
    get_recommend(User, ?VHOST).
get_recommend(User, VHost) ->
    Jid = User ++ "@" ++ VHost,
    ?MODULE ! {get_recommend, {Jid,VHost}}.

init(Username, Password, VHost, Server, Port) ->
    application:start(exmpp),
    MySession = xmppclient_util:login(Username, Password, VHost, Server, Port),
    loop(MySession, 0).

loop(MySession, Count) ->
    receive
	{get_recommend, {Jid, VHost}} ->
	    get_recommend_in(MySession, Jid, VHost),
	    loop(MySession, Count);
        stop ->
            exmpp_session:stop(MySession);
        Record ->
	    Data = exmpp_xml:document_to_iolist(Record#received_packet.raw_packet),
            ?PRINT("(#~p)recvived xml#~s~n~n", [Count, Data]),
            loop(MySession, Count + 1)
    end.


%% Internal APIs:
get_recommend_in(MySession, Jid, VHost) ->
    Data = io_lib:format(
           "<iq type='get' to='recommend.~s' from='~s'>" ++
	     "<query xmlns='youbao:xmpp:recommend'/>" ++
	   "</iq>", [VHost, Jid]),
    xmppclient_util:send_string(MySession, Data).

    
