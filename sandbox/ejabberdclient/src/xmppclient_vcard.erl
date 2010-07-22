-module(xmppclient_vcard).
-include("xmppclient.hrl").
-include_lib("exmpp.hrl").
-include_lib("exmpp_client.hrl").

-export([login/2, login/5,
	 logout/0,
	 set_vcard/1, set_vcard/2,
         set_vcard_without_photo/1, set_vcard_without_photo/2,
	 get_vcard/1, get_vcard/2]).
-export([init/5, loop/2]).

login(Username, Password) ->
    login(Username, Password, ?VHOST, ?SERVER, ?PORT).

login(Username, Password, VHost, Server, Port) ->
    register(?MODULE,  spawn(?MODULE, init, 
			     [Username, Password, VHost, Server, Port])).
logout() ->
    ?MODULE ! stop.

%% 测试youbao的新特性:
%% 当更新VCard的时候, 如果头像的部分<PHOTO></PHOTO>为空
%% 则首先检查原有的VCard, 如果原有的VCard有头像, 则使用原来的头像.
set_vcard_without_photo(User) ->
    set_vcard_without_photo(User, ?VHOST).
set_vcard_without_photo(User, VHost) ->
    Jid = User ++ "@" ++ VHost,
    ?MODULE ! {set_vcard_without_photo, Jid}.

set_vcard(User) ->
    set_vcard(User, ?VHOST).
set_vcard(User, VHost) ->
    Jid = User ++ "@" ++ VHost,
    ?MODULE ! {set_vcard, Jid}.

get_vcard(User) ->
    get_vcard(User, ?VHOST).
get_vcard(User, VHost) ->
    Jid = User ++ "@" ++ VHost,
    ?MODULE ! {get_vcard, Jid}.

init(Username, Password, VHost, Server, Port) ->
    application:start(exmpp),
    MySession = xmppclient_util:login(Username, Password, VHost, Server, Port),
    loop(MySession, 0).

loop(MySession, Count) ->
    receive
        {set_vcard, Jid} ->
	    set_vcard_in(MySession, Jid),
	    loop(MySession, Count);
        {set_vcard_without_photo, Jid} ->
	    set_vcard_without_photo_in(MySession, Jid),
	    loop(MySession, Count);
	{get_vcard, Jid} ->
	    get_vcard_in(MySession, Jid),
	    loop(MySession, Count);
        stop ->
            exmpp_session:stop(MySession);
        Record ->
	    Data = exmpp_xml:document_to_iolist(Record#received_packet.raw_packet),
            ?PRINT("(#~p)recvived xml#~s~n~n", [Count, Data]),
            loop(MySession, Count + 1)
    end.


%% Internal APIs:
set_vcard_in(MySession, Jid) ->
    Data = io_lib:format(
           "<iq type='set' from='~s'>" ++
	     "<vCard xmlns='vcard-temp'>" ++
               "<BDAY>1476-06-09</BDAY>" ++
               "<ADR>" ++
                 "<CTRY>Italy</CTRY>" ++
                 "<LOCALITY>Verona</LOCALITY>" ++
                 "<HOME/>" ++
               "</ADR>" ++
               "<NICKNAME/>" ++
               "<N><GIVEN>Juliet</GIVEN><FAMILY>Capulet</FAMILY></N>" ++
               "<EMAIL>~s</EMAIL>" ++
               "<PHOTO>" ++
                 "<TYPE>image/jpeg</TYPE>" ++
                 "<BINVAL>" ++
                 "Base64-encoded-avatar-file-of-~s" ++
                 "</BINVAL>" ++
               "</PHOTO>" ++
	     "</vCard>" ++ 
	   "</iq>", [Jid, Jid, Jid]),
    xmppclient_util:send_string(MySession, Data).

set_vcard_without_photo_in(MySession, Jid) ->
    Data = io_lib:format(
           "<iq type='set' from='~s'>" ++
	     "<vCard xmlns='vcard-temp'>" ++
               "<BDAY>1476-06-09</BDAY>" ++
               "<ADR>" ++
                 "<CTRY>Italy</CTRY>" ++
                 "<LOCALITY>Verona</LOCALITY>" ++
                 "<HOME/>" ++
               "</ADR>" ++
               "<NICKNAME/>" ++
               "<N><GIVEN>Juliet</GIVEN><FAMILY>Capulet</FAMILY></N>" ++
               "<EMAIL>~s</EMAIL>" ++
               "<PHOTO/>" ++
	     "</vCard>" ++ 
	   "</iq>", [Jid, Jid]),
    xmppclient_util:send_string(MySession, Data).

get_vcard_in(MySession, Jid) ->
    Data = io_lib:format(
           "<iq type='get' to='~s'>" ++
	     "<vCard xmlns='vcard-temp'/>" ++
	   "</iq>", [Jid]),
    xmppclient_util:send_string(MySession, Data).

    
