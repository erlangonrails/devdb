-module(mod_youbao_onlinetime).
-export([start/2, stop/1, process_iq/3]).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(NS_YOUBAO_ONLINETIME, "guoguo:xmpp:onlinetime").
-define(STANZA_ONLINETIME(Time),
	{xmlelement, "query",
	 [{"xmlns", ?NS_YOUBAO_ONLINETIME}],
	 [{xmlelement, "onlinetime", [], [{xmlcdata, Time}]}]}).

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_YOUBAO_ONLINETIME,
				  ?MODULE, process_iq, one_queue).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_YOUBAO_ONLINETIME).


process_iq(From, To, IQ) ->
    #iq{sub_el = SubEl} = IQ,
    #jid{lserver = LServer} = From,
    case lists:member(LServer, ?MYHOSTS) of
	true ->
	    process_local_iq(From, To, IQ);
	_ ->
	    IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
    end.

process_local_iq(_From, To, #iq{type = Type} = IQ) ->
    case Type of
	set ->
            #iq{sub_el = SubEl} = IQ,         
            IQ#iq{type = error, sub_el = [SubEl, ?STANZA_ERROR("404", "cancel", "can-not-set-onlinetime")]}; 
	get ->
	    Time = youbao_onlinetime:get_onlinetime(To#jid.lserver, To#jid.luser),
            IQ#iq{type = result, sub_el = [?STANZA_ONLINETIME(Time)]}
    end.
