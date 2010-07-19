-module(balance).

-behaviour(application).

-include("erl_logger.hrl").
-include("balance.hrl").

-export([start/0, get_config/2]).
%% application callbacks
-export([start/2, stop/1]).

start() ->
    setup_log(),
    application:start(sasl), 
    application:start(balance).

%%%----------------------------------------------------------------------
%%% Callback functions from application
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}   
%%----------------------------------------------------------------------
start(normal, _StartArgs) ->
    LocalTcpPort = get_config('local_tcp_port', 2525),
    BeConnTimeout = get_config('be_conn_timeout', 10*1000), %% 10 seconds
    BeInactivityTimeout = get_config('be_inactivity_timeout', 120*1000), %% 120 seconds
    bal_sup:start_link([LocalTcpPort, BeConnTimeout, BeInactivityTimeout]).    

stop(_State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
setup_log() ->
    ?SETUP_LOG(tcpbalance_ctl:get_log_file()),
    ?SET_LOG_LEVEL(get_config('loglevel', 5)). %% Default is debug mode

%% get the configuration from the cfg file.
get_config(Key, Default) ->
    case file:consult(tcpbalance_ctl:get_config_file()) of
	{ok, Options} ->
	    case proplists:lookup(Key, Options) of
		{Key, Value} ->
		     Value;
		_ ->
		    Default
            end;
	_ ->
	    Default
    end.
