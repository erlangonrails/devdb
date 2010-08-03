-module(ememcached).
-include("erl_logger.hrl").

%% Application callbacks
-export([start/0, stop/0]).
-export([get_config/2]).

start() ->
    application:start(sasl),
    setup_log(), %% manual setup logger
    application:start(mnesia),
    ememcached_storage:init(mnesia),
    application:start(ememcached).

stop() ->
    application:stop(ememcached),
    application:stop(mnesia),
    application:stop(sasl).


%% get the configuration from the cfg file.
get_config(Key, Default) ->
    case file:consult(ememcached_ctl:get_config_file()) of
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

%%====================================================================
%% Internal functions
%%====================================================================
setup_log() ->
    ?SETUP_LOG(ememcached_ctl:get_log_file()),
    ?SET_LOG_LEVEL(get_config(loglevel, 5)).


