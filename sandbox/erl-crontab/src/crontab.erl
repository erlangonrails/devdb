-module(crontab).
-behaviour(application).
-include("erl_logger.hrl").

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0]).

start() ->
    setup_log(),
    application:start(sasl), 
    application:start(crontab).

start(normal, _StartArgs) ->
    case crontab_server:start_link() of
	{ok, Pid} -> 
	    {ok, Pid}
    end.

stop(_State) ->
    ok.


%% Internal APIs:
setup_log() ->
    ?SETUP_LOG(crontab_ctl:get_log_file()),
    ?SET_LOG_LEVEL(5). %% Debug Mode


