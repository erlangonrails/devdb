%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(sysmon).
-author('author <author@example.com>').
-export([start/0, stop/0]).
-export([get_config/2]).
-include("erl_logger.hrl").

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the sysmon server.
start() ->
    application:start(sasl),
    setup_log(), %% manual setup logger
    mnesia_start(),
    sysmon_deps:ensure(),
    ensure_started(crypto),
    application:start(sysmon).

%% @spec stop() -> ok
%% @doc Stop the sysmon server.
stop() ->
    Res = application:stop(sysmon),
    application:stop(crypto),
    application:stop(mnesia),
    application:stop(sasl),
    Res.


%% get the configuration from the cfg file.
get_config(Key, Default) ->
    case file:consult(sysmon_ctl:get_config_file()) of
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

mnesia_start() ->
    case mnesia:system_info(extra_db_nodes) of
	[] ->
	    mnesia:create_schema([node()]);
	_ ->
	    ok
    end,
    application:start(mnesia),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity).

setup_log() ->
    ?SETUP_LOG(sysmon_ctl:get_log_file()),
    ?SET_LOG_LEVEL(get_config(loglevel, 5)).
