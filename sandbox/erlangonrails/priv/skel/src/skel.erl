%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(skel).
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
%% @doc Start the skel server.
start() ->
    application:start(sasl),
    setup_log(), %% manual setup logger
    %% application:start(mnesia),
    skel_deps:ensure(),
    ensure_started(crypto),
    application:start(skel).

%% @spec stop() -> ok
%% @doc Stop the skel server.
stop() ->
    Res = application:stop(skel),
    application:stop(crypto),
    %% application:stop(mnesia),
    application:stop(sasl),
    Res.


%% get the configuration from the cfg file.
get_config(Key, Default) ->
    case file:consult(skel_ctl:get_config_file()) of
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
    ?SETUP_LOG(skel_ctl:get_log_file()),
    ?SET_LOG_LEVEL(get_config(loglevel, 5)).
