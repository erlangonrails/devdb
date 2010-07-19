-module(erl_loglevel).

-export([set/1, get/0]).
-include("erl_logger.hrl").

-define(LOGMODULE, "error_logger").

%% Error levels:
-define(LOG_LEVELS,[ {0, no_log, "No log"}
                    ,{1, critical, "Critical"}
                    ,{2, error, "Error"}
                    ,{3, warning, "Warning"}
                    ,{4, info, "Info"}
                    ,{5, debug, "Debug"}
                    ]).

get() ->
    Level = erl_logger:get(),
    case lists:keysearch(Level, 1, ?LOG_LEVELS) of
        {value, Result} -> Result;
        _ -> erlang:error({no_such_loglevel, Level})
    end.


set(LogLevel) when is_atom(LogLevel) ->
    set(level_to_integer(LogLevel));
set(Loglevel) when is_integer(Loglevel) ->
    try
        {Mod,Code} = dynamic_compile:from_string(erl_logger_src(Loglevel)),
        code:load_binary(Mod, ?LOGMODULE ++ ".erl", Code)
    catch
        Type:Error -> ?CRITICAL_MSG("Error compiling logger (~p): ~p~n", [Type, Error])
    end;
set(_) ->
    exit("Loglevel must be an integer").

level_to_integer(Level) ->
    case lists:keysearch(Level, 2, ?LOG_LEVELS) of
        {value, {Int, Level, _Desc}} -> Int;
        _ -> erlang:error({no_such_loglevel, Level})
    end.

%% --------------------------------------------------------------
%% Code of the erl logger, dynamically compiled and loaded
%% This allows to dynamically change log level while keeping a
%% very efficient code.
erl_logger_src(Loglevel) ->
    L = integer_to_list(Loglevel),
    "-module(erl_logger).

    -export([debug_msg/4,
             info_msg/4,
             warning_msg/4,
             error_msg/4,
             critical_msg/4,
             get/0]).

   get() -> "++ L ++".

    %% Helper functions
    debug_msg(Module, Line, Format, Args) when " ++ L ++ " >= 5 ->
            notify(info_msg,
                   \"D(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    debug_msg(_,_,_,_) -> ok.

    info_msg(Module, Line, Format, Args) when " ++ L ++ " >= 4 ->
            notify(info_msg,
                   \"I(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    info_msg(_,_,_,_) -> ok.

    warning_msg(Module, Line, Format, Args) when " ++ L ++ " >= 3 ->
            notify(error,
                   \"W(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    warning_msg(_,_,_,_) -> ok.

    error_msg(Module, Line, Format, Args) when " ++ L ++ " >= 2 ->
            notify(error,
                   \"E(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    error_msg(_,_,_,_) -> ok.

    critical_msg(Module, Line, Format, Args) when " ++ L ++ " >= 1 ->
            notify(error,
                   \"C(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    critical_msg(_,_,_,_) -> ok.

    %% Distribute the message to the Erlang error logger
    notify(Type, Format, Args) ->
            LoggerMsg = {Type, group_leader(), {self(), Format, Args}},
            gen_event:notify(error_logger, LoggerMsg).
    ".
