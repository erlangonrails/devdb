-module(sysmon_util).
-export([log_data/3, consult/1]).

-define(DEFAULT_DATADIR, "/tmp").

log_data(Filename, Fmt, Args) ->
    FilenameFull = filename:join([sysmon:get_config(datadir,?DEFAULT_DATADIR), Filename]),
    case file:open(FilenameFull, [append]) of
	{ok, FileHandle} ->
	    io:format(FileHandle, Fmt, Args),
	    file:close(FileHandle),
	    ok;
	{error, Reason} ->
	    {error, Reason}
    end.

-spec consult(Filename :: string()) -> {ok, [any()]} | {error, any()}.
consult(Filename) ->
    FilenameFull = filename:join([sysmon:get_config(datadir,?DEFAULT_DATADIR), Filename]),
    file:consult(Filename).


%% ===================================
%% 处理monitor_app_task产生的data文件
%% ===================================

