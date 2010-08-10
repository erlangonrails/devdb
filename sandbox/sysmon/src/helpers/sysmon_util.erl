-module(sysmon_util).
-export([log_data/3]).

-define(DEFAULT_DATADIR, "/tmp").

log_data(Filename, Fmt, Args) ->
    FilenameFull = filename:join([sysmon:get_config(datadir,?DEFAULT_DATADIR), Filename]),
    case file:open(FilenameFull, write) of
	{ok, FileHandle} ->
	    io:format(FileHandle, Fmt, Args),
	    file:close(FileHandle),
	    ok;
	{error, Reason} ->
	    {error, Reason}
    end.
