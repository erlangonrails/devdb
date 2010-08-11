-module(sysmon_util).
-export([log_data/3, consult/1, list_dir/0]).

-define(DEFAULT_DATADIR, "/tmp").

%% @doc
%% 写书聚到data目录
-spec log_data(Filename :: string(), Fmt :: string(), Args :: [any()]) -> ok | {error, any()}.
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

%% @doc
%% 从data目录读取数据
-spec consult(Filename :: string()) -> {ok, [any()]} | {error, any()}.
consult(Filename) ->
    FilenameFull = filename:join([sysmon:get_config(datadir,?DEFAULT_DATADIR), Filename]),
    file:consult(FilenameFull).


%% @doc
%% 列出data目录下的所有文件
-spec list_dir() -> {ok, string()} | {error, any()}.
list_dir() ->
    Dir = sysmon:get_config(datadir,?DEFAULT_DATADIR),
    file:list_dir(Dir).

