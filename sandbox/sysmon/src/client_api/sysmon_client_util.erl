-module(sysmon_client_util).
-include("sysmon_client_api_config.hrl").
-export([list_to_integer/1, list_to_integer/2, 
	 list_to_float/1, list_to_float/2,
	 get_pid/1, get_cpu/1, get_mem/1]).


%%
%% 这个模块于获取进程pid相关的部分通过hard-code实现的.
%%


list_to_integer(Num) ->
    ?MODULE:list_to_integer(Num, undefined).
list_to_integer(Num, Default) ->
    case catch erlang:list_to_integer(Num) of
	{'EXIT', _} ->
	    Default;
	NumInt ->
	    NumInt
    end.

list_to_float(Num) ->
    ?MODULE:list_to_float(Num, undefined).
list_to_float(Num, Default) ->
    case catch erlang:list_to_float(Num) of
	{'EXIT', _} ->
	    Default;
	NumFloat ->
	    NumFloat
    end.
 
%%
%% @doc
%% 获取一个系统进程的pid.
%% 参数支持内置的atom: 'redis' | 'mysql' | 'dispatchserver' | 'ejabberd'
%% 或者string()
%%  
%% e.g.
%% pgrep -f 'ejabberd' ----- 查找ejabberd的pid.
%%
-spec get_pid(Pattern :: string() | atom()) -> integer() | undefined.
get_pid(?REDIS_APP) ->
    get_pid(?REDIS_PATTERN);
get_pid(?MYSQL_APP) ->
    get_pid(?MYSQL_PATTERN);
get_pid(?DISPATCHSERVER_APP) ->
    get_pid(?DISPATCHSERVER_PATTERN);
get_pid(?EJABBERD_APP) ->
    get_pid(?EJABBERD_PATTERN);
get_pid(Pattern) when is_list(Pattern)->
    Cmd = io_lib:format("pgrep -f '~s'", [Pattern]),
    case os:cmd(Cmd) of
	[] ->
	    undefined;
	Pid ->
	    %% E.g. Pid = "2713\n"
	    Pid1 = string:strip(Pid, right, $\n),
	    ?MODULE:list_to_integer(Pid1, undefined)
    end.

%%
%% @doc
%% 获取一个系统进程占用的内存(KB)
%%
%% e.g.
%% ps -o rss= -p $PID
-spec get_mem( Arg :: integer() | string() | atom()) -> integer() | undefined.
get_mem(Pid) when is_integer(Pid) ->
    Cmd = io_lib:format("ps -o rss= -p ~p", [Pid]),
    Mem = os:cmd(Cmd),
    %% E.g. Mem = " 56341\n"
    Mem1 = string:strip(Mem, both, $\n),
    Mem2 = string:strip(Mem1, both, 32), %% remove the whitespace
    ?MODULE:list_to_integer(Mem2, undefined);
get_mem(PatternOrAtom) ->
    Pid = ?MODULE:get_pid(PatternOrAtom),
    if
	is_integer(Pid) ->
	    get_mem(Pid);
	true ->
	    undefined
    end.

%%
%% @doc
%% 获取一个系统进程占用的CPU百分比
%% 
%% e.g.
%% ps -o pcpu= -p $PID
-spec get_cpu(Arg :: integer() | string() | atom()) -> float() | undefined.
get_cpu(Pid) when is_integer(Pid) ->
    Cmd = io_lib:format("ps -o pcpu= -p ~p", [Pid]),
    CPU = os:cmd(Cmd),
    %% E.g. CPU = " 4.5\n".
    CPU1 = string:strip(CPU, both, $\n),
    CPU2 = string:strip(CPU1, both, 32), %% remove the whitespace
    ?MODULE:list_to_float(CPU2, undefined);
get_cpu(PatternOrAtom) ->
    Pid = get_pid(PatternOrAtom),
    if
	is_integer(Pid) ->
	    get_cpu(Pid);
	true ->
	    undefined
    end.



