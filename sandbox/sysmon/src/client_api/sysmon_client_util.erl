-module(sysmon_client_util).
-include("sysmon_client_api_config.hrl").
-export([list_to_integer/1, list_to_integer/2, 
	 list_to_float/1, list_to_float/2,
	 get_pid/1, get_cpu/1, get_mem/1,
         kill_app/1, check_alive/1, start_app/1, start_app/2]).


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
%% 参数支持内置的atom: 'redis' | 'mysql' | 'dispatchserver' | 'ejabberd' | 'murmur'
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
get_pid(?MURMUR_APP) ->
    get_pid(?MURMUR_PATTERN);
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

%%
%% @doc
%% 杀掉一个进程
%%
%% e.g.
%% sudo kill $PID
%%
-spec kill_app(Arg :: integer() | string() | atom()) -> ok.
kill_app(Pid) when is_integer(Pid) ->
    Cmd = io_lib:format("sudo kill ~p", [Pid]),
    _Ret = os:cmd(Cmd), %% ignore the return value
    ok;
kill_app(PatternOrAtom) ->
    Pid = get_pid(PatternOrAtom),
    if
	is_integer(Pid) ->
	    kill_app(Pid);
	true ->
	    ok
    end.

%%
%% @doc
%% 检测application是否在运行
%% <1> 对于原生程序, 如mysql, redis, murmur等, 我们使用判断进程pid的方式进行判断
%% <2> 对于非原生程序, 如ejabberd, dispatchserver, 我们需要使用check command & check command ret来判断.
-spec check_alive(App :: atom()) -> boolean().
check_alive(?REDIS_APP) ->
    Pid = get_pid(?REDIS_APP),
    if
	is_integer(Pid) -> true;
	true -> false
    end;
check_alive(?MYSQL_APP) ->
    Pid = get_pid(?MYSQL_APP),
    if
	is_integer(Pid) -> true;
	true -> false
    end;
check_alive(?MURMUR_APP) ->
    Pid = get_pid(?MURMUR_APP),
    if
	is_integer(Pid) -> true;
	true -> false
    end;
%% 
%% Ret = os:cmd("/usr/local/dispatchserver/bin/dispatchserverctl status").
%% "Node dispatchserver@localhost is started. Status: started\ndispatchserver 0.1 is running in that node\ntcp_server 0.1 is running in that node\n"
%% Tokens = string:tokens(Ret, "\n").
%% ["Node dispatchserver@localhost is started. Status: started",
%%  "dispatchserver 0.1 is running in that node",       -------   ?DISPATCHSERVER_CHECK_CMD_RET
%%  "tcp_server 0.1 is running in that node"]  
check_alive(?DISPATCHSERVER_APP) ->
    Ret = os:cmd(?DISPATCHSERVER_CHECK_CMD), 
    Tokens = string:tokens(Ret, "\n"),
    lists:member(?DISPATCHSERVER_CHECK_CMD_RET, Tokens);
%% Ret = os:cmd("/usr/local/imserver/sbin/ejabberdctl status").          
%% "The node ejabberd@localhost is started with status: started\nejabberd 2.1.4 is running in that node\n"
%% Tokens = string:tokens(R1, "\n").
%% ["The node ejabberd@localhost is started with status: started",
%%  "ejabberd 2.1.4 is running in that node"]     -------   ?EJABBERD_CHECK_CMD_RET
check_alive(?EJABBERD_APP) ->
    Ret = os:cmd(?EJABBERD_CHECK_CMD), 
    Tokens = string:tokens(Ret, "\n"),
    lists:member(?EJABBERD_CHECK_CMD_RET, Tokens).

start_app(App) ->
    start_app(App, 1).

%% @doc
%% 启动一个进程, 可以做多次尝试, 如果尝试TryNum次后仍然失败, 则退出
-spec start_app(App :: atom(), TryNum :: integer()) -> boolean().
start_app(_App, 0) ->
    false;
start_app(App, TryNum) ->
    case check_alive(App) of
        true ->
            true;
        false ->
            kill_app(App),
            timer:sleep(5*1000), 
            start_app_internal(App),
            timer:sleep(5*1000), %% sleep 5 秒钟
            start_app(App, TryNum - 1)
    end.

-spec start_app(App :: atom()) -> ok.
start_app_internal(?REDIS_APP) ->
    _Ret = os:cmd(?REDIS_START_CMD),
    ok;
start_app_internal(?MYSQL_APP) ->
    _Ret = os:cmd(?MYSQL_START_CMD),
    ok;
start_app_internal(?DISPATCHSERVER_APP) ->
    _Ret = os:cmd(?DISPATCHSERVER_START_CMD),
    ok;
start_app_internal(?EJABBERD_APP) ->
    _Ret = os:cmd(?EJABBERD_START_CMD),
    ok;
start_app_internal(?MURMUR_APP) ->
    _Ret = os:cmd(?MURMUR_START_CMD),
    ok.




