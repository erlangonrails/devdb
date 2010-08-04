-module(ememcached_server).

%% 负责于客户端交互, 接收客户端发来的数据, 解析memcached协议, 
%% 调用存储引擎读取或者存储数据, 发送处理结果给客户端.
%% 在实现上本质是一个gen_fsm, 有几个不同的状态, 我们会创建一个单独的进程来不断的调用gen_tcp:recv(Socket, 0)
%% 接收数据, 并解析, 每次解析出一行数据, 则发送给FSM进程处理, 并继续接收数据, 所以Socket进程和fsm进程是一对一的关系.
%%
%% (Socket进程  -> LINE MSG ->  FSM进程)
%% 
%% 1. 如何解析基于行数据的协议?
%%    memcached是基于行的协议, 命令行以\r\n结尾, 如何从数据流(list)中解析出一行数据, 实现如下:
%%    基于文本行的协议都可以用这种方式来解析出一行数据.
%%
%% read_line("abc") -> noline.
%% read_line("abc\r\ndddd") -> {line, "abc", "dddd"}.
%%
%% 实现:
%% read_line(Data) ->
%%     read_line(Data, "").
%% read_line("", _Line) ->
%%     noline;
%% read_line("\r\n" ++ Data, Line) ->
%%     {line, lists:reverse(Line), Data};
%% read_line([Char|Data], Line) ->
%%     read_line(Data, [Char | Line]).
%%
%% 2. memcached协议数据分为两类: 文本行数据和自由数据, 这两类数据都是用\r\n结尾的.
%%    我们如何从Socket中接收并解析出这些数据? 看下面的实现代码:
%%    不断的调用gen_tcp:recv/2接收数据, 每接收到一个数据包, 把上次解析剩下的数据和这次接收到的数据组合, 
%%    解析出其中的'行数据(可能是一行, 也可能是多行)', 解析完成后继续接收.
%%
%% receive TCP packets
%% 注意: 参数Data是上次process_packet解析完之后'剩下'的没有处理的数据, 继续处理.
%% loop(FSM_Pid, Socket, Data) ->
%%     case gen_tcp:recv(Socket, 0) of
%%         {ok, Packet} ->
%%	    %?DEBUG("Packet Received~n~p~n", [Packet]),
%%	    NewData = process_packet(FSM_Pid, Data++Packet), %% 处理: '上次解析剩下的数据' + '这次接收到的数据'
%%	    loop(FSM_Pid, Socket, NewData); %% 注意: NewData是这次解析剩下的数据, 在收到下个数据包后'组合'使用.
%%	{error, closed} ->
%%	    ?DEBUG("closed~n", []),
%%	    ok;
%%	{error, Reason} ->
%%	    ?ERROR_MSG("Error receiving on socket ~p: ~p~n", [Socket, Reason]),
%%	    {error, Reason}
%%    end.
%%
%% parse TCP packet to find lines, and send 'them' to the FSM
%% 把解析出来的'一行'或者'多行'数据发送给FSM, 返回剩下的数据.
%% process_packet(FSM_Pid, Data) ->
%%     case read_line(Data) of
%%         {line, Line, NewData} ->
%%	    ?DEBUG("Line~n~p", [Line]),
%%	    gen_fsm:send_event(FSM_Pid, {line, Line}), %% 把解析出来'一行数据'Line发送给有限状态机.
%%	    process_packet(FSM_Pid, NewData);          %% 继续处理'剩下'的数据NewData
%%	 noline ->
%%	    Data
%%    end.

%%
%% 3. 这个模块就是一个FSM, 有如下几个状态:
%%    a. process_command
%%    b. process_data_block
%%    c. discard_data_block
%%
%% 当初始化之后, 进入process_command状态, 等待解析一个'行命令', 当Socket收到的数据经过2中的逻辑解析之后, 发送给FSM的
%% process_command状态, 开始解析命令. 命令解析出来之后, 根据命令的状况：
%% 进入到process_data_block或者discard_data_block状态.
%% 例如:
%% 当解析出一个set key flags exptime bytes\r\n命令之后, 进入process_data_block状态等待接收数据, 当数据接收完成之后
%% (接收到的数据长度等于bytes的描述), 则调用存储引擎存储数据, 发送STORED\r\n给客户端. 完成之后进入process_command状态. 
%%
%% 当解析出一个add key flags exptime bytes\r\n命令之后, 我们需要先判断key对应的数据在存储引擎中是否存在, 如果存在, 则
%% 进入discard_data_block状态. 当数据接收完成之后(接收到的数据长度等于bytes的描述), 发送NOT_STORED\r\n给客户端, 再次进入
%% process_command状态; 令一个逻辑, 如果key对应的数据在存储引擎中不存在, 则进入和上面set命令一样的逻辑, 进入到process_data_block
%% 状态, 存储数据, 发送STORED\r\n给客户端. 完成之后进入process_command状态.
%%
%% 4. 支持的命令:
%% set, add, replace, get, incr, decr, delete, flush_all, quit
%% 
%% set 意思是 “储存此数据”
%% add 意思是 “储存此数据, 只在服务器*未*保留此键值的数据时”
%% replace意思是 “储存此数据, 只在服务器*曾*保留此键值的数据时”
%% get 获取操作
%% incr/decr 增加/减少操作
%% delete 删除操作
%% flush_all 删除所有数据
%% quit 退出操作
%%

-behaviour(gen_fsm).

-include("ememcached.hrl").
-include("erl_logger.hrl").

%% External exports
-export([start/1, stop/1]).
%% internal function
-export([loop/3]).

%% gen_fsm callbacks
-export([init/1,
	 process_command/2,
	 process_data_block/2,
	 discard_data_block/2,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 code_change/4]).

-record(state, {socket, storage, command}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Socket) ->
    gen_fsm:start(?MODULE, Socket, ?FSMOPTS).

stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

init(Socket) ->
    Storage = mnesia, % TODO -> get from parameters
    % run TCP server
    _Pid = proc_lib:spawn_link(?MODULE, loop, [self(), Socket, ""]),
    {ok, process_command, #state{socket=Socket, storage=Storage}}.

%%====================================================================
%% TCP server
%%====================================================================

% receive TCP packets
loop(FSM_Pid, Socket, Data) ->
    %?DEBUG("loop ~p~n", [Socket]),
    case gen_tcp:recv(Socket, 0) of
	{ok, Packet} ->
	    %?DEBUG("Packet Received~n~p~n", [Packet]),
	    NewData = process_packet(FSM_Pid, Data++Packet),
	    loop(FSM_Pid, Socket, NewData);
	{error, closed} ->
	    ?DEBUG("closed~n", []),

	    ok;
	{error, Reason} ->
	    ?ERROR_MSG("Error receiving on socket ~p: ~p~n", [Socket, Reason]),
	    {error, Reason}
    end.

% parse TCP packet to find lines, and send them to the FSM
process_packet(FSM_Pid, Data) ->
    case read_line(Data) of
	{line, Line, NewData} ->
	    ?DEBUG("Line~n~p", [Line]),
	    gen_fsm:send_event(FSM_Pid, {line, Line}),
	    process_packet(FSM_Pid, NewData);
	 noline ->
	    Data
    end.

% Try to find the first line in the Data.
% return {line, Line, Rest_of_date} if found or
% noline
-spec(read_line(list()) -> {line, list(), list()} | noline).
read_line(Data) ->
    read_line(Data, "").
read_line("", _Line) ->
    noline;
read_line("\r\n" ++ Data, Line) ->
    {line, lists:reverse(Line), Data};
read_line([Char|Data], Line) ->
    read_line(Data, [Char | Line]).

%%====================================================================
%% FSM Callbacks
%%====================================================================

% memcached "set" storage command line
process_command({line, "set "++Line}, StateData) ->
    {next_state, process_data_block, StateData#state{command=parse_storage_command(Line)}};
% memcached "add" storage command line
process_command({line, "add "++Line}, StateData) ->
    StorageCommand = parse_storage_command(Line),
    case StorageCommand of
	#storage_command{key=Key} ->
	    NewStorageCommand = StateData#state{command=StorageCommand},
	    case catch ememcached_storage:has_item(StateData#state.storage, Key) of
		false ->
		    {next_state, process_data_block, NewStorageCommand};
		_ ->
		    {next_state, discard_data_block, NewStorageCommand}
	    end;
	_ ->
	    ?ERROR_MSG("CLIENT_ERROR invalid command format~n~p~n", [Line]),
	    send_command(StateData#state.socket, "CLIENT_ERROR invalid command format "++Line),
	    {next_state, process_command, StateData}
    end;

% memcached "replace" storage command line
process_command({line, "replace "++Line}, StateData) ->
    StorageCommand = parse_storage_command(Line),
    case StorageCommand of
	#storage_command{key=Key} ->
	    NewStorageCommand = StateData#state{command=StorageCommand},
	    case catch ememcached_storage:has_item(StateData#state.storage, Key) of
		true ->
		    {next_state, process_data_block, NewStorageCommand};
		_ ->
		    {next_state, discard_data_block, NewStorageCommand}
	    end;
	_ ->
	    ?ERROR_MSG("CLIENT_ERROR invalid command format~n~p~n", [Line]),
	    send_command(StateData#state.socket, "CLIENT_ERROR invalid command format "++Line),
	    {next_state, process_command, StateData}
    end;

% memcached "get" retrieval command line
process_command({line, "get "++Line}, #state{socket=Socket, storage=Storage}=StateData) ->
    Keys = parse_retrieval_command(Line),
    lists:foreach(fun(Key) ->
			  send_item(Socket, Storage, Key)
		  end, Keys),
    send_command(Socket, "END"),
    {next_state, process_command, StateData};

% memcached "incr" command line
process_command({line, "incr "++Line}, StateData) ->
    process_incr_decr_command(fun(A, B) -> A+B end, Line, StateData);
process_command({line, "decr "++Line}, StateData) ->
    process_incr_decr_command(fun(A, B) -> A-B end, Line, StateData);

% memcached "delete" command line
% TODO second time argument support
process_command({line, "delete "++Line}, #state{socket=Socket, storage=Storage}=StateData) ->
    case parse_delete_command(Line) of
	{Key, _Time} ->
	    case catch ememcached_storage:delete_item(Storage, Key) of
		ok ->
		    send_command(Socket, "DELETED");
		none ->
		    send_command(Socket, "NOT_FOUND");
		Other ->
		    ?ERROR_MSG("SERVER_ERROR~n~p~n", [Other]),
		    send_command(Socket, io_lib:format("SERVER_ERROR ~p", [Other]))
	    end;
	_ ->
	    ?ERROR_MSG("CLIENT_ERROR invalid delete command format~n~p~n", [Line]),
	    send_command(Socket, "CLIENT_ERROR invalid delete command format: delete "++Line)
    end,
    {next_state, process_command, StateData};

% memcached "flush_all" command line
% TODO second time argument support
process_command({line, "flush_all"++_Line}, #state{socket=Socket, storage=Storage}=StateData) ->
    case catch ememcached_storage:flush_items(Storage) of
	ok ->
	    send_command(Socket, "OK");
	Other ->
	    ?ERROR_MSG("SERVER_ERROR~n~p~n", [Other]),
	    send_command(Socket, io_lib:format("SERVER_ERROR ~p", [Other]))
    end,
    {next_state, process_command, StateData};

% memcached "quit" command line
process_command({line, "quit"}, StateData) ->
    {stop, normal, StateData};

% unknown memcached command
process_command({line, Line}, #state{socket=Socket} = StateData) ->
    ?ERROR_MSG("CLIENT_ERROR unknown command~n~p~n", [Line]),
    send_command(Socket, "CLIENT_ERROR unknown command "++Line),
    {next_state, process_command, StateData}.

% process data block that won't be stored
discard_data_block({line, Line}, #state{socket=Socket,
					command=#storage_command{bytes=Bytes}} = StateData) ->
    case length(Line) of
	Bytes ->
	    send_command(Socket, "NOT_STORED"),
	    {next_state, process_command, StateData#state{command=undefined}};
	Length ->
	    % -2 because we count the discarded "\r\n" in the Data block
	    {next_state, discard_data_block, StateData#state{command=#storage_command{bytes=Bytes-Length-2}}}
    end;
discard_data_block({line, Line}, #state{socket=Socket} = StateData) ->
    ?ERROR_MSG("CLIENT_ERROR invalid command format~n~p~nState;~p~n", [Line, StateData]),
    send_command(Socket, "CLIENT_ERROR invalid command format "++Line),
    {next_state, process_command, StateData}.

% process data block that will be stored
process_data_block({line, Line}, #state{socket=Socket,
					storage=Storage,
					command=StorageCommand}=StateData)
  when is_record(StorageCommand, storage_command) ->
    Data = StorageCommand#storage_command.data,
    NewData = case Data of
		  "" ->
		      Line;
		  _ -> Data ++ "\r\n" ++ Line
	      end,
    NewStorageCommand = StorageCommand#storage_command{data=NewData},
    Bytes = StorageCommand#storage_command.bytes,
    case length(NewData) of
	Bytes ->
	    case catch ememcached_storage:store_item(Storage, NewStorageCommand) of
		ok ->
		    send_command(Socket, "STORED");
		Other ->
		    ?ERROR_MSG("SERVER_ERROR~n~p~n", [Other]),
		    send_command(Socket, "SERVER_ERROR "++Other)
	    end,
	    {next_state, process_command, StateData#state{command=undefined}};
	_Length ->
	    {next_state, process_data_block,
	     StateData#state{command=NewStorageCommand}}
    end;
process_data_block({line, Line}, #state{socket=Socket} = StateData) ->
    ?ERROR_MSG("CLIENT_ERROR invalid command format~n~p~n", [Line]),
    send_command(Socket, "CLIENT_ERROR invalid command format "++Line),
    {next_state, process_command, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData};
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, _StateName, #state{socket=Socket}=StateData) ->
    gen_tcp:close(Socket), %% 主动关闭Socket, 可以简介导致接收socket数据的进程loop/3退出, 
                           %% 从而使fsm进程和socket进程一并退出:) 
    ?DEBUG("StateData:~p, FSM stopped due to ~p", [StateData, Reason]),
    ok.


%%====================================================================
%% Communication functions
%%====================================================================

send_command(Socket, Command) ->
    gen_tcp:send(Socket, Command++"\r\n").

send_item(Socket, Storage, Key) ->
    case catch ememcached_storage:get_item(Storage, Key) of
	{ok, {Flags, Data}} ->
	    SData = case Data of
			Value when is_integer(Value) ->
			    integer_to_list(Value);
			Value when is_list(Value) ->
			    Value;
			Value when is_binary(Value) ->
			    binary_to_list(Value)
		    end,
	    send_command(Socket,
			 io_lib:format("VALUE ~s ~w ~w", [Key, Flags, length(SData)])),
	    send_command(Socket, SData);
	none ->
	    ok;
	Other ->
	    ?ERROR_MSG("SERVER_ERROR~n~p~n", [Other]),
	    send_command(Socket, "SERVER_ERROR "++Other)
    end.

%%====================================================================
%% Helper functions
%%====================================================================

% Format of Line is
% <key> <flags> <exptime> <bytes>
% return #storage_command or error
parse_storage_command(Line) ->
    case string:tokens(Line, " ") of
	[Key, SFlags, SExptime, SBytes] ->
	    case {string:to_integer(SFlags),
		  string:to_integer(SExptime),
		  string:to_integer(SBytes)} of
		{{Flags, ""}, {Exptime, ""}, {Bytes, ""}} ->
		    #storage_command{key = Key,
				     flags = Flags,
				     exptime = Exptime,
				     bytes = Bytes};
		_ ->
		    error
	    end;
	_ ->
	    error
    end.

% Format of Line is
% <key>*
% return [Key] when is_list(Key)
parse_retrieval_command(Line) ->
    string:tokens(Line, " ").

% Format of Line is
% <key> <time>?
% return {Key, Time}
parse_delete_command(Line) ->
    case string:tokens(Line, " ") of
	[Key, STime] ->
	    case string:to_integer(STime) of
		{Time, ""} ->
		    {Key, Time};
		_ ->
		    error
	    end;
	[Key] ->
	    {Key, 0};
	_ ->
	    error
    end.

% Format of Line is
% <key> <value>
% return {Key, Value} when is_integer(Value)
parse_incr_decr_command(Line) ->
    case string:tokens(Line, " ") of
	[Key, SValue] ->
	    case string:to_integer(SValue) of
		{Value, ""} ->
		    {Key, Value};
		_ ->
		    error
	    end;
	_ ->
	    error
    end.


process_incr_decr_command(Operation, Line, #state{socket=Socket, storage=Storage} = StateData) ->
    case parse_incr_decr_command(Line) of
	{Key, Value} ->
	    case catch ememcached_storage:update_item_value(Storage, Key, Value, Operation) of
		{ok, NewValue} ->
		    send_command(Socket, integer_to_list(NewValue));
		none ->
		    send_command(Socket, "NOT_FOUND");
		Other ->
		    ?ERROR_MSG("SERVER_ERROR~n~p~n", [Other]),
		    send_command(Socket, io_lib:format("SERVER_ERROR ~p", [Other]))
	    end;
	_ ->
	    ?ERROR_MSG("CLIENT_ERROR invalid incr/decr command format~n~p~n", [Line]),
	    send_command(Socket, "CLIENT_ERROR invalid incr/decr command format: "++Line)
    end,
    {next_state, process_command, StateData}.
