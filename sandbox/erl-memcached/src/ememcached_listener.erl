-module(ememcached_listener).

%% 一个客户端Socket的接收器, 就是简单的创建(gen_tcp:listen/2)一个ListenSocket来侦听客户端
%% 的连接, 如果有新的连接到来, 简单的调用onecached_server:start(Socket)处理后, 继续接收
%% (gen_tcp:accept(ListenSocket))客户端的连接. 这和ejabberd中接收客户端连接的模块的设计结构
%% 是一样的.

-include("ememcached.hrl").
-include("erl_logger.hrl").

-define(DEFAULT_PORT, 11211).

-export([start_link/0, init/0]).


start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, init, [])}.




init() ->
    register(?MODULE, self()),
    Port = ememcached:get_config(port, ?DEFAULT_PORT),
    case gen_tcp:listen(Port, [list,
			       {packet, raw},
			       {active, false},
			       {reuseaddr, true},
			       {nodelay, true},
			       {keepalive, true}]) of
	{ok, LSock} ->
	    ?DEBUG("listen ~p", [LSock]),
	    accept(LSock);
	{error, Reason} ->
	    ?CRITICAL_MSG("Failed to open socket on port ~p: ~p", [{Port}, Reason])
    end.

accept(ListenSocket) ->
    ?DEBUG("accept ~p", [ListenSocket]),
    case gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
	    ?DEBUG("accepting ~p", [Socket]),
	    Pid = ememcached_server:start(Socket),
	    case ok of
		ok ->
		    ?DEBUG("Controlling process ~p", [Pid]),
		    ok;
		{error, Reason} ->
		    ?ERROR_MSG("(~w) Failed Controlling Process ~p: ~w", [Socket, Pid, Reason]),
		    gen_tcp:close(Socket)
	    end;
	{error, Reason} ->
	    ?ERROR_MSG("(~w) Failed TCP accept: ~w", [ListenSocket, Reason])
    end,
    ?DEBUG("end accept~n", []),
    accept(ListenSocket).
