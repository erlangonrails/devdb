%%%-------------------------------------------------------------------
%%% File    : tcp_listener.erl
%%% Author  : liqiang <liqiang@liqiang>
%%% Description : 创建一个Listen Socket, 并创建若干个accept模块接收客户端的链接.
%%%
%%% Created : 27 May 2010 by liqiang <liqiang@liqiang>
%%%-------------------------------------------------------------------
-module(tcp_listener).

-behaviour(gen_server).
-include("ds_log.hrl").

%% API
-export([start_link/7]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {sock, 
		on_startup,
		on_shutdown}).

%%====================================================================
%% 模块说明:
%% 该模块就是创建一个ListenSocket, 然后创建若干个tcp_acceptor进程来针对这个
%% ListenSocket接收客户端的连接.
%%
%% 补充:
%% 创建的tcp_acceptor进程都会挂在AcceptSup监控进程下, 所以该模块依赖tcp_acceptor_sup
%% 进程, 要求该进程必须启动起来.
%%
%%
%% 关于{keepalive, Boolean}
%% inet:setopts(Socket, [{keepalive, Boolean}]) -> ok | {error, Reason}
%% 官方文档:
%% (TCP/IP sockets) Enables periodic transmission(周期性的数据传输) on a 
%% connected socket, when no other data is being exchanged. If the other
%% end does not respond, the connection is considered broken and an error 
%% message will be sent to the controlling process. Default disabled.
%%====================================================================

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%
%% IP = ip_address()
%% Port = 0..65535
%% Opts:list()
%% ConcurrentAcceptorCount:integer()
%% AcceptorSup:atom(), the name of acceptor supervisor process
%% OnStartup = {M, F, A}, 在ListenSocket创建后调用apply(M, F, A ++ [IPAddress, Port])
%% OnShutdown = {M, F, A}, 在ListenSocket关闭(close)前调用apply(M, F, A ++ [IPAddress, Port])
%%--------------------------------------------------------------------
start_link(IPAddress, Port, Opts, 
	   ConcurrentAcceptorCount, AcceptorSup,
	   OnStartup, OnShutdown) ->
    gen_server:start_link(?MODULE, {IPAddress, Port, Opts, 
				    ConcurrentAcceptorCount, AcceptorSup,
				    OnStartup, OnShutdown}, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init({IPAddress, Port, Opts,
      ConcurrentAcceptorCount, AcceptorSup,
      {M, F, A} = OnStartup, OnShutdown}) ->
    process_flag(trap_exit, true),
    ListenOpts = [{ip, IPAddress},
		  {active, false},
		  {backlog, 30},
		  {keepalive, true},
		  {reuseaddr, true}],
    case gen_tcp:listen(Port, Opts ++ ListenOpts) of
	{ok, ListenSock} ->
	    lists:foreach(fun(_) ->
			      {ok, _AcceptorPid} = supervisor:start_child(AcceptorSup, [ListenSock])
			  end, lists:duplicate(ConcurrentAcceptorCount, dummy)),
	    ?INFO_MSG("started TCP listener on ~s:~p", [inet_parse:ntoa(IPAddress), Port]),
	    apply(M, F, A ++ [IPAddress, Port]),
	    {ok, #state{sock = ListenSock, on_startup = OnStartup, on_shutdown = OnShutdown}};
	{error, Reason} ->
	    ?ERROR_MSG("failed to start TCP listener on ~s:~p - ~p", [inet_parse:ntoa(IPAddress), Port, Reason]),
	    {stop, {cannot_listen, IPAddress, Port, Reason}}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{sock = ListenSock, on_shutdown = {M, F, A}}) ->
    {ok, {IPAddress, Port}} = inet:sockname(ListenSock),
    ?INFO_MSG("stopped TCP listener on ~s:~p", [inet_parse:ntoa(IPAddress), Port]),
    gen_tcp:close(ListenSock),
    apply(M, F, A ++ [IPAddress, Port]).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
