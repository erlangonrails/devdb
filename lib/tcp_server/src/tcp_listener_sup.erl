%%%-------------------------------------------------------------------
%%% File    : tcp_listener_sup.erl
%%% Author  : liqiang 
%%% Description : 
%%%
%%% Created : 27 May 2010 by liqiang 
%%%-------------------------------------------------------------------
-module(tcp_listener_sup).

-behaviour(supervisor).

%% API
-export([start_link/6, start_link/7]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% 模块说明:
%% 是一个监控进程, 用来监控下面的两个进程: tcp_listener和tcp_acceptor_sup
%% (tcp_listener和tcp_acceptor_sup是一对一的关系)
%%
%%           tcp_listener_sup
%%            /             \
%%  tcp_listener         tcp_acceptor_sup
%%                       /       |      \
%%         tcp_acceptor1  tcp_acceptor2  tcp_acceptor3
%% 
%% 备注:
%% 每个acceptor接收到一个TCP链接后直接调用AcceptCallback, 
%% apply(M, F, A ++ [Sock])后继续accept.
%%====================================================================

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%
%% OnStartup = {M, F, A}
%% OnShutdown = {M, F, A}
%% AcceptCallback = {M, F, A}
%%--------------------------------------------------------------------
start_link(IPAddress, Port, SocketOpts, 
           OnStartup, OnShutdown, AcceptCallback) ->
    start_link(IPAddress, Port, SocketOpts, 
               OnStartup, OnShutdown, AcceptCallback, 1).

start_link(IPAddress, Port, SocketOpts, 
           OnStartup, OnShutdown, AcceptCallback, ConcurrentAcceptorCount) ->
    supervisor:start_link(?MODULE, {IPAddress, Port, SocketOpts, 
                                    OnStartup, OnShutdown, AcceptCallback, ConcurrentAcceptorCount}).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init({IPAddress, Port, SocketOpts, OnStartup, OnShutdown, AcceptCallback, ConcurrentAcceptorCount}) ->
    AcceptorSupName = tcp_misc:tcp_name(tcp_acceptor_sup, IPAddress, Port),

    AcceptorSup = {tcp_acceptor_sup, {tcp_acceptor_sup, start_link,
                                     [AcceptorSupName, AcceptCallback]},
                   transient, infinity, supervisor, [tcp_acceptor_sup]},

    Listener = {tcp_listener, {tcp_listener, start_link,
                              [IPAddress, Port, SocketOpts,
                               ConcurrentAcceptorCount, AcceptorSupName,
                               OnStartup, OnShutdown]},
	       transient, 100, worker, [tcp_listener]},

    {ok,{{one_for_all, 10, 10}, [AcceptorSup, Listener]}}.

%%====================================================================
%% Internal functions
%%====================================================================
