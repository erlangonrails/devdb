%%%-------------------------------------------------------------------
%%% File    : tcp_acceptor_sup.erl
%%% Author  : liqiang
%%% Description : 一个纯粹的监控进程, 用来监控simple_one_for_one的tcp_acceptor
%%                子进程.
%%%
%%% Created : 27 May 2010 by liqiang 
%%%-------------------------------------------------------------------
-module(tcp_acceptor_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% 模块说明:
%% 负责监控simple_one_for_one模式的tcp_acceptor子进程.
%%
%% 该模块只有一个APIs:
%% start_link(Name, Callback)来启动该模块.
%%
%% 如何动态添加子进程?
%% supervisor:start_child(Name, [ListenSock]), 
%% 这会以apply(M, F, A ++ List)的方式启动子进程, 也就是调用
%% tcp_acceptor:start_link(Callback, ListenSock)来启动子进程
%% 
%%====================================================================

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(Name, Callback) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Name, Callback) ->
    supervisor:start_link({local, Name}, ?MODULE, Callback).

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
init(Callback) ->
    AChild = {tcp_acceptor, {tcp_acceptor, start_link, [Callback]},
	      transient, brutal_kill, worker, [tcp_acceptor]},
    {ok,{{simple_one_for_one, 10, 10}, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
