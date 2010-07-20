%%%-------------------------------------------------------------------
%%% File    : tcp_client_sup.erl
%%% Author  : liqiang
%%% Description : 一个纯粹的client监控进程, 用来监控simple_one_for_one模式的
%%%               Client子进程.
%%%
%%% Created : 27 May 2010 by liqiang 
%%%-------------------------------------------------------------------
-module(tcp_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% 模块说明:
%% 该模块用来监控simple_one_for_one的Client进程.
%% start_local(Name, Callback)用于启动该进程.
%% 注意: 
%% Callback = {M, F, A}是Client子进程的启动方式.
%%
%% 如何动态的添加Client子进程?
%% supervisor:start_child(Name, List), 
%% 这会以apply(M, F, A ++ List)的方式启动Client子进程.
%%
%%====================================================================

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(SupName, Callback) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%
%% SupName = {local,Name} | {global,Name}
%% Callback = {M, F, A}, Client进程的启动方式
%%--------------------------------------------------------------------
start_link(Callback) ->
    supervisor:start_link(?MODULE, Callback).

start_link(SupName, Callback) ->
    supervisor:start_link(SupName, ?MODULE, Callback).

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
init({M, F, A}) ->
    AChild = {undefined, {M, F, A},
	      temporary, brutal_kill, worker, [M]},
    {ok,{{simple_one_for_one, 10, 10}, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
