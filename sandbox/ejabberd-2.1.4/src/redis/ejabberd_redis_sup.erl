%%%-------------------------------------------------------------------
%%% File    : ejabberd_redis_sup.erl
%%% Author  : liqiang 
%%% Description : 
%%%
%%% Created : 28 Jun 2010 by liqiang 
%%%-------------------------------------------------------------------
-module(ejabberd_redis_sup).

-behaviour(supervisor).

%% API
-export([start/4]).
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start(Host, Port, Password, Count) ->
    start_link(Host, Port, Count),
    start_connect_pool(Host, Port, Password, Count).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Host, Port, Count) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Host, Port, Count]).

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
init([Host, Port, Count]) ->
    AChild = {'redis_client_sup',{'redis_conn_sup',start_link,[]},
	      permanent,1000,supervisor,['redis_client']},
    AChild1 = {'ejabberd_redis', {'ejabberd_redis', start_link, [Host, Port, Count]},
	      permanent, 1000, worker, ['ejabberd_redis']},
    {ok,{{one_for_one,10,10}, [AChild, AChild1]}}.

%% Internal APIs:
start_connect_pool(Host, Port, Password, Count) ->
    lists:foreach(fun(I) -> 
		      Name = redis_client:name(Host, Port, I),
		      {ok, _} = redis_conn_sup:connect(Host, Port, Password, Name)
                  end, lists:seq(1, Count)).





    
