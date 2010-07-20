%%%-------------------------------------------------------------------
%%% File    : dispatchserver.erl
%%% Author  : liqiang 
%%% Description : 
%%%
%%% Created :  2 Jun 2010 by liqiang 
%%%-------------------------------------------------------------------
-module(dispatchserver).

-behaviour(application).

-export([start/0, stop/0]).

%% Application callbacks
-export([start/2, stop/1]).
%% Supervisor callbacks
-export([init/1]).

start() ->
    tcp_server:start(),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE),
    tcp_server:stop().

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
    supervisor:start_link({local, dispatchserver_sup}, ?MODULE, []).

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.


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
init([]) ->
    AChild = {'dispatchserver_route', {'dispatchserver_route', start_link,[]},
	      permanent,2000,worker,['dispatchserver_route']},
    {ok,{{one_for_all,10,10}, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
