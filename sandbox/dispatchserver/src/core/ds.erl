%%%-------------------------------------------------------------------
%%% File    : ds.erl
%%% Author  : liqiang 
%%% Description : 
%%%
%%% Created :  2 Jun 2010 by liqiang 
%%%-------------------------------------------------------------------
-module(ds).

-behaviour(application).

-export([start/0, stop/0, get_log_file/0, get_config_file/0]).

%% Application callbacks
-export([start/2, stop/1]).
%% Supervisor callbacks
-export([init/1]).

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

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
    supervisor:start_link({local, ds_sup}, ?MODULE, []).

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
    application:start(sasl),
    ds_config:start_link(),
    ds_loglevel:set(ds_config:get_log_level()),
    error_logger:add_report_handler(ds_logger_h, get_log_file()),
    application:start('tcp_server'),
    AChild = {'ds_route',{'ds_route',start_link,[]},
	      permanent,2000,worker,['ds_route']},
    {ok,{{one_for_all,10,10}, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================

get_config_file() ->
    case os:getenv("DISPATCHSERVER_CONFIG_PATH") of
	false ->
	    "dispatchserver.cfg";
	Path ->
	    Path
    end.

get_log_file() ->
    case os:getenv("DISPATCHSERVER_LOG_PATH") of
	false ->
	    "dispatchserver.log";
	Path ->
	    Path
    end.
