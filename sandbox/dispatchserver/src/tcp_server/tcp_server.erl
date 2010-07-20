%%%-------------------------------------------------------------------
%%% File    : tcp_server.erl
%%% Author  : liqiang <liqiang@liqiang>
%%% Description : 
%%%
%%% Created : 27 May 2010 by liqiang <liqiang@liqiang>
%%%-------------------------------------------------------------------
-module(tcp_server).

-include("ds_log.hrl").

-behaviour(application).
-behaviour(supervisor).

%% APIs:
-export([start/0, stop/0,
	 tcp_listener_started/2, %% OnStartup callback
	 tcp_listener_stopped/2, %% OnShutdown callback
	 start_client/2]).       %% Accept callback

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

tcp_listener_started(_IPAddress, _Port) ->
    ok.
tcp_listener_stopped(_IPAddress, _Port) ->
    ok.

start_client(ClientModule, Sock) ->
    {ok, Child} = supervisor:start_child(tcp_client_sup, []), %% Note: Pid of the child process
    ok = gen_tcp:controlling_process(Sock, Child),
    ClientModule:set_socket(Child, Sock),
    Child.

%%====================================================================
%% tcp_server的网络结构:
%%                    tcp_server_sup
%%                   /               \
%%     tcp_client_sup                tcp_listener_sup
%%    /      |       \              /                \
%% reader1  reader2  reader3   tcp_acceptor_sup     tcp_listener
%%                            /        |       \
%%                 tcp_acceptor1  tcp_acceptor2 tcp_acceptor3
%%
%% a. 这个模块可以启动一个tcp_server.
%% b. reader是动态创建的(ds_reader), 当tcp_acceptor接收到一个新的客户端链接之后, 
%%    会调用start_client(ClientModule, Sock)来动态的创建一个reader进程来接收Sock的
%%    数据.
%% c. 一个tcp_listener_sup针对一个端口, 我们可以创建多个tcp_listener_sup来侦听不同的端口.
%%====================================================================

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
    supervisor:start_link({local, tcp_server_sup}, ?MODULE, []).

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
    ClientModule = ds_config:get_client_module(),
    SupFlags = {one_for_one, 5, 3600},
    ClientSup = childspec({client, ClientModule}),

    Listeners = ds_config:get_listeners(),
    ListenerSups = lists:map(fun({listener, Address, Port, ConcurrentAcceptorCount}) ->
			         childspec({listener, Address, Port, ClientModule, ConcurrentAcceptorCount})
			     end, Listeners),
    {ok, {SupFlags, [ClientSup] ++ ListenerSups}}.

%%====================================================================
%% Internal functions
%%====================================================================

%% 注意:
%% client(reader)模块的启动方式: ClientModule:start_link/0
childspec({client, Module}) ->
    {tcp_client_sup, {tcp_client_sup, start_link,
		      [{local, tcp_client_sup}, {Module, start_link, []}]},
     permanent, infinity, supervisor, [tcp_client_sup]};
childspec({listener, Host, Port, ClientModule, ConcurrentAcceptorCount}) ->
    IPAddress = case inet:getaddr(Host, inet) of
		    {ok, IPAddressTmp} ->
			IPAddressTmp;
		    {error, Reason} ->
			?ERROR_MSG("invalid host ~p - ~p", [Host, Reason]),
			throw({error, {invalid_host, Host, Reason}})
                end,
    if 
        is_integer(Port) andalso (Port >= 0) andalso (Port =< 65535) ->
	     ok;
        true ->
	    ?ERROR_MSG("invalid port ~p - not 0..65535", [Port]),
	     throw({error, {invalid_port, Port}})
    end,
    Name = tcp_misc:tcp_name(tcp_listener_sup, IPAddress, Port),
    {Name, {tcp_listener_sup, start_link, 
	    [IPAddress, Port, [binary, {packet, raw}, {exit_on_close, false}],
	     {?MODULE, tcp_listener_started, []},
	     {?MODULE, tcp_listener_stopped, []},
	     {?MODULE, start_client, [ClientModule]},
             ConcurrentAcceptorCount]},
    transient, infinity, supervisor, [tcp_listener_sup]}.
    
