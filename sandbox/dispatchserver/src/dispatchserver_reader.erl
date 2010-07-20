%%%-------------------------------------------------------------------
%%% File    : dispatchserver_reader.erl
%%% Author  : liqiang
%%% Description : 
%%%
%%% Created : 28 May 2010 by liqiang 
%%%-------------------------------------------------------------------
-module(dispatchserver_reader).

-behaviour(gen_fsm).
-include("erl_logger.hrl").

%% API
-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% gen_fsm states
-export(['Socket'/2]).

-record(state, {socket, addr}). %% 客户端的socket       

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

'Socket'({socket_ready, Socket}, State) when is_port(Socket) ->
    {ok, {IP, Port}} = inet:peername(Socket),
    {SrvIP, SrvPort} = dispatchserver_route:get_xmpp_service(),
    Data = io_lib:format("~s:~p", [SrvIP, SrvPort]),
    ?DEBUG("send data ~p to client ~p:~p~n", [Data, IP, Port]),
    gen_tcp:send(Socket, Data),
    {stop, normal, State#state{socket = Socket, addr = {IP, Port}}}.

%%====================================================================
%% 模块说明:
%% 这是一个reader模块, 每个reader都是一个socket的control process.
%%
%% 这个模块有两个APIs, 都是在tcp_server组件中被自动调用的. 
%%====================================================================

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, 'Socket', #state{}}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(Event, StateName, State) ->
    {stop, {StateName, undefined_event, Event}, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, State) ->
    {stop, {StateName, undefined_event, Event}, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, _StateName, #state{socket=Socket, addr=Addr} = State) ->
    ?DEBUG("~p Client ~p send data: ~p~n", [Socket, Addr, Bin]),
    {stop, normal, State};
handle_info({tcp_closed, Socket}, _StateName, #state{socket=Socket, addr=Addr} = State) ->
    ?DEBUG("~p Client ~p disconnected. ~n", [Socket, Addr]),
    {stop, normal, State};
handle_info(_Info, StateName, State) ->
    {noreply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket=Socket}) ->
    catch gen_tcp:close(Socket),
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
