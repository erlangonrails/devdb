%%%-------------------------------------------------------------------
%%% File    : e2d_node.erl
%%% Author  : liqiang
%%% Description : 
%%%
%%% Created : 23 Jun 2010 by liqiang 
%%%-------------------------------------------------------------------
-module(e2d_node).
-include("e2d.hrl").
-behaviour(gen_server).

%%% @doc the module manager the local node
%%%   node state:
%%%   join : connecting other nodes
%%%   sync : sync bucket data with other nodes
%%%   work : the node is in working state, the normal state
%%%   exit : the node is about to exit the system

%% API
-export([start_link/1]).
-export([id/0, self/0, status/0, runtime/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% the state in e2d_node
-record(state, {
                node = #e2dnode{} :: e2dnode(), % the node info
                status = init :: node_status(), % the node's status
                logintime = now()               % the node login time
                }).

-define(SERVER, ?MODULE).

-spec id() -> node_id().
id() ->
    gen_server:call(?SERVER, {id}).

-spec self() -> e2dnode().
self() ->
    gen_server:call(?SERVER, {self}).

-spec status() -> node_status().
status() ->
    gen_server:call(?SERVER, {status}).

%%@doc return running time(unit second)
-spec runtime() -> pos_integer().
runtime() ->
    gen_server:call(?SERVER, {runtime}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(#e2dnode{} = Node) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Node, []).

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
init(#e2dnode{} = Node) ->
        State = #state{node = Node},
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({id}, _From, State) ->
    Node = State#state.node,
    {reply, Node#e2dnode.name, State};
handle_call({self}, _From, State) ->
    {reply, State#state.node, State};
handle_call({status}, _From, State) ->
    {reply, State#state.status, State};
handle_call({runtime}, _From, State) ->
    Time = timer:now_diff(now(), State#state.logintime),
    {reply, Time div 1000000, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

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
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
