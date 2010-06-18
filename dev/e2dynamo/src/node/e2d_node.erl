%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the module manager the local node
%%%   node state:
%%%   join : connecting other nodes
%%%   sync : sync bucket data with other nodes
%%%   work : the node is in working state, the normal state
%%%   exit : the node is about to exit the system
-module(e2d_node).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("e2d.hrl").

-behaviour(gen_server).

-export([start_link/1]).
-export([id/0, self/0, status/0, runtime/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).


%% the state in e2d_node
-record(state, {
                node = #e2dnode{} :: e2dnode(), % the node info
                status = init :: node_status(), % the node's status
                logintime = now()       % the node login time
                }).

-define(SERVER, ?MODULE).

-spec start_link(Node :: e2dnode()) -> {'ok', pid()} | 'ingore' | {'error', any()}.
start_link(#e2dnode{} = Node) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Node, []).

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

%% gen_server callbacks
init(#e2dnode{} = Node) ->
        State = #state{node = Node},
    {ok, State}.

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

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
