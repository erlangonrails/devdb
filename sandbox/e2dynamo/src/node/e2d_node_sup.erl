%%%-------------------------------------------------------------------
%%% File    : e2d_node_sup.erl
%%% Author  : liqiang 
%%% Description : 
%%%
%%% Created : 24 Jun 2010 by liqiang 
%%%-------------------------------------------------------------------
-module(e2d_node_sup).
-include("e2d.hrl").
-behaviour(gen_event).
%% API
-export([start_link/0]).
-export([nodeup/1, nodedown/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-record(state, {}).
-define(SERVER, ?MODULE).
-define(MonitorOpt, [{node_type, visible}, nodedown_reason]).

%% @doc node login
-spec nodeup(Nodes :: [atom()]) -> 'ok'.
nodeup(Nodes) ->
    ?Debug("~p triggle nodeup event : ~p~n", [?MODULE, Nodes]),
    gen_event:notify(?SERVER, {nodeup, Nodes}).

%% @doc node failure
-spec nodedown(Nodes :: [atom()]) -> 'ok'.
nodedown(Nodes) ->
    ?Debug("~p triggle nodedown event : ~p~n", [?MODULE, Nodes]),
    gen_event:notify(?SERVER, {nodedown, Nodes}).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | {error,Error} 
%% Description: Creates an event manager.
%%--------------------------------------------------------------------
start_link() ->
    ?Debug("~p start_link ~n", [?MODULE]),
    case gen_event:start_link({local, ?SERVER}) of
        {ok, Pid} ->
            gen_event:add_handler(?SERVER, ?MODULE, []), %% callback Module:init([])
            {ok, Pid};
        Other ->
            Other
    end.


%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init([]) ->
    net_kernel:monitor_nodes(true, ?MonitorOpt),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function:  
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event. 
%%--------------------------------------------------------------------
handle_event({nodeup, Nodes}, State) ->
    lists:foreach(fun(Node) -> e2d_membership:update_node({add, Node}) end, Nodes),
    {ok, State};
handle_event({nodedown, Nodes}, State) ->
    lists:foreach(fun(Node) -> e2d_membership:update_node({remove, Node}) end, Nodes),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1, 
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% Description: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event 
%% handler to handle the request.
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% Description: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%%--------------------------------------------------------------------
handle_info({nodeup, Node, Info}, State) ->
    Nodes = e2d_config:get(nodes),
    case lists:member(Node, Nodes) of
        false ->
            ?Error("this node is not in the node list~n", []);
        true ->
            ?Debug("receive nodeup: ~p~n", [Node]),
            nodeup([Node])
    end,
    {ok, State};
handle_info({nodedown, Node, Info}, State) ->
    Nodes = e2d_config:get(nodes),
    case lists:member(Node, Nodes) of
        false ->
            ?Error("this node is not in the node list~n", []);
        true ->
            ?Debug("receive nodedown: ~p~n", [Node]),
            nodedown([Node])
    end,
    {ok, State};
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description:Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and 
%% do any necessary cleaning up. 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState} 
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
