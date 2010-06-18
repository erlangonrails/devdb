%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the node state supervisor
-module(e2d_node_sup).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("e2d.hrl").

-behaviour(gen_event).

-export([start_link/0]).
-export([nodeup/1, nodedown/1]).
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {}).
-define(SERVER, ?MODULE).
-define(MonitorOpt, [{node_type, visible}, nodedown_reason]).

start_link() ->
    ?Debug("~p start_link ~n", [?MODULE]),
    case gen_event:start_link({local, ?SERVER}) of
        {ok, Pid} ->
            gen_event:add_handler(?SERVER, ?MODULE, []),
            {ok, Pid};
        Other ->
            Other
    end.

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

%% gen_event callbacks
init([]) ->
    net_kernel:monitor_nodes(true, ?MonitorOpt),
    {ok, #state{}}.

handle_event({nodeup, Nodes}, State) ->
    %e2d_membership:add(
    lists:foreach(fun(Node) -> e2d_membership:update_node({add, Node}) end, Nodes),
    {ok, State};
handle_event({nodedown, Nodes}, State) ->
    lists:foreach(fun(Node) -> e2d_membership:update_node({remove, Node}) end, Nodes),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

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

terminate(_Reason, _State) ->
    net_kernel:monitor_nodes(false, ?MonitorOpt),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
