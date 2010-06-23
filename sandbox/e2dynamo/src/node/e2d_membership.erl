%%%-------------------------------------------------------------------
%%% File    : e2d_membership.erl
%%% Author  : liqiang <liqiang@liqiang>
%%% Description : 
%%%
%%% Created : 23 Jun 2010 by liqiang
%%%-------------------------------------------------------------------
-module(e2d_membership).
-include("e2d.hrl").
-behaviour(gen_server).

%%% @doc manager the membership with nodes,
%%%   we use the erlang build in distributed mechanism

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).
-export([update_node/1]).
-export([add/1, remove/1, update/2]).
-export([nodedown/1]).
-export([to_list/0]).
-export([successor/2, predecessor/2]).
-export([select_coordinator/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

-spec update_node({'remove'|'add', Node :: e2dnode()}) -> any().
update_node({remove, Node}) ->
    gen_server:cast(?SERVER, {remove_node, Node});
update_node({add, Node}) ->
    ?Debug("get node info~n", []),
    {ok, Info} = e2d_comm:request(Node, {node_info}),
    add(Info).


-spec add(Node :: e2dnode()) -> 'ok'.
add(Node) ->
    gen_server:cast(?SERVER, {add, Node}).

-spec remove(Id :: node_id()) -> 'ok'.
remove(Id) ->
    gen_server:cast(?SERVER, {remove, Id}).

-spec update(Id :: node_id(), Node :: e2dnode()) -> 'ok'.
update(Id, Node) ->
    gen_server:cast(?SERVER, {update, Id, Node}).

%% @doc some node down
nodedown(_Node) ->
    throw(not_impl).

-spec to_list() -> node_list().
to_list() ->
    gen_server:call(?SERVER, {to_list}).

%% @doc get the successor of the Id
-spec successor(Id :: node_id(), N :: pos_integer()) ->
    [atom()].
successor(Id, N) ->
    gen_server:call(?SERVER, {successor, Id, N}).

%% @doc get the predecessor of the Id
-spec predecessor(Id :: node_id(), N :: pos_integer()) ->
    [atom()].
predecessor(Id, N) ->
    gen_server:call(?SERVER, {predecessor, Id, N}).

%% @doc select coordinator
-spec select_coordinator(Key :: key()) ->
        {'ok', e2dnode(), [e2dnode()]} | none.
select_coordinator(Key) ->
    N = e2d_config:get(n),
    Nodes = successor(Key, N*2),
    {Prefs, Cands} =
    case catch lists:split(N, Nodes) of
        {'EXIT', {badarg, _}} -> % not enough Nodes : length(Nodes) < N
            {Nodes, []};
        {_, _} = Ret ->
            Ret
    end,

    case Prefs of
        [_ | _]  ->
            [Coor | Rest] =
            lists:sort(
                        fun(#e2dnode{capacity = C1}, #e2dnode{capacity = C2}) ->
                            C1 > C2
                        end,
                        Prefs),
            {ok, Coor, Rest ++ Cands};
        [] ->
            none
    end.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    ?Debug("~p init~n", [?MODULE]),
    process_flag(trap_exit, true),
    ets:new(node_list, [ordered_set, protected, named_table, {keypos, #e2dnode.id}]),
    Self = e2d_node:self(),
    ets:insert(node_list, Self),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({to_list}, _From, State) ->
    L = ets:tab2list(node_list),
    {reply, L, State};
handle_call({successor, Id, N}, _From, State) ->
    L = do_getnodes(Id, N, true),
    ?Debug("~p successor reply:~p~n", [?MODULE, L]),
    {reply, L, State};
handle_call({predecessor, Id, N}, _From, State) ->
    L = do_getnodes(Id, N, false),
    ?Debug("~p predecessor reply:~p~n", [?MODULE, L]),
    {reply, L, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add, Node}, State) ->
    ?Debug("~p add ~p~n", [?MODULE, Node]),
    ets:insert(node_list, Node),
    {noreply, State};
handle_cast({remove_node, Node}, State) ->
    ets:match_delete(node_list, #e2dnode{id='_',name=Node,ip='_',port='_',capacity='_'}),
    {noreply, State};
handle_cast({remove, Id}, State) ->
    ets:delete(node_list, Id),
    {noreply, State};
handle_cast({update, _Id, Node}, State) ->
    ?Debug("~p update ~p~n", [?MODULE, Node]),
    ets:insert(node_list, Node),
    {noreply, State};
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
is_table_empty(Tab) ->
    ets:info(Tab, size) =:= 0.

%% get the next key forward or backwrad
do_get_key_iter(Key, Forward) ->
    case Forward of
        true ->
            case ets:next(node_list, Key) of
                '$end_of_table' ->
                    ets:first(node_list);
                Key2 ->
                    Key2
            end;
        false ->
            case ets:prev(node_list, Key) of
                '$end_of_table' ->
                    ets:last(node_list);
                Key2 ->
                    Key2
            end
    end.

do_getnodes(Key, N, Forward) ->
    case is_table_empty(node_list) of
        true ->
            [];
        false ->
            FirstKey = do_get_key_iter(Key, Forward),
            [First] = ets:lookup(node_list, FirstKey),
            do_getnodes(Key, N-1, Forward, FirstKey, [First])
    end.

do_getnodes(_Key, 0, _Forward, _OrgK, Acc) ->
    lists:reverse(Acc);
do_getnodes(Key, N, Forward, OrgK, Acc) ->
    K = do_get_key_iter(Key, Forward),

    ?Debug("get nodes next key ~p N :~p OrgK:~p~n", [K, N, OrgK]),

    case K of
        OrgK ->
            lists:reverse(Acc); % loop to the orginal key, so just stop
        _ ->
            [Node] = ets:lookup(node_list, K),
            do_getnodes(K, N - 1, Forward, OrgK, [Node | Acc])
    end.

-ifdef(debug).
basic_test() ->
    {ok, _Pid} = start_link(),
    Id = 0,
    Node = #e2dnode{id = Id},
    Node2 = Node#e2dnode{port = 2334},
    add(Node),
    List1 = to_list(),
    update(Id, Node2),
    List2 = to_list(),
    remove(Id),
    List3 = to_list(),
    remove(Id),
    List4 = to_list(),
    Id3 = 234345,
    Node3 = #e2dnode{id = Id3, port = 2334},
    Id4 = 234346,
    Node4 = #e2dnode{id = Id4, port = 2334},
    add(Node2),
    add(Node3),
    add(Node4),
    Pre = predecessor(Id3, 3),
    %io:format(user, "predecessor ~p~n", [Pre]),
    ?assert([Node] == List1),
    ?assert([Node2] == List2),
    ?assert([] == List3),
    ?assert([] == List4),
    ?assert([Node4, Node2] == Pre).

-endif.
