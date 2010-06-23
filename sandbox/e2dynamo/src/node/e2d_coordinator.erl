%%% @doc the coordinator for the get/put/del request
-module(e2d_coordinator).
-include("e2d.hrl").

-export([route/2]).

-define(NODE_STORE_SERVER, e2d_store).

-spec route(Req :: request(), Nodes :: [e2dnode()]) ->
    {'ok', any()} | {'error', any()}.
route({get, Key}, Nodes) ->
    [N, R] = e2d_config:get([n, r]),
    do_route({get, Key}, Nodes, N, R);
route({put, _Key, _Context, Value}, _Nodes) when byte_size(Value) > ?MAX_VALUE_LEN ->
    {error, {bad_req, value_too_big}};
route({put, Key, Context, Value}, Nodes) ->
    [N, W] = e2d_config:get([n, w]),
    do_route({put, Key, Context, Value}, Nodes, N, W);
route({del, Key}, Nodes) ->
    [N, W] = e2d_config:get([n, w]),
    do_route({del, Key}, Nodes, N, W).

%%
%% internal API
%%
do_route(_Req, Nodes, _N, R) when length(Nodes) < R ->
    {error, {server_internal, not_enough_nodes}};
do_route(Req, Nodes, N, R) ->
    [Timeout] = e2d_config:get([api_timeout]),
    % start the timer
    TRef = erlang:start_timer(Timeout, self(), ok),
    Tag = make_ref(),

    ENs = erlang_nodes(Nodes),
    % spawn a new process do the route action
    % so we can detemine when the timeout is reached
    Receiver =
    proc_lib:spawn_link(
        fun() ->
            receive
                {Parent, Tag} ->
                    ?Debug("start send request:~w to ~p~n", [Req, ENs]),
                    {Mons, SendBads} = send_req(ENs, Tag, Req, N),
                    % notify the node supervisor module the bad nodes
                    e2d_node_sup:nodedown(SendBads),

                    {Replies, _BadReplies, DownBads} = gather_response(Mons, Tag, R),
                    ?Debug("gather the response ~p~n", [Replies]),

                    e2d_node_sup:nodedown(DownBads),

                    RepToParent = valid_reply(Req, Replies, R),

                    % send the rsp
                    Parent ! {Tag, RepToParent},

                    % wait a moment
                    hanle_later_rsp(Mons, Tag, RepToParent);
                             _ ->
                    ?Debug("the do_route exit normal~n", []),
                    ok
            end
        end
    ),

    %start to gather rsp
    Receiver ! {self(), Tag},
    receive
        {timeout, TRef, _} -> % timeout
            % kill the Receiver process
            exit(Receiver, shutdown),
            ?Log("request timeout~n", []),
            {error, timeout};
        {Tag, Rsp} -> % the rsp
            Rsp
    end.


%% send req to N nodes, if the node in preference list
%% is unreachable, then select the next one
send_req(Nodes, Tag, Req, N) ->
    send_req(Nodes, Tag, Req, N, [], []).

send_req([], _Tag, _Req, _N, Mons, Bads) ->
    {Mons, Bads};
send_req(_Nodes, _Tag, _Req, 0, Mons, Bads) ->
    {Mons, Bads};
send_req([Node|T], Tag, Req, N, Mons, Bads) ->
    MRef = erlang:monitor(process, {?NODE_STORE_SERVER, Node}),
    receive
        {'DOWN', MRef, _, _, _} -> % the node is die
            send_req(T, Tag, Req, N, Mons, [Node|Bads])
    after 0 ->
            % we think the node is live, so send msg
            catch {?NODE_STORE_SERVER, Node} ! {'$gen_call', {self(), {Tag, Node}}, Req},
            send_req(T, Tag, Req, N-1, [{Node, MRef}|Mons], Bads)
    end.

valid_reply(Req, Replies, Expect) ->
    ValidReplies =
    [V || {ok, V} <- Replies],

    if length(ValidReplies) =:= Expect ->
            case element(1, Req) of
                get ->
                    {ok, reconcile(ValidReplies)};
                put ->
                    ok;
                del ->
                    ok
            end;
        true -> % the result is not enough
            {error, {server_internal, not_enough_reply}}
    end.

%% gather the rsponses
gather_response(Mons, Tag, N) ->
    gather_response(Mons, Tag, N, [], [], []).

gather_response([_|_], _Tag, 0, Reps, RepEs, Bads) ->
    {Reps, RepEs, Bads};
gather_response([], _Tag, _N, Reps, RepEs, Bads) ->
    {Reps, RepEs, Bads};
gather_response(Mons, Tag, N, Reps, RepEs, Bads) ->
    receive
        {{Tag, Node}, Reply} ->
            {value, {_, MRef}, T} = lists:keytake(Node, 1, Mons),
            unmonitor(MRef),
        case Reply of
            {ok, _V} = Value ->
                ?Log("~p receive Value:~p", [Node, Value]),
                gather_response(T, Tag, N-1, [Value | Reps], RepEs,Bads);
            {error, _R} ->
                ?Warn("~p error reply :~p", [Node, _R]),
                gather_response(T, Tag, N, Reps, [Node | RepEs], Bads)
        end;
        {'DOWN', MRef, _, {_, Node}, _} ->
            unmonitor(MRef),
            {value, _, T} = lists:keytake(MRef, 2, Mons),
            gather_response(T, Tag, N, Reps, RepEs, [Node|Bads])
    end.

erlang_nodes(Nodes) ->
    erlang_nodes(Nodes, []).

erlang_nodes([], Acc) ->
    lists:reverse(Acc);
erlang_nodes([#e2dnode{name = Name} | Tail], Acc) ->
    erlang_nodes(Tail, [Name | Acc]).


unmonitor(Ref) when is_reference(Ref) ->
    erlang:demonitor(Ref),
    receive
        {'DOWN', Ref, _, _, _} ->
            true
    after 0 ->
            true
    end.

%% handle the later rsp
hanle_later_rsp(Mons, Tag, Rsp) ->
    Timer = erlang:start_timer(10000, self(), ok),
    receive
        {{Tag, Node}, Reply} ->
            {value, {_, MRef}, T} = lists:keytake(Node, 1, Mons),
            unmonitor(MRef),
            % if the data is older the the previous rsp
            read_repair({Node, Reply}, Rsp),

            hanle_later_rsp(T, Tag, Rsp);
        {'DOWN', MRef, _, _, _} ->
            unmonitor(MRef),
            {value, _, T} = lists:keytake(MRef, 2, Mons),
            hanle_later_rsp(T, Tag, Rsp);
        {timeout, Timer, _} ->
            exit(normal)
    end.


%% reconcile the get replies
reconcile(VDs) ->
    reconcile(VDs, []).

reconcile([H], Acc) ->
    lists:reverse(Acc, [H]);
reconcile([H | T], Acc) ->
    case check_version(T, H) of
        {consume, T2} -> % consume
            reconcile(T2, Acc);
        divergent -> % differnt with all other version info, so add to result list
            reconcile(T, [H | Acc])
    end.


check_version(VDs, VD) ->
    check_version(VDs, VD, VDs).

check_version([], _VD, _Acc) ->
    divergent;
check_version([{Vch, _Dh} | T], VD = {Vc, _D}, Acc) ->
    case vclock:descends(Vc, Vch) of
        true ->  % Vc is new
            Acc2 = lists:keyreplace(Vch, 1, Acc, VD),
            {consume, Acc2};
        false -> % Vc is older, do nothing
            {consume, Acc};
        divergent ->
            check_version(T, VD, Acc)
    end.


read_repair({_Node, _Reply}, _Rsp) ->
    not_impl.
