%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc sync the data
-module(e2d_sync).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("e2d.hrl").

-export([update_buckets/0]).
-export([sync_buckets/2, sync_bucket/2]).

%% @doc update all the buckets from the nodes
-spec update_buckets() ->
    'ok'.
update_buckets() ->
    ?Debug("start update all buckets~n", []),
    Id = e2d_node:id(),

    SuccNode =
    case e2d_membership:successor(Id, 1) of
        [] ->
        ?Error("get sucessor empty~n", []),
            throw(no_successor);
        [Node] ->
            Node
    end,

    {ok, SuccInchBkts} = e2d_comm:request(SuccNode, {incharge_buckets_id}),

    BucketIndex = bucket_index(Id),
    {InchBkts, _Rest} = split_buckets(SuccInchBkts, BucketIndex),

    ?Debug("sync incharge buckets from successor node:~w~n", [SuccNode]),
    sync_bucket(SuccNode, InchBkts),

    ?Debug("sync replica buckets from successor~n", []),
    sync_replica_buckets(SuccNode),

    ?Debug("sync data complete! notify the N successor do remove some buckets~n", []),
    N = e2d_config:get(n),
    SuccNodes = e2d_membership:successor(Id, N),
    lists:foreach(fun(Node) ->
                          e2d_comm:request(Node, {remove_buckets, InchBkts})
                  end,
                  SuccNodes),
    ok.


%% @doc sync the Buckets from the Node
-spec sync_buckets(Node :: atom(), Buckets :: list() | non_neg_integer()) ->
    bool().
sync_buckets(Node, Buckets) when is_list(Buckets) andalso is_atom(Node) ->
    Ret =
    lists:all(fun(B) ->
                      case sync_bucket(Node, B) of
                          ok ->
                              true;
                          {error, _R} ->
                              false
                      end
              end,
              Buckets),
    Ret.

%% @doc sync the Bucket from the Node
-spec sync_bucket(Node :: atom(), Bucket :: non_neg_integer()) ->
    'ok' | {'error', atom()}.
sync_bucket(Node, Bucket)  ->
    ?Debug("sync bucket:~w ~w ~n", [Node, Bucket]),
    ?Debug("compare merkle tree root hash~n", []),
    RootSelf = e2d_store:get_merkle_root(Bucket),
    case e2d_comm:request(Node, {get_merkle_root, Bucket}) of
        {ok, RootSelf} -> % the data is same
            ok;
        {ok, _Root} -> % the data is not same
            MerkSelf = e2d_store:get_merkle(Bucket),
            case e2d_comm:request(Node, {get_merkle, Bucket}) of % request entire merkle tree
                {ok, Merkle} ->
                    Diffs = merkerl:diff(MerkSelf, Merkle),
                    get_key(Node, Diffs);
                {error, _R} ->
                    ?Warn("get merkle tree error, from ~w~n", [Node]),
                    {error, e2d_comm}
            end;
        {error, _R} ->
            {error, e2d_comm}
    end.

%% @doc get key from nodes and save it to local storage
get_key(Node, Keys) ->
        case e2d_comm:reqeust(Node, {get_key, Keys}) of
                {ok, KCVs} ->
                        ?Debug("add KCV(key context value) into local store~n", []),
                        lists:foreach(fun({Key, Context, Value} = _KCV) ->
                                        e2d_stroe:add(Key, Context, Value)
                                end,
                                KCVs);
                {error, _R} ->
                        ?Warn("get key error, from ~w~n", [Node]),
                        {error, e2d_comm}
        end.

%%
%% internal API
%%

sync_replica_buckets(SuccNode) ->
    {ok, ReplicaBkts} = e2d_comm:request(SuccNode, {replica_buckets_id}),
    sync_bucket(SuccNode, ReplicaBkts).

%% get bucket index from id
bucket_index(Id) ->
    NumberofBuckets = e2d_config:get(buckets_number),
    R = e2d_util:bucket_range(NumberofBuckets),
    (Id + R - 1) div R.

%% split the buckets
split_buckets(Buckets, B) ->
    case e2d_util:split_list(Buckets, B) of
        {[], _B, _L}->
            ?Error("the node is not in successor's buckets rang~n", []),
            exit(hash_ring);
        {Pre, B, Rest} ->
            {Pre ++ [B], Rest}
    end.
