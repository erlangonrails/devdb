%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the data manager
-module(e2d_store).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("e2d.hrl").
-behaviour(gen_server).

-export([start_link/0]).

%% data operations
-export([get/1, put/3, del/1]).

%% about buckets
-export([remove_buckets/1, get_buckets_id/1]).

%% about the merkle tree
-export([get_merkle_root/1, get_merkle/1, update_merkle_tree/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-record(state, {
          buckets = [] :: list(), % the buckets list
          store_handler = e2d_store_mnesia :: atom() % the stroe handler
         }).


-define(SERVER, ?MODULE).
-type value_info() :: [{value(), context()}].


-spec start_link() -> {'ok', pid()} | 'ingore' | {'error', any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get(Key :: key()) -> {'ok', value_info()} | {'error', any()}.
get(Key) ->
    gen_server:call(?SERVER, {get, Key}).

-spec put(Key :: key(), Context :: context(), Value :: value())
        -> 'ok' | {'error', any()}.
put(Key, Context, Value) ->
    gen_server:call(?SERVER, {put, Key, Context, Value}).

-spec del(Key :: key()) -> 'ok' | {'error', any()}.
del(Key) ->
    gen_server:call(?SERVER, {del, Key}).

%% @doc remove the data belong to the buckets
remove_buckets(Buckets) ->
    gen_server:call(?SERVER, {remove_buckets, Buckets}).

%% @doc
get_buckets_id(Type) ->
    gen_server:call(?SERVER, {get_buckets_id, Type}).

%% @doc get root hash of merkle tree
get_merkle_root(Bucket) ->
    gen_server:call(?SERVER, {get_merkle_root, Bucket}).

%% @doc get all the merkle tree
get_merkle(Bucket) ->
    gen_server:call(?SERVER, {get_merkle, Bucket}).

%% @doc update the merkle tree
update_merkle_tree(_Bucket) ->
    ok.

%% gen_server callbacks
init(_Args) ->
    ?Debug("~p init~n", [?MODULE]),
    ets:new(merkle, [ordered_set, protected, named_table, {keypos, 1}]),
    State = #state{},
    Handler = e2d_config:get(mod_store_handler),
    ?Debug("~p init~n", [Handler]),
    Handler:init(),

    Buckets = Handler:buckets(),
    ?Debug("get buckets ~p ~n", [Buckets]),
    build_merkle(Handler, Buckets),
    {ok, State#state{store_handler = Handler, buckets = Buckets}}.

handle_call({get, Key}, _From, State) ->
    Reply = do_get(Key, State),
    {reply, Reply, State};
handle_call({put, Key, Context, Value}, _From, State) ->
    Reply = do_put(Key, Context, Value, State),
    ?Debug("~p put : ~p ~n Result:~p~n", [?MODULE, Key, Reply]),
    {reply, Reply, State};
handle_call({del, Key}, _From, State) ->
    Reply = do_del(Key, State),
    {reply, Reply, State};
handle_call({get_buckets_id, Type}, From, State) ->
    do_heavy_action({get_buckets_id, Type}, From, State),
    {noreply, State};
handle_call({remove_buckets, Buckets}, From, State) ->
    do_heavy_action({remove_buckets, Buckets}, From, State),
    {noreply, State};
handle_call({get_merkle_root, Bucket}, _From, State) ->
        [Merkle] = ets:lookup(merkle, Bucket),
        Reply = merkerl:root(Merkle),
    {reply, Reply, State};
handle_call({get_merkle, Bucket}, _From, State) ->
    [Merkle] = ets:lookup(merkle, Bucket),
    {reply, Merkle, State};
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

%%
%% internal API
%%
do_get(Key, State) ->
    Handler = State#state.store_handler,
    Handler:get(Key).

do_put(Key, Context, Value, State) ->
    Handler = State#state.store_handler,
        Handler:put(Key, Context, Value).

do_del(Key, State) ->
    Handler = State#state.store_handler,
        Handler:del(Key).


%% spawn a new process do some heavy operations
do_heavy_action(Req, From, _State) ->
    proc_lib:spawn(fun() ->
                           Reply = process_req(Req),
                           gen_server:reply(From, Reply)
                           end),
    ok.

process_req({remove_buckets, Buckets}) ->
    % accrodding to the bucket rang, del the keys in local
    lists:foreach(fun(B) ->
                          %traverse_keys(B)
                          remove_bucket(B)
                  end,
                  Buckets);
process_req({get_buckets_id, _Type}) ->
    exit(not_impl).

remove_bucket(B) ->
    exit(not_impl).

%% build the merkle tree from the data
build_merkle(Handler, Buckets) ->
    ?Debug("Build the merkle tree: Buckets ~p~n", [Buckets]),
    NumberofBuckets = e2d_config:get(buckets_number),
    Range = e2d_util:bucket_range(NumberofBuckets),
    lists:map(
      fun(B) ->
              Begin = Range * B,
              End = Begin + Range - 1,
              Keys = Handler:keys(Begin, End),
              Tree =
                  lists:foldl(fun(K, T) ->
                                      {ok, {_Context, Value}} = Handler:get(K),
                                      merkerl:insert({K, merkerl:sha(Value)}, T)
                              end,
                              undefined,
                              Keys),
              ?Debug("insert merkle tree:~w~n", [B]),
              ets:insert(merkle, {B, Tree})
      end,
      Buckets).
