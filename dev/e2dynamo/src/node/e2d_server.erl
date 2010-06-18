%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc handle the request, it communicate with the external
-module(e2d_server).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("e2d.hrl").
-behaviour(gen_server).

-export([start_link/0]).
-export([get/1, put/3, del/1]).
-export([get_version/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-type value_info() :: [{value(), context()}].

-spec start_link() -> {'ok', pid()} | 'ingore' | {'error', any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc get the key
-spec get(Key :: key()) ->
    {'ok', any()} | {'error', atom()}.
get(Key) ->
    gen_server:call(?SERVER, {api, {get, Key}}, infinity).

%% @doc update the key
-spec put(Key :: key(), Context :: context(), Value :: value()) ->
    {'ok', any()} | {'error', atom()}.
put(Key, Context, Value) ->
    gen_server:call(?SERVER, {api, {put, Key, Context, Value}}, infinity).

%% @doc delete the key
-spec del(Key :: key()) ->
    'ok' | {'error', atom()}.
del(Key) ->
    gen_server:call(?SERVER, {api, {del, Key}}, infinity).

%% @doc get the application version
-spec get_version() -> string().
get_version() ->
    Apps = application:loaded_applications(),
    case lists:keysearch(e2d_node, 1, Apps) of
    {value, {_, _, Vsn}} ->
        Vsn;
    false ->
        "0.0.0"
    end.

%% gen_server callbacks
init(_Args) ->
    ?Debug("~p init~n", [?MODULE]),
    {ok, []}.

handle_call({api, Req}, From, State) ->
    % don't block the main loop, the reply will be send in new process
    do_api_request(Req, From, State),
    {noreply, State};

handle_call({node_req, Req}, _From , State) ->
    Reply = do_req(Req),
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({api, from, From, Req}, State) ->
    do_api_request(Req, From, State),
    {noreply, State};
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

%% do the api request
do_api_request(Req, From, _State) ->
    proc_lib:spawn_link(
        fun() ->
            Key = get_key(Req),
            {ok, Coor, Cands} = e2d_membership:select_coordinator(Key),
            ?Debug("the Coordinator is ~p~n candidate:~p~n", [Coor, Cands]),
            IsMe = Coor#e2dnode.name =:= node(),
            case IsMe of % I'm the coordinator
                true ->
                    ?Debug("i'm the coordinator~n", []),
                    Result = e2d_coordinator:route(Req, [Coor | Cands]),
                    ?Debug("~p route return:~n~p~n", [?MODULE, Result]),
                    gen_server:reply(From, Result);
                false -> % transmit the api request to the fact coordinator
                    ?Debug("transmit the api request to the coordinator:~p~n", [Coor]),
                    gen_server:cast({e2d_server, Coor#e2dnode.name}, {api, from, From, Req})
            end
      end).

%% get the parmater Key from request
get_key({get, Key}) ->
        Key;
get_key({put, Key, _Context, _Value}) ->
        Key;
get_key({del, Key}) ->
        Key.

%% handle the request from the other nodes
do_req({node_info}) ->
    e2d_node:self();
do_req({remove_buckets, Buckets}) ->
    e2d_store:remove_buckets(Buckets);
do_req({incharge_buckets_id}) ->
    e2d_store:get_buckets_id(incharge);
do_req({replica_buckets_id}) ->
    e2d_store:get_buckets_id(replica);
do_req({get_key, Key}) ->
    e2d_store:get(Key).
