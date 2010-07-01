%%%-------------------------------------------------------------------
%%% File    : ejabberd_redis.erl
%%% Author  : liqiang 
%%% Description : 
%%%
%%% Created : 28 Jun 2010 by liqiang 
%%%-------------------------------------------------------------------
-module(ejabberd_redis).

-behaviour(gen_server).

%% API
-export([ping/0,
	 delete/1]).
-export([set/2,
	 set/3,
	 get/1]).
-export([set_add/2,
	 set_rm/2,
	 set_members/1,
	 set_random_member/1]).
-export([get_redis/0]).
-export([start_link/3]).

-define(SERVER, ?MODULE).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {host, port, count}).

%%------------------------------------------------------------------------------
%% generic commands
%%------------------------------------------------------------------------------
%% -spec ping() -> 'ok' | {'error', [atom()]}.
ping() ->
    Redis = get_redis(),
    Redis:ping().

%% -spec delete(Keys :: [key()]) -> boolean().
delete(Key) ->
    Redis = get_redis(),
    Redis:delete(Key).

%%------------------------------------------------------------------------------
%% string commands 
%%------------------------------------------------------------------------------
%% -spec set(Key :: key(), Val :: str()) -> 'ok'.
set(Key, Val) ->
    Redis = get_redis(),
    Redis:set(Key, Val).

%% -spec set(key(), str(), second()) -> 'ok'.
set(Key, Val, Expire) ->
    Redis = get_redis(),
    Redis:set(Key, Val, Expire).

%% -spec get(Key :: key()) -> null() | binary().
get(Key) ->
    Redis = get_redis(),
    Redis:get(Key).

%%------------------------------------------------------------------------------
%% set commands 
%%------------------------------------------------------------------------------

%% -spec set_add(Key :: key(), Mem :: str()) -> boolean().
set_add(Key, Mem) ->
    Redis = get_redis(),
    Redis:set_add(Key, Mem).

%% -spec set_add(Key :: key(), Mem :: str()) -> boolean().
set_rm(Key, Mem) ->
    Redis = get_redis(),
    Redis:set_rm(Key, Mem).

%% spec set_members(Key :: key()) -> [value()].
set_members(Key) ->
    Redis = get_redis(),
    Redis:set_members(Key).

%% -spec set_random_member(Key :: key()) -> value() | null()
set_random_member(Key) ->
    Redis = get_redis(),
    Redis:set_random_member(Key).

get_redis() ->
    gen_server:call(?SERVER, {get_redis}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Port, Count) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, 
			  [Host, Port, Count], []).

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
init([Host, Port, Count]) ->
    {ok, #state{host=Host, port=Port, count=Count}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get_redis}, _From, State = #state{host = Host, port = Port, count = Count}) ->
    Redis = get_redis(Host, Port, Count),
    {reply, Redis, State};
handle_call(_Request, _From, State) ->
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

%% Usage:
%% Redis = get_redis(Host, Port, Count),
%% Redis:set("k1", "v1"),
%% Redis:get("k1").
get_redis(Host, Port, Count) ->
    Selected = redis_client:existing_name(Host, Port, random:uniform(Count)),
    redis_client:handler(Selected).


