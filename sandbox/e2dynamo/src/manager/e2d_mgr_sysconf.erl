%%%-------------------------------------------------------------------
%%% File    : e2d_mgr_sysconf.erl
%%% Author  : liqiang 
%%% Description : 
%%%
%%% Created : 21 Jun 2010 by liqiang 
%%%-------------------------------------------------------------------
-module(e2d_mgr_sysconf).
-include("e2d_manager.hrl").
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(CONFFILE, "e2d_manager.conf").

%% API
-export([start_link/0]).
-export([get/1, get/2]).
-export([set/1, set/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-spec get(Par :: [atom()] | atom()) -> [any() | 'undefined'] | any() | 'undefined'.
get(Pars) when is_list(Pars) ->
    gen_server:call(?SERVER, {get_multi, Pars});
get(Par) when is_atom(Par) ->
    gen_server:call(?SERVER, {get, Par}).

-spec get(Par :: atom(), Def :: any()) -> any().
get(Par, Def) when is_atom(Par) ->
    case gen_server:call(?SERVER, {get, Par}) of
        undefined ->
            Def;
        Val ->
            Val
    end.

set(KVs) when is_list(KVs) ->
    lists:foreach(
        fun({Key, Val}) ->
            ?MODULE:set(Key, Val)
        end,
        KVs).

set(Key, Value) ->
    {Key2, Value2} = normalize_kv(string:to_lower(Key), Value),
    gen_server:call(?SERVER, {set, Key2, Value2}).

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
    case filelib:is_file(?CONFFILE) of
        true ->
            case file:consult(?CONFFILE) of
                {ok, [Config]} ->
                    {ok, Config};
                _  ->
                    ?Error("the ~p file format error!~n", [?CONFFILE]),
                    {stop, e_conf_format}
            end;
        false ->
            ?Warn("not specify the ~p config file~n", [?CONFFILE]),
            {stop, e_conf}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get, Par}, _From, State) ->
    Reply = proplists:get_value(Par, State),
    {reply, Reply, State};
handle_call({get_multi, Pars}, _From, State) ->
    Reply =
    lists:map(fun(Key) ->
                      proplists:get_value(Key, State)
              end,
              Pars),
    {reply, Reply, State};
handle_call({set, Key, Value}, _From, State) ->
    State2 = lists:keyreplace(Key, 1, State, {Key, Value}),
    {reply, ok, State2};
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
normalize_kv([Q], Value) when Q =:= $n orelse Q =:= $r orelse Q =:= $w ->
    {list_to_atom([Q]), list_to_integer(Value)};
normalize_kv("cookie", Value) ->
    {cookie, list_to_atom(Value)};
normalize_kv("bn", Value) ->    % buckets_number
    {buckets_number, list_to_integer(Value)}.
