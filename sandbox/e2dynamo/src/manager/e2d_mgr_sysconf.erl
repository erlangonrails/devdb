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

%% 功能:
%% 一个全局的配置模块, 其它模块可以从该模块读取配置文件的信息(get), 也可以更新(set)
%% 配置文件的信息.
%%
%% 说明:
%% 该模块是一个gen_server, 在启动的时候, 会自动读取e2d_manager.conf中的内容,
%% 里面的内容是proplists格式的数据, 读取之后作为gen_server的State进行传递,
%% 我们可以对这些数据做get/set操作, 来获取或者更新这些数据.
%% 例如:
%% [{n, 1},
%%  {w, 1}, 
%%  {r, 1},
%%  {cookie, 'e2dynamo-random-cookie243234xcvasf23423423cfdslluwer'}, 
%%  {api_timeout, 1000},
%%  {buckets_number, 1024} 
%% ].

%% 补充:
%% 1. TODO: 当前版本的配置文件必须和*.beam在同一个目录下, 在后续版本修复这个问题. 
%% 2. 约定所有的Key都是atom()类型, Value没有类型限制
%% 3. set/1, set/2我们在实现的时候使用的是lists:keyreplace/4而不是lists:keystore/4,
%%    这意味着我们只能更新配置文件中存在的Key对应的Value, 但是不能插入新的{Key, Value}
%%    对. - by design :)

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
    gen_server:call(?SERVER, {set, Key, Value}).

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
    Reply = proplists:get_value(Par, State, undefined),
    {reply, Reply, State};
handle_call({get_multi, Pars}, _From, State) ->
    Reply =
    lists:map(fun(Key) ->
                  proplists:get_value(Key, State, undefined)
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
