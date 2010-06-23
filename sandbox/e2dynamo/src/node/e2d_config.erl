%%%-------------------------------------------------------------------
%%% File    : e2d_config.erl
%%% Author  : liqiang 
%%% Description : 
%%%
%%% Created : 23 Jun 2010 by liqiang
%%%-------------------------------------------------------------------
-module(e2d_config).
-include("e2d.hrl").
-behaviour(gen_server).

-export([get_ms_config/2, get_e2d_config/1]).
-export([get/1, get/2]).
%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% @doc get configure from the manager server
-spec get_ms_config(Url :: string(), File :: string()) ->
    {'ok', list()} | {'error', atom()}.
get_ms_config([_|_] = Url, []) ->
    get_ms_config(Url);
get_ms_config([], [_|_] = File) ->
    get_ms_config_local(File);
get_ms_config([_|_] = Url, [_|_] = File) ->
    case get_ms_config(Url) of
        {ok, Config} ->
            {ok, Config};
        {error, _Reason} ->
            get_ms_config_local(File)
    end.

%% @doc load the local config default is ./conf/e2dynamo.conf
-spec get_e2d_config(Conf :: string()) ->
    {'ok', list()} | {'error', atom()}.
get_e2d_config(Conf) ->
    case filelib:is_file(Conf) of
        true ->
            case file:consult(Conf) of
                {ok, [Config]} ->
                    {ok, Config};
                _  ->
                    ?Error("the ~p file format error!~n", [Conf]),
                    {error, e_e2dynamo_conf}
            end;
        false ->
            ?Warn("not specify the ~p config file~n", [Conf]),
            {ok, []}
    end.

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

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Config) ->
    ?Debug("~p start_link ~n", [?MODULE]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).

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
init([Config]) when is_list(Config) ->
    ?Debug("~p init Arg:~p~n", [?MODULE, Config]),
    {ok, Config}.

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

%% get config from manager server based http
get_ms_config(Url) ->
    %% get the config from manager server
    ?Debug("start get ms config ~n",[]),
    case net_util:http_get(Url, 5000) of
        {ok, Result} ->
            case e2d_util:consult_str(Result) of
                {ok, _Term} = Config ->
                    Config;
                {error, _} ->
                    {error, eformat}
            end;
        {error, _Reason} ->
            {error, e_ms_config}
    end.

%% get the previous config from local file
get_ms_config_local(File) when is_list(File) ->
    ?Warn("get the old config info from local file ~p~n", [File]),
    case file:consult(File) of
        {ok, [Config]} ->
            {ok, Config};
        _ ->
            {error, e_ms_local_config}
    end.
