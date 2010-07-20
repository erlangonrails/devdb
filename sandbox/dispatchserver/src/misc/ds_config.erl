%%%-------------------------------------------------------------------
%%% File    : ds_config.erl
%%% Author  : liqiang 
%%% Description : 
%%%
%%% Created : 29 May 2010 by liqiang
%%%-------------------------------------------------------------------
-module(ds_config).

-behaviour(gen_server).

%% API
-export([start_link/0, get_config/1]).
-export([get_log_level/0, get_client_module/0, get_listeners/0, get_cluster/0]).

-define(CONFIG_TABLE, '$dispatchserver_config').

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).


get_log_level() ->
    case get_config(loglevel) of
	{loglevel, Level} -> Level;
	undefined -> 3
    end.

get_client_module() ->
    case get_config(clientmodule) of
	{clientmodule, ClientModule} -> ClientModule;
	undefined -> undefined
    end.


get_listeners() ->
    case get_config(listeners) of
	{listeners, Listeners} -> Listeners;
	undefined -> undefined
    end.

get_cluster() ->
    case get_config(cluster) of
	{cluster, Cluster} -> Cluster;
	undefined -> undefined
    end.

get_config(Key) ->
    gen_server:call(?MODULE, {get, Key}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
    ets:new(?CONFIG_TABLE, [named_table]),
    load_config(),
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
handle_call({get, Key}, _From, State) ->
    Reply = case ets:lookup(?CONFIG_TABLE, Key) of
		[Val] -> Val;
		_ -> undefined
            end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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

load_config() ->
    {ok, Terms} = file:consult(ds:get_config_file()),
    lists:foreach(fun({Key, Val}) ->
		      ets:insert(?CONFIG_TABLE, {Key, Val})
		  end, Terms).
    
