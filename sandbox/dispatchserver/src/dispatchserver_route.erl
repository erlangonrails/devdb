%%%-------------------------------------------------------------------
%%% File    : dispatchserver_route.erl
%%% Author  : liqiang 
%%% Description : 
%%%
%%% Created :  2 Jun 2010 by liqiang 
%%%-------------------------------------------------------------------
-module(dispatchserver_route).

-behaviour(gen_server).

%% API
-export([start_link/0, get_xmpp_service/0, get_req_count/0, get_cluster/0, get_uptime/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {uptime, cluster, len, index, req_count}).

get_xmpp_service() ->
    gen_server:call(?MODULE, 'get_xmpp_service').

get_req_count() ->
    gen_server:call(?MODULE, 'get_req_count').

get_cluster() ->
    gen_server:call(?MODULE, 'get_cluster').

get_uptime() ->
    gen_server:call(?MODULE, 'get_uptime').

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
    Cluster = tcp_server:get_config(cluster, []),
    {ok, #state{uptime=now(),cluster=Cluster, len=length(Cluster), index=1, req_count=0}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call('get_xmpp_service', _From, 
             #state{cluster=Cluster, len=Len, index=Index, req_count=Count}=State) when Index < Len ->
    [Reply] = lists:sublist(Cluster, Index, 1),
    {reply, Reply, State#state{index=Index + 1, req_count=Count + 1}};
handle_call('get_xmpp_service', _From,
             #state{cluster=Cluster, len=Len, index=Index, req_count=Count}=State) when Index >= Len ->
    [Reply] = lists:sublist(Cluster, Index, 1),
    {reply, Reply, State#state{index=1, req_count=Count + 1}};
handle_call('get_req_count', _From, #state{req_count=Count}=State) ->
    Reply = Count,
    {reply, Reply, State};
handle_call('get_cluster', _From, #state{cluster=Cluster}=State) ->
    Reply = Cluster,
    {reply, Reply, State};
handle_call('get_uptime', _From, #state{uptime=Uptime}=State) ->
    Reply = Uptime,
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
