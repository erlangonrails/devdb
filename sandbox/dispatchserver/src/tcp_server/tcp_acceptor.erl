%%%-------------------------------------------------------------------
%%% File    : tcp_acceptor.erl
%%% Author  : liqiang 
%%% Description : 针对一个ListenSocket循环调用prim_inet:async_accept/2来接收
%%                客户端的链接.
%%%
%%% Created : 27 May 2010 by liqiang 
%%%-------------------------------------------------------------------
-module(tcp_acceptor).

-behaviour(gen_server).
-include("ds_log.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {callback, %% {M, F, A}, 接收到新进的TCP链接后的回调函数
                          %% 调用方式: apply(M, F, A ++ [Socket])
		sock,     %% ListenSocket
		ref}).    %% 调用prim_inet:async_accept/2之后的返回值,
                          %% 每次调用该函数时都会更新

%%====================================================================
%% 模块功能:
%% 该模块功能很简单, 只有一个APIs:
%% 就是start_link(Callback, ListenSock), 该函数用来来启动一个gen_server
%% 来接受客户端的TCP链接, 每接受到一个链接就调用apply(M, F, A ++ [Sock]), 然后继续
%% 接收客户端链接.
%%
%% 备注:
%% 1. 该模块使用了没有被文档化的prim_inet:async_accept/2函数来接收连接,
%%    我们使用该模块, 需要打"patch", 使之Socket看起来和gen_tcp:accept/2得到的
%%    一样.
%%
%% 2. 其实erlang driver里面的accept操作肯定是async的, 或者说nonblocking的,
%%    不管我们调用gen_tcp:accept还是prim_inet:async_accept/2.
%%
%% 3. gen_tcp:accept/2和prim_inet:async_accept/2的区别是accept帮我们处理了
%%    下面的消息:
%%    {inet_async, ListenSock, Ref, {ok, Socket}} 
%%    {inet_async, ListenSock, Ref, {error, closed}
%%    返回{ok, Socket}或者{error, closed}等结果.
%%    而async_accept调用prot_control后直接返回了, 需要我们自己去接收并处理
%%    {inet_async, ...}消息.
%%
%%====================================================================

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(Callback, ListenSock) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%
%% Callback = {M, F, A}
%%--------------------------------------------------------------------
start_link(Callback, ListenSock) ->
    gen_server:start_link(?MODULE, {Callback, ListenSock}, []).

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
init({Callback, ListenSock}) ->
    case prim_inet:async_accept(ListenSock, -1) of
	{ok, Ref} ->
	    {ok, #state{callback = Callback, sock = ListenSock, ref = Ref}};
	Error ->
	    {stop, {cannot_accept, Error}}
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
handle_info({inet_async, ListenSock, Ref, {ok, Sock}}, 
	    State = #state{callback = {M, F, A}, sock = ListenSock, ref = Ref}) ->
    %% patch up the socket so it looks like one we got from gen_tcp:accept/1
    {ok, Mod} = inet_db:lookup_socket(ListenSock),
    inet_db:register_socket(Sock, Mod),

    %% report
    {ok, {Address, Port}} = inet:sockname(ListenSock),
    {ok, {PeerAddress, PeerPort}} = inet:peername(Sock),
    ?DEBUG("accepted TCP connection on ~s:~p from ~s:~p", 
	    [inet_parse:ntoa(Address), Port, 
	     inet_parse:ntoa(PeerAddress), PeerPort]),
    
    %% handle
    apply(M, F, A ++ [Sock]),

    %% async accept more
    case prim_inet:async_accept(ListenSock, -1) of
	{ok, NRef} ->
	    {noreply, State#state{ref=NRef}};
	Error ->
	    {stop, {cannot_accept, Error}, none}
    end;
handle_info({inet_async, ListenSock, Ref, {error, closed}},
	    State = #state{sock = ListenSock, ref = Ref}) ->
    %% handle ListenSock closed error, restart it.
    %% Note:
    %% It would be wrong to attempt to restart the acceptor when we
    %% know this will fail.
    {stop, normal, State};
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
