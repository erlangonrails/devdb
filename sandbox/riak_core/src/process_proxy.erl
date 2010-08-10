-module(process_proxy).
-export([start_link/2, init/1, stop/1]).

%%
%% 代理进程模块:
%% 发送到代理进程的消息, 都会被转发到ProxyTo进程.
%% (代理进程接收到stop消息后退出)
%%

-spec(start_link(RegName :: atom(), ProxyTo :: pid()) -> {ok, pid()}).
start_link(RegName, ProxyTo) ->
    proc_lib:start_link(?MODULE, init, [[self(), RegName, ProxyTo]]).

init([ParentPid, RegName, ProxyTo]) ->
    erlang:register(RegName, self()),
    proc_lib:init_ack(ParentPid, {ok, self()}),
    loop(ProxyTo).

stop(Name) ->
    Name ! stop.

loop(ProxyTo) ->
    receive
        stop ->
            exit(normal);
        M ->
            ProxyTo ! M,
            loop(ProxyTo)
    end.
