-module(tcp_proxy).

-include("erl_logger.hrl").
-include("balance.hrl").

%% Setsockopt buffer size.
-define(BUFSIZ, (128*1024)).

%% External exports
-export([init/1, init/2]).

%% Internal exports
-export([accept_init/2, proxy/1]).

init(LocalPort) ->
    init(LocalPort, self()).
init(LocalPort, BalancerPid) ->
    spawn_link(?MODULE, accept_init, [LocalPort, BalancerPid]). %% BalancePid: the process of bal_proxy

-record(state, {
	  timeout,
	  host,
	  port
	 }).


accept_init(LPort, BalancerPid) ->
    {ok, LSock} = gen_tcp:listen(LPort, [binary, 
					 {backlog, 256},
					 {nodelay, true}, 
					 {reuseaddr, true},
					 {active, false}]),
    acceptor(LSock, BalancerPid).

acceptor(LSock, BalancerPid) ->
    case gen_tcp:accept(LSock) of
	{ok, CSock} ->
	    Proxy = spawn_link(?MODULE, proxy, [CSock]),
	    inet:setopts(CSock, [{recbuf, ?BUFSIZ}, {sndbuf, ?BUFSIZ}]),
	    gen_tcp:controlling_process(CSock, Proxy),
	    Proxy ! {go_for_it, CSock, BalancerPid, self()},
	    acceptor(LSock, BalancerPid);
	Error ->
	    exit(Error)
    end.

%%%
%%% proxy() -- Wait for the {go_for_it, ...} tuple from our parent that
%%%    tells us that it's OK to continue to execute proxy2().
%%%
%%%

proxy(CSock) ->
    receive
	{go_for_it, CSock, BalancerPid, AcceptorPid} -> 
	    link(BalancerPid),
	    unlink(AcceptorPid),
	    ok
    end,
    proxy2(CSock, BalancerPid, bal_proxy:get_be(BalancerPid)).

%%%
%%% proxy2() -- Figure out which back-end host to connect to.  If we
%%%    get a good connection, then proceed with proxy_loop().  If not,
%%%    inform the balancer that we got an error, then ask the balancer
%%%    for a new back-end.
%%%

proxy2(CSock, BalancerPid, {ok, RHost, RPort, ConnTimeout, ActTimeout}) ->
    case gen_tcp:connect(RHost, RPort, [binary, {nodelay, true},
					{active, true}, {recbuf, ?BUFSIZ},
				        {sndbuf, ?BUFSIZ}], ConnTimeout) of
	{ok, SSock} ->
	    bal_proxy:remote_ok(BalancerPid),
	    inet:setopts(CSock, [{active, true}]),
	    proxy_loop(CSock, SSock, #state{timeout = ActTimeout,
					    host = RHost, 
					    port = RPort});
	Error ->
	    ?ERROR_MSG("proxy: TCP socket to remote server failed, Error = ~w\n", [Error]),
	    bal_proxy:remote_error(BalancerPid, Error),
	    proxy2(CSock, BalancerPid, bal_proxy:get_be(BalancerPid))
    end;
proxy2(_CSock, _BalancerPid, ?TIMEOUT_BE) ->
    ?INFO_MSG("proxy: backend timeout, no backends available at this time\n", []),
    exit(normal);
proxy2(_CSock, _BalancerPid, Answer) ->
    ?ERROR_MSG("proxy: got bad answer = ~w\n", [Answer]),
    exit(bad_answer).

%%%
%%% proxy_loop(ClientSock, ServerSock, State)
%%%
%%% At the moment, State is just the activity timeout
%%%
%%% Note that, at the moment, we're a bit stupid by exiting whenever
%%% either socket is closed or has an error.  At least in the case of
%%% closure, a nice proxy would continue forwarding data in the other
%%% direction.  However, that would mean we would have to carry more
%%% state in State, horror!  :-)
%%%

proxy_loop(closed, closed, _State) ->
    exit(byebye);
proxy_loop(CSock, SSock, State) ->
    receive
	{tcp, CSock, Data} ->
	    gen_tcp:send(SSock, Data),
	    proxy_loop(CSock, SSock, State);
	{tcp, SSock, Data} ->
	    gen_tcp:send(CSock, Data),
	    proxy_loop(CSock, SSock, State);
	{tcp_closed, _Sock} ->
	    exit(byebye);
	{tcp_error, Sock} ->
	    ?ERROR_MSG("proxy_loop: socket ~w ERROR\n", [Sock]),
	    exit(byebye);
	Msg ->
	    ?INFO_MSG("proxy_loop: got ~w\n", [Msg]),
	    proxy_loop(CSock, SSock, State)
    after State#state.timeout ->
	    ?INFO_MSG("proxy_loop: TIMEOUT after ~w\n", [State]),
	    exit(byebye)
    end.
