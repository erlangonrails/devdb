-module(dispatchserver_benchmark).
-export([start/0]).

%% Benchmark配置:
%% 根据配置文件dispatchserver.cfg修改下面的默认设置
-define(DEF_IP, "127.0.0.1").
-define(DEF_PORT, 5225).
-define(DEF_TIMEOUT, 5000).  %% 如果一个请求超过这个时间仍然没有返回, 则认为该请求失败

start() ->
    case init:get_plain_arguments() of
	["benchmark", ReqCount, ReqInterval] ->
            multi_req(list_to_integer(ReqCount),
		      list_to_integer(ReqInterval)),
            halt(0);
	Any ->
	    io:format("benchmark arguments error# ~p!~n", [Any]),
            halt(0)   
    end.

multi_req(ReqCount, ReqInterval) ->
    ReqFun = send_req(?DEF_IP, ?DEF_PORT),
    multi_req(ReqFun, ReqCount, ReqInterval).

%% multi process the same request
multi_req(Req, N, Interval) when is_function(Req), is_integer(N) ->
    Parent = self(),
    Reqs = lists:map(fun(_Item) ->
                         timer:sleep(Interval),
                         spawn(fun() ->Req(Parent) end)
                     end, lists:seq(1, N)), %% Reqs: the list of child process pids
    Rsps = gather_rsp(N),
    analyze(Reqs, Rsps, N).

gather_rsp(N) ->
    gather_rsp0(N, []).

gather_rsp0(0, Acc) ->
    lists:reverse(Acc);
gather_rsp0(N, Acc) ->
    receive
        {_Child, _Reply, _Time} = V ->    %% Child: is the pid of child process
	                                  %% Reply: {ok, Ret} | {error, Ret}
            gather_rsp0(N-1, [V | Acc]);
        _ ->
            gather_rsp0(N, Acc)
    end.

analyze(_Children, Rsps, N) ->
    {SuccN, SuccList} = %% 统计成功的结果{ok, Ret}
    lists:foldl(
        fun({_P, R, _T} = Elem, {AccN, AccL}) ->
            case R of
                {ok, _} ->
                    {AccN + 1, [Elem | AccL]};
                _ ->
                    {AccN, AccL}
            end
        end,
        {0, []},
        Rsps),

    ErrN = %% 统计失败的结果{ok, Ret}
    lists:foldl(fun({_P, R, _T}, Acc) ->
            case R of
                {error, _ }->
                    Acc + 1;
                _ ->
                    Acc
            end
        end,
        0,
        Rsps),

    SortSucc = lists:keysort(3, SuccList), %% 按照Time排序结果: 小 -> 大
    case SortSucc of
        [] ->
            MinT = 0,
            MaxT = 0,
            AvgT = 0;
        [_ | _] ->
            {_P1, _R1, MinT} = hd(SortSucc), %% 用时最少的请求
            {_P2, _R2, MaxT} = lists:last(SortSucc), %% 用时最多的请求
            AvgT = lists:foldl( %% 计算平均时间
                fun({_P, _R, T} = _Elem, Sum) ->
                    T + Sum
                end,
            0,
            SortSucc) / SuccN
    end,

    io:format("
    Analyze:
    Request N:  ~p  Success: ~p Error: ~p
    Timer (ms) Min: ~p Max:~p Avg:~p ~n",
    [N, SuccN, ErrN, MinT / 1000, MaxT / 1000, AvgT / 1000]).


%% actions
send_req(IP, Port) ->
    fun(Parent) ->
        Start = erlang:now(),
        {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {packet, 0}]),
        receive
	    {tcp, Socket, <<"127.0.0.1:5222">>} -> %% 参考dispatchserver.cfg
	        gen_tcp:close(Socket),
                End = erlang:now(),
	        Parent ! {self(), {ok, success}, timer:now_diff(End, Start)};
            {tcp, Socket, <<"192.168.0.13:5222">>} -> %% 参考dispatchserver.cfg
	        gen_tcp:close(Socket),
                End = erlang:now(),
	        Parent ! {self(), {ok, success}, timer:now_diff(End, Start)}
        after ?DEF_TIMEOUT ->
            End = erlang:now(),
            gen_tcp:close(Socket),
	    Parent ! {self(), {error, timeout}, timer:now_diff(End, Start)}
        end
    end.

