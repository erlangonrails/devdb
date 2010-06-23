%% @doc test the http interface: GET, POST, DELETE

-module(test_e2d_http_api).
-export([start/1, multi_req/2, aget/2, aput/2, adel/2]).

start(Req) ->
    inets:start(),
    Reply = Req(self()),
    io:format("Ret:~p~n", [Reply]),
    ok.

%% multi process the same request
multi_req(Req, N) when is_function(Req), is_integer(N) ->
    inets:start(),
    Parent = self(),
    Reqs =
    [spawn(fun() ->Req(Parent) end) ||  _ <- lists:seq(1, N)],
    Rsps = gather_rsp(N),
    analyze(Reqs, Rsps, N).

gather_rsp(N) ->
    gather_rsp0(N, []).

gather_rsp0(0, Acc) ->
    lists:reverse(Acc);
gather_rsp0(N, Acc) ->
    receive
        {_Child, _Reply, _Time} = V ->
            gather_rsp0(N-1, [V | Acc]);
        _ ->
            gather_rsp0(N, Acc)
    end.

analyze(_Children, Rsps, N) ->
    {SuccN, SuccList} =
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

    ErrN =
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

    SortSucc = lists:keysort(3, SuccList),
    case SortSucc of
        [] ->
            MinT = 0,
            MaxT = 0,
            AvgT = 0;
        [_ | _] ->
            {_P1, _R1, MinT} = hd(SortSucc),
            {_P2, _R2, MaxT} = lists:last(SortSucc),
            AvgT = lists:foldl(
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
aget(Srv, Key) ->
    fun(Parent) ->
        Start = erlang:now(),
        Reply = e2d_http_api:get(Srv, Key),
        End = erlang:now(),
        Parent ! {self(), Reply, timer:now_diff(End, Start)}
    end.

aput(Srv, Arg) ->
    fun(Parent) ->
        Start = erlang:now(),
        Reply = e2d_http_api:put(Srv, Arg),
        End = erlang:now(),
        Parent ! {self(), Reply, timer:now_diff(End, Start)}
    end.

adel(Srv, Key) ->
    fun(Parent) ->
        Start = erlang:now(),
        Reply = e2d_http_api:del(Srv, Key),
        End = erlang:now(),
        Parent ! {self(), Reply, timer:now_diff(End, Start)}
    end.
