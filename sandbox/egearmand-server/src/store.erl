-module(store) .

-include_lib("eunit/include/eunit.hrl") .

-export([insert/3, next/2, dequeue/2, delete/3, all/2, delete_if/2, dequeue_if/2, alter/2, detect_if/2]) .

%% @doc
%% 每个Key对应的Val集合都是一个Queue.

%% @doc
%% 向Queue中插入一条记录:
%% Key -> atom()
%% Val -> [any()] (Val是一个列表, 里面的元素可以重复)
%%
%% 例子:
%% Q = store:insert(key1, val11, []) -> [{key1,[val11]}]
%% Q1 = store:insert(key1, val11, Q) -> [{key1,[val11,val11]}]
%% Q2 = store:insert(key1, val12, Q1) -> [{key1,[val11,val11,val12]}]
%% Q3 = store:insert(key2, val21, Q2) -> [{key2,[val21]},{key1,[val11,val11,val12]}]

-spec(insert(atom(), any(), [{atom(),[any()]}]) -> [{atom(),[any()]}]) .

insert(Key,Value,Queues) ->
    DoInsert = fun(_F,K,V,[],A)              -> [{K,[V]} | A] ;
                 (_F,K,V,[{K,Vs} | T], A)    -> A ++ [{K, lists:reverse([V|Vs])} | T] ;
                 (F,K,V,[{_K,_Vs}=H | T], A) -> F(F,K,V,T,[H|A])
               end ,
    DoInsert(DoInsert,Key,Value,Queues,[]) .


%% @doc
%% 遍历Key对应的Val, Queue是一个环形的队列.
%%
%% 例子:
%% Q = [{key1, [val1, val2, val3]}, {a, [vala]}]. %% 插入顺序为val1, val2, val3
%% {V1, Q1} = store:next(key1, Q)  -> {val1,[{key1,[val2,val3,val1]},{a,[vala]}]}
%% {V2, Q2} = store:next(key1, Q1) -> {val2,[{key1,[val3,val1,val2]},{a,[vala]}]}
%% {V3, Q3} = store:next(key1, Q2) -> {val3,[{key1,[val1,val2,val3]},{a,[vala]}]}
%% {V4, Q4} = store:next(key1, Q3) -> {val1,[{key1,[val2,val3,val1]},{a,[vala]}]}

-spec(next(atom(), [{atom(),[any()]}]) -> {atom(), [{atom(),[any()]}]}) .

next(Key,Queues) ->
    DoNext = fun(_F,_K,[],_A)               -> not_found ;
                (_F,K,[{K,[VH|VR]} | R], A) -> {VH, [{K, VR ++ [VH]} | R] ++ A} ; %% we found the queue
                (F,K,[{_K,_Vs} = H | R], A) -> F(F,K,R,[H|A])
             end ,
    DoNext(DoNext,Key,Queues,[]) .


%% @doc
%% 遍历 & 删除Key对应的Val.
%% (对比next/2)
%%
%% 例子:
%% Q = [{key1, [val1, val2, val3]}, {a, [vala]}]. %% 插入顺序为val1, val2, val3
%% {V1, Q1} = store:dequeue(key1, Q)  -> {val1,[{key1,[val2,val3]},{a,[vala]}]}
%% {V2, Q2} = store:dequeue(key1, Q1) -> {val2,[{key1,[val3]},{a,[vala]}]}
%% {V3, Q3} = store:dequeue(key1, Q2) -> {val3,[{key1,[]},{a,[vala]}]}   %% 注意: 此时的key1对应的Val列表为空[]
%% {V4, Q4} = store:dequeue(key1, Q3) -> {not_found,[{a,[vala]},{key1,[]}]}

-spec(dequeue(atom(), [{atom(),[any()]}]) -> {atom(), [{atom(),[any()]}]}) .

dequeue(Key,Queues) ->
    DoDequeue = fun(_F,_K,[],A)               -> {not_found, A};
                   (_F,K,[{K,[VH|VR]} | R], A) -> {VH, [{K, VR} | R] ++ A} ; %% we found the queue
                   (F,K,[{_K,_Vs} = H | R], A) -> F(F,K,R,[H|A])
                end ,
    DoDequeue(DoDequeue,Key,Queues,[]) .


%% @doc
%% 返回Key对应的所有元素, 如果没有则返回空[].
%%
%% 例子:
%% Q = [{key1, [val1, val2, val3]}, {a, [vala]}], %% 插入顺序为val1, val2, val3
%% store:all(key1, Q) -> [val1,val2,val3]

-spec(all(atom(), [{atom(),[any()]}]) -> [any()] | []) .

all(Key,Queues) ->
    DoAll = fun(_F,_K,[],_A)             -> [] ;
               (_F,K,[{K,Vs} | _R], _A)  -> Vs ; %% we found the queue
               (F,K,[H | R], A)          -> F(F,K,R,[H|A])
            end ,
    DoAll(DoAll,Key,Queues,[]) .


%% @doc
%% 从Queue中删除一条记录
%%
%% 例子:
%% Q = [{key1, [val1, val2, val3]}, {a, [vala]}], %% 插入顺序为val1, val2, val3
%% Q = store:delete(key1, val5, Q)   -> [{key1,[val1,val2,val3]},{a,[vala]}]
%% Q1 = store:delete(key1, val1, Q)  -> [{key1,[val2,val3]},{a,[vala]}]
%% Q2 = store:delete(key1, val2, Q1) -> [{key1,[val3]},{a,[vala]}]
%% Q3 = store:delete(key1, val3, Q2) -> [{key1,[]},{a,[vala]}]
%% Q3 = store:delete(key1, val4, Q3) -> [{key1,[]},{a,[vala]}]

-spec(delete(atom(), any(), [{atom(),[any()]}]) -> [{atom(),[any()]}]).

delete(Key,Value, Queues) ->
    DoDelete = fun(_F,_K,[],_A)               -> erlang:error(unknown_queue) ;
                (_F,K,[{K,Vs} | R], A)        -> [{K, lists:delete(Value,Vs)} | R] ++ A ; %% we found the queue
                (F,K,[{_K,_Vs} = H | R], A)   -> F(F,K,R,[H|A])
             end ,
    DoDelete(DoDelete,Key,Queues,[]) .


%% @doc
%% Deletes one value from all the queues if the
%% given predicate P returns true.
delete_if(P, Queues) ->
    Pp = fun(X) -> not(P(X)) end,
    DoDeleteP = fun(_F,_Pr,[],A)            -> A ;
                   (F,Pr,[{K,Vs} | R], A)   -> VsP = lists:filter(Pr,Vs),
                                               F(F,P,R,[{K,VsP} | A])
                end ,
    DoDeleteP(DoDeleteP, Pp, Queues, []) .

%% @doc
%% Deletes one value from all the queues if the
%% given predicate P returns true.
dequeue_if(P, Queues) ->
    Pp = fun(X) -> not(P(X)) end,
    DoDeleteP = fun(_F, [], A) ->
                        A ;

                   (F, [{K,Vs} | R], {V, A})   ->
                        case lists_extensions:detect(P,Vs) of
                            {ok, Vp}          ->  VsP = lists:filter(Pp,Vs),
                                                  F(F, R, {Vp,[{K,VsP}|A]}) ;
                            {error,not_found} -> F(F, R, {V, [{K,Vs}|A]})
                        end

                end ,
    DoDeleteP(DoDeleteP, Queues, {not_found,[]}) .


%% @doc
%% Returns one value from all the queues if the
%% given predicate P returns true.
detect_if(P, Queues) ->
    DoDetect = fun(_F, []) ->
                        not_found ;

                   (F, [{_K,Vs} | R])   ->
                        case lists_extensions:detect(P,Vs) of
                            {ok, Vp}          ->  Vp ;
                            {error,not_found} -> F(F, R)
                        end

                end ,
    DoDetect(DoDetect, Queues) .


%% @doc
%% applies predicate P to values of the queues.
%% The predicate P must returns {false, Vp} with the new value of V if
%% the execution must go on in the list or {true, Vp} with the new
%% value Vp for V if the execution must stop and Vp to be returned
%% altogether with the updated list.
-spec(alter(any(), [{atom(),[any()]}]) -> {any(), [{atom(),[any()]}]}).

alter(P, Queues) ->
     DoAlter = fun(_F,[],A) -> A ;
                 (F,[{K,Vs}|Cdr],{not_found,A}) ->
                       case lists_extensions:update_and_detect(P,Vs) of
                          {not_found, _Vsp} -> F(F,Cdr,{not_found, [{K,Vs},A]}) ;
                          {Vp, Vsp}         -> {Vp, lists:reverse(A) ++ [{K, Vsp} | Cdr]}
                      end
              end,
    DoAlter(DoAlter,Queues,{not_found,[]}) .


%% @doc
%% Deletes Value from all the Queues.
-spec(delete_from_all(atom(), [{atom(),[any()]}]) -> [{atom(),[any()]}]).

delete_from_all(Value,Queues) ->
    lists:map(fun({K,Vs}) -> {K,lists:delete(Value,Vs)} end, Queues) .


%% tests

%% Q = store:insert(key1, val11, []) -> [{key1,[val11]}]
%% Q1 = store:insert(key1, val11, Q) -> [{key1,[val11,val11]}]
%% Q2 = store:insert(key1, val12, Q1) -> [{key1,[val11,val11,val12]}]
%% Q3 = store:insert(key2, val21, Q2) -> [{key2,[val21]},{key1,[val11,val11,val12]}]
insert_test() ->
    Queue = insert(key1,val11,[]),
    Queue1 = insert(key1,val11, Queue),
    Queue2 = store:insert(key1, val12, Queue1),
    Queue3 = store:insert(key2, val21, Queue2),
    ?assertEqual([{key1,[val11]}], Queue),
    ?assertEqual([{key1,[val11,val11]}], Queue1),
    ?assertEqual([{key1,[val11,val11,val12]}], Queue2),
    ?assertEqual([{key2,[val21]},{key1,[val11,val11,val12]}], Queue3).


%% Q = [{key1, [val1, val2, val3]}, {a, [vala]}]. %% 插入顺序为val1, val2, val3
%% {V1, Q1} = store:next(key1, Q)  -> {val1,[{key1,[val2,val3,val1]},{a,[vala]}]}
%% {V2, Q2} = store:next(key1, Q1) -> {val2,[{key1,[val3,val1,val2]},{a,[vala]}]}
%% {V3, Q3} = store:next(key1, Q2) -> {val3,[{key1,[val1,val2,val3]},{a,[vala]}]}
%% {V4, Q4} = store:next(key1, Q3) -> {val1,[{key1,[val2,val3,val1]},{a,[vala]}]}
next_test() ->
    Q =  [{key1, [val1, val2, val3]}, {a, [vala]}],
    {V1, Q1} = store:next(key1, Q),
    {V2, Q2} = store:next(key1, Q1), 
    {V3, Q3} = store:next(key1, Q2),
    {V4, _Q4} = store:next(key1, Q3),
    ?assertEqual(val1, V1),
    ?assertEqual(val2, V2),
    ?assertEqual(val3, V3),
    ?assertEqual(val1, V4).

%% Q = [{key1, [val1, val2, val3]}, {a, [vala]}]. %% 插入顺序为val1, val2, val3
%% {V1, Q1} = store:dequeue(key1, Q)  -> {val1,[{key1,[val2,val3]},{a,[vala]}]}
%% {V2, Q2} = store:dequeue(key1, Q1) -> {val2,[{key1,[val3]},{a,[vala]}]}
%% {V3, Q3} = store:dequeue(key1, Q2) -> {val3,[{key1,[]},{a,[vala]}]}   %% 注意: 此时的key1对应的Val列表为空[]
%% {V4, Q4} = store:dequeue(key1, Q3) -> {not_found,[{a,[vala]},{key1,[]}]}
dqueue_test() ->
    Q =  [{key1, [val1, val2, val3]}, {a, [vala]}],
    {V1, Q1} = store:dequeue(key1, Q),
    {V2, Q2} = store:dequeue(key1, Q1), 
    {V3, Q3} = store:dequeue(key1, Q2),
    {V4, _Q4} = store:dequeue(key1, Q3),
    ?assertEqual(val1, V1),
    ?assertEqual(val2, V2),
    ?assertEqual(val3, V3),
    ?assertEqual(not_found, V4).


%% Q = [{key1, [val1, val2, val3]}, {a, [vala]}], %% 插入顺序为val1, val2, val3
%% store:all(key1, Q) -> [val1,val2,val3]
all_test() ->
    Q = [{key1, [val1, val2, val3]}, {a, [vala]}], %% 插入顺序为val1, val2, val3
    ?assertEqual(store:all(key1, Q), [val1,val2,val3]).

%% Q = [{key1, [val1, val2, val3]}, {a, [vala]}], %% 插入顺序为val1, val2, val3
%% Q = store:delete(key1, val5, Q)   -> [{key1,[val1,val2,val3]},{a,[vala]}]
%% Q1 = store:delete(key1, val1, Q)  -> [{key1,[val2,val3]},{a,[vala]}]
%% Q2 = store:delete(key1, val2, Q1) -> [{key1,[val3]},{a,[vala]}]
%% Q3 = store:delete(key1, val3, Q2) -> [{key1,[]},{a,[vala]}]
%% Q3 = store:delete(key1, val4, Q3) -> [{key1,[]},{a,[vala]}]
delete_test() ->
    Q = [{key1, [val1, val2, val3]}, {a, [vala]}], %% 插入顺序为val1, val2, val3
    Q = store:delete(key1, val5, Q), 
    Q1 = store:delete(key1, val1, Q), 
    Q2 = store:delete(key1, val2, Q1), 
    Q3 = store:delete(key1, val3, Q2),
    Q3 = store:delete(key1, val4, Q3),
    ?assertEqual(Q1, [{key1,[val2,val3]},{a,[vala]}]),
    ?assertEqual(Q2, [{key1,[val3]},{a,[vala]}]),
    ?assertEqual(Q3, [{key1,[]},{a,[vala]}]).


detect_if_test() ->
    Queue = insert(test,1,[]),
    ?assertEqual(1,length(Queue)),
    QueueB = insert(test,2,Queue),
    Result= detect_if(fun(E) -> E =:= 1 end, QueueB),
    ?assertEqual(1, Result),
    ResultB= detect_if(fun(E) -> E =:= 3 end, QueueB),
    ?assertEqual(not_found, ResultB) .

alter_test() ->
    Queue = insert(test,1,[]),
    ?assertEqual(1,length(Queue)),
    QueueB = insert(test,2,Queue),
    {Result,QueueC} = alter(fun(E) ->
                                    if
                                        E =:= 1 -> {true, E + 1} ;
                                        true    -> {false, E}
                                    end
                            end, QueueB),
    ?assertEqual(2,Result),
    ?assertEqual(1,length(QueueC)),
    {_K,Vs} = lists:nth(1,QueueC),
    ?assertEqual(2,length(Vs)) ,
    ?assertEqual([2,2], Vs) .

deletion_from_all_test() ->
    Queue = insert(test,1,[]),
    QueueB = insert(test,2,Queue),
    QueueC = insert(test_2,1,QueueB),
    QueueD = delete_from_all(1,QueueC),
    lists:foreach(fun({_K,V}) ->
                          ?assertEqual(false, lists:any(fun(Vp) ->
                                                                Vp =:= 1
                                                        end, V))
                  end,QueueD) .

dequeue_if_test() ->
    Queues = [{a,[1,2,3,4]}, {b,[1,2,3]}, {c,[5,6,7]}],
    {Val,Queuesp} = dequeue_if(fun(X) -> X=:=2 end, Queues),
    ?assertEqual(2,Val),
    ?assertEqual(3, length(Queuesp)),
    lists:foreach(fun({_Id,Es}) ->
                          ?assertEqual(false, lists:any(fun(E) -> E=:=2 end, Es))
                  end, Queuesp) .
