-module(lists_extensions).

%% @doc
%%操作lists的附加函数

-include_lib("eunit/include/eunit.hrl") .

-export([compact/1, unique/1, splice/2, eachWithIndex/2, eachWithIndexP/2, interleave/1, mapWithIndex/2, detect/2]) .
-export([index/2, update_and_detect/2]) .

%% @doc
%% 删除lists中所有出现过的undefined
%%
%% 例子:
%% lists_extensions:compact([a, b, undefined, c, undefined, e]) -> [a,b,c,e]
-spec(compact([any()]) -> [any()]) .

compact(List) ->
    FilterList = fun(_,[],Acum) -> lists:reverse(Acum) ;
                    (F,[H|T],Acum) ->
                         case H == undefined of
                             true -> F(F,T,Acum) ;
                             false -> F(F,T,[H | Acum])
                         end
                 end,
    FilterList(FilterList,List,[]).


%% @doc
%% 删除lists中重复的元素, 只保留第一次出现的元素
%%
%% 例子:
%% lists_extensions:unique([a,b,c,d,a,c,d,e,a,b]) -> [a,b,c,d,e]
-spec(unique([any()]) -> [any()]) .

unique(List) ->
    FilterList = fun(_,[],Acum) -> lists:reverse(Acum) ;
                    (F,[H|T],Acum) ->
                         case lists:member(H,Acum) of
                             true -> F(F,T,Acum) ;
                             false -> F(F,T,[H | Acum])
                         end
                 end,
    FilterList(FilterList,List,[]).


%% @doc
%% 将一个lists拆分成最代元素为Max的sublists的集合.
%%
%% 例子:
%% lists_extensions:splice(4, []) -> []
%% lists_extensions:splice(4, [a,b]) -> [[a,b]]
%% lists_extensions:splice(4, [a,b,c,d,e,f,g,h,i]) -> [[a,b,c,d],[e,f,g,h],[i]]

-spec(splice(integer(),[any()]) -> [[any()]]) .

splice(Max,List) ->
    DoSplice = fun(_,_,[],Acum) -> lists:reverse(Acum);
                  (F,M,L,Acum) ->
                       case length(L) < Max of
                           true -> F(F,M,[],[L|Acum]);
                           false -> {H,T} = lists:split(M,L),
                                    F(F,M,T,[H|Acum])
                       end
               end,
    DoSplice(DoSplice,Max,List,[]).


%% @doc
%% 对lists中的每一个元素调用Fun(Index, Item), 返回undefined.
%% Index从0开始.
%%
%% 例子:
%% (打印下面四行内容, 并返回undefined):
%% lists_extensions:eachWithIndex(fun(I, H) -> io:format("~p:~p~n", [I, H]) end, 
%%                                [a,b,c,d]) -> undefined
%% 0:a
%% 1:b
%% 2:c
%% 3:d
-spec(eachWithIndex(fun(), [any()]) -> undefined).
eachWithIndex(Fun,L) ->
    DoPagination= fun(_,_,[]) -> undefined ;
                     (F,I,[H|T]) -> Fun(I,H),
                                    F(F,I+1,T)
                  end,
    DoPagination(DoPagination,0,L).


%% @doc
%% 对lists中的每一个元素调用Fun(Index, Item), 根据Fun/2的返回结果产生新的lists, 返回这个lists.
%%
%% 例子:
%% lists_extensions:mapWithIndex(fun(I, H) -> {I, H} end, 
%%                               [a,b,c,d]) -> [{0,a},{1,b},{2,c},{3,d}]
-spec(mapWithIndex(fun(), [any()]) -> [any()]).
mapWithIndex(Fun,L) ->
    DoPagination= fun(_,_,[],Acum) -> lists:reverse(Acum) ;
                     (F,I,[H|T],Acum) -> F(F,I+1,T,[Fun(I,H) | Acum])
                  end,
    DoPagination(DoPagination,0,L,[]).


%% @doc
%% 功能同eachWithIndex/2一样
%% 不同之处是每一次Fun(Index, Item)都在单独的process中执行.
-spec(eachWithIndexP(fun(), [any()]) -> undefined).
eachWithIndexP(Fun,L) ->
    DoPagination= fun(_,_,[]) -> undefined ;
                     (F,I,[H|T]) -> spawn(fun() -> Fun(I,H) end),
                                    F(F,I+1,T)
                  end,
    DoPagination(DoPagination,0,L).


%% @doc
%% 将多个lists组合成一个lists, 组合的方式是:
%% 依次从子list中选取第N个元素, 分别插入结果list中, 如果当前子list的长度不够N, 则跳过.
%% N从1到最大子list的长度.
%%
%% 例子:
%% lists_extensions:interleave([[a1, a2, a3, a4, a5],[b1, b2], [c1, c2, c3]]) -> [a1,b1,c1,a2,b2,c2,a3,c3,a4,a5]
%% lists_extensions:interleave([[a1],[b1, b2], [c1, c2, c3]]) -> [a1,b1,c1,b2,c2,c3]
-spec(interleave([[any()]]) -> [any()]) .

interleave([]) -> [];
interleave(Lists) ->
    DoInterleave = fun(_F,[[]],Acum,_Level) ->
                           lists:reverse(Acum);
                      (F,[L|T],Acum,Level) ->
                           case length(L) < Level of
                               true -> F(F,[L|T],Acum, Level - 1);
                               false -> case L of
                                            [H|Hs] -> F(F,T ++ [Hs],[H|Acum],Level);
                                            []  -> F(F,T,Acum,Level)
                                        end
                           end
                   end,
    DoInterleave(DoInterleave,Lists,[],length(lists:nth(1,Lists))).


%% @doc
%% 判断一个元素是否满足P(Item) =:= true, 如果存在这样的元素, 则返回{ok, Item}; 
%% 否则返回{error, not_found}
%% 当存在多个满足条件的元素的时候, 只返回第一个元素.
%% P(Item) -> true | false
%%
%% 例子:
%% lists_extensions:detect(fun(X) -> X =:= 2 end, [1,2,3,4]) -> {ok, 2}
%% lists_extensions:detect(fun(X) -> X =:= 8 end, [1,2,3,4]) -> {error, not_found}
-spec(detect(fun(), [any()]) -> {ok, any()} | {error, not_found}).
detect(_P,[]) ->
    {error, not_found} ;

detect(P, [H | R]) ->
    case P(H) of
        true  -> {ok, H} ;
        false -> detect(P,R)
    end .


%% @doc
%% 遍历list, 对每一个元素应用P(Item)函数, 如果返回{true, NewValue}, 则替换该元素为NewValue, 
%% 并且马上返回{NewValue, NewList}; 
%% 如果当前P(Item)返回{false, Value}, 则保持该元素不变(Value)，继续处理下一个元素.
%% P(Item) -> {true, NewValue} | {false, Value}
%%
%% 例子:
%% lists_extensions:update_and_detect(fun(X) -> if X =:= 2 -> {true, here} ;
%%                                                 X =/= 2 -> {false, X}
%%                                              end
%%                                    end, [1,2,3,4,2,6])) -> {here, [1,here,3,4,2,6]}
%%lists_extensions:update_and_detect(fun(X) -> if X =:= 8 -> {true, here} ;
%%                                                X =/= 8 -> {false, X}
%%                                             end
%%                                   end, [1,2,3,4,2,6]) -> {not_found, [1,2,3,4,2,6]}
-spec(update_and_detect(fun(), [any()]) -> {not_found, [any()]} | {any(), [any()]}).
update_and_detect(P, L) ->
    update_and_detect(P, L, {not_found, []}) .

update_and_detect(_P, [], {Any, Acum}) -> {Any, lists:reverse(Acum)}; %% fixed by liqiang, invoke lists:reverse/1
update_and_detect(P, [H | R], {not_found, Acum} ) ->
    case (P(H)) of
       {true,  Hp} -> {Hp , lists:reverse(Acum) ++ [Hp | R]} ;
       {false, Hp} -> update_and_detect(P, R, {not_found, [Hp | Acum]})
    end.


%% @doc
%% 返回一个元素在lists中的索引(index), 索引从1开始, 如果一个元素在lists中出现多次,
%% 只返回第一次出现位置的索引; 如果元素在lists中不存在, 返回not_found.
%%
%% 例子:
%% lists_extensions:index(a, [b,a,c,a]) -> 2
%% lists_extensions:index(a, [b,c,a]) -> 3
%% lists_extensions:index(ab, [b,c,a]) -> not_found
-spec(index(any(), [any()]) -> integer() | not_found) .

index(Element, List)                  -> index(Element, List, 1) .
index(_Element, [], _Index)           -> not_found ;
index(Element, [Element | _T], Index) -> Index ;
index(Element, [_H | T], Index)       -> index(Element, T, (Index + 1)) .


%% tests
compact_test() ->
    ?assertEqual(lists_extensions:compact([a, b, undefined, c, undefined, e]), [a,b,c,e]).

unique_test() ->
    ?assertEqual(lists_extensions:unique([a,b,c,d,a,c,d,e,a,b]), [a,b,c,d,e]).

splice_test() ->
    ?assertEqual(lists_extensions:splice(4, []), []),
    ?assertEqual(lists_extensions:splice(4, [a,b]), [[a,b]]),
    ?assertEqual(lists_extensions:splice(4, [a,b,c,d,e,f,g,h,i]), [[a,b,c,d],[e,f,g,h],[i]]).

mapWithIndex_test() ->
    ?assertEqual(lists_extensions:mapWithIndex(fun(I, H) -> {I, H} end, [a,b,c,d]),
	         [{0,a},{1,b},{2,c},{3,d}]).

interleave_test() ->
    ?assertEqual(lists_extensions:interleave([[a1, a2, a3, a4, a5],[b1, b2], [c1, c2, c3]]), 
                 [a1,b1,c1,a2,b2,c2,a3,c3,a4,a5]),
    ?assertEqual(lists_extensions:interleave([[a1],[b1, b2], [c1, c2, c3]]),
                 [a1,b1,c1,b2,c2,c3]).

detect_test() ->
    ?assertEqual({ok, 2}, lists_extensions:detect(fun(X) -> X =:= 2 end, [1,2,3,4])),
    ?assertEqual({error, not_found}, lists_extensions:detect(fun(X) -> X =:= 8 end, [1,2,3,4])).

update_and_detect_test() ->
    ?assertEqual({here, [1,here,3,4,2,6]},
                 lists_extensions:update_and_detect(fun(X) -> if X =:= 2 -> {true, here} ;
                                                                 X =/= 2 -> {false, X}
                                                              end
                                                    end, [1,2,3,4,2,6])),
    ?assertEqual({not_found, [1,2,3,4,2,6]},
                 lists_extensions:update_and_detect(fun(X) -> if X =:= 8 -> {true, here} ;
                                                                 X =/= 8 -> {false, X}
                                                              end
                                                    end, [1,2,3,4,2,6])).

index_test() ->
    ?assertEqual(lists_extensions:index(test,[1,2,test,4,t]), 3),
    ?assertEqual(lists_extensions:index(test,[1,2]), not_found),
    ?assertEqual(lists_extensions:index(test,[]), not_found),
    ?assertEqual(lists_extensions:index(test,[test]), 1),
    ?assertEqual(lists_extensions:index(test,[test,2]), 1),
    ?assertEqual(lists_extensions:index(test,[1,2,test]), 3).




