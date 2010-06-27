-module(unit_test).
-export([test_all/0]).

-define(PRINT(X, Y), io:format(X, Y)).

test_all() ->
    crypto:start(),

    test_vclock(),
    test_chash(),
    test_merkle().

test_vclock() ->
    A = vclock:fresh(),
    B = vclock:fresh(),
    A1 = vclock:increment(a, A),
    B1 = vclock:increment(b, B),

    true = vclock:descends(A1,A),
    true = vclock:descends(B1,B),
    false = vclock:descends(A1,B1),
    A2 = vclock:increment(a, A1),

    C = vclock:merge([A2, B1]),
    C1 = vclock:increment(c, C),
    true = vclock:descends(C1, A2),
    true = vclock:descends(C1, B1),
    false = vclock:descends(B1, C1),
    false = vclock:descends(B1, A1),
    ?PRINT("================ test_vclock passed! ================~n", []),    		
    ok.

test_chash() ->
    %% Range = node_range(8),

    %% 要注意, 一个ring的partition的个数一旦确定, 不能修改, 只能update替换节点.
    A0 = chash:fresh(8, node0),
    M0 = chash:members(A0), 
    N0 = chash:nodes(A0),
    assert([node0], M0), 
    assert(8, chash:size(A0)),
    ?PRINT("members: ~p~n", [M0]),
    ?PRINT("nodes: ~n~p~n", [N0]),
    
    Key1 = chash:key_of("this is key1"),
    Key2 = chash:key_of("this is key2"),
    Key3 = chash:key_of("this is key3"),
    S1 = chash:successors(Key1, A0),
    S2 = chash:successors(Key2, A0),
    S3 = chash:successors(Key3, A0),
    ?PRINT("S1: ~p~n~p~n", [hash(Key1), S1]),
    ?PRINT("S2: ~p~n~p~n", [hash(Key2), S2]),
    ?PRINT("S3: ~p~n~p~n", [hash(Key3), S3]),
    ?PRINT("================ test_chash passed! ================~n", []),    		
    ok.

test_merkle() ->
    A = [{one, merkerl:sha("this is value of one")}, 
	 {two, merkerl:sha("this is value of two")}, 
	 {three, merkerl:sha("this is value of three")}, 
	 {four, merkerl:sha("this is value of four")}, 
	 {five, merkerl:sha("this is value of five")}],
    B = [{one, merkerl:sha("this is value of one")}, 
	 {two, merkerl:sha("this is value of two-another")}, %% diff
	 {three, merkerl:sha("this is value of three")}, 
	 {four, merkerl:sha("this is value of four-another")}, %% diff 
	 {five, merkerl:sha("this is value of five")}],

    A1 = merkerl:build_tree(A),
    B1 = merkerl:build_tree(B),
    C1 = merkerl:insert({two, merkerl:sha("this is value of two")}, undefined), 

    assert(merkerl:diff(A1, B1), lists:usort([two, four])),
    assert(merkerl:diff(A1, C1), lists:usort([one, three, four, five])),

    A2 = merkerl:insert({four, merkerl:sha("this is value of four-another")}, A1),
    assert(merkerl:diff(A1, A2), lists:usort([four])),

    A3 = merkerl:insert({five, merkerl:sha("this is value of five-another")}, A2),
    assert(merkerl:diff(A1, A3), lists:usort([four, five])),
    assert(merkerl:diff(A2, A3), lists:usort([five])),	

    A4 = merkerl:delete(five, A2),
    A5 = merkerl:delete(five, A3),
    assert(merkerl:diff(A4, A5), []),

    assert(merkerl:diff(A1, undefined), lists:usort([one, two, three, four, five])),
    ?PRINT("A1#root: ~p~n", [hash(merkerl:root(A1))]),
    ?PRINT("A2#root: ~p~n", [hash(merkerl:root(A2))]),
    ?PRINT("A3#root: ~p~n", [hash(merkerl:root(A3))]),
    ?PRINT("A4#root: ~p~n", [hash(merkerl:root(A4))]),
    ?PRINT("A5#root: ~p~n", [hash(merkerl:root(A5))]),
    ?PRINT("B1#root: ~p~n", [hash(merkerl:root(B1))]),
    ?PRINT("C1#root: ~p~n", [hash(merkerl:root(C1))]),
    ?PRINT("================ test_merkle passed! ================~n", []),    		
    ok.
    
    


%% Internal APIs:
assert(X, X) -> true.

%% @doc gen the identify
%% @return integer()
%% 将一个160bit(20 bytes)的sha binary转换成一个大整数 :)
hash(ShaData) when is_binary(ShaData) ->
    <<Id:160, _/bits>> = ShaData,
    Id.

%% @doc gen the identify
%% @return integer()
%%  将一个二进制数据做md5哈希, 然后将128bit(16 bytes)的md5 binary转换成一个大整数 :)
%% hash(Data) when is_binary(Data) ->
%%     <<Id:128, _/bits>> = erlang:md5(Data),
%%     Id.


%% node_range(NumberofBuckets) ->
%%     trunc(math:pow(2, 160)) div NumberofBuckets.
