-module(proplists_extensions) .

%% @doc
%% 操作proplists的附加函数

-include_lib("eunit/include/eunit.hrl") .

-export([get_value/3]) .


%% @doc
%% 首先尝试从List中获取Key对应的Val, 如果找到则直接返回Val; 
%% 如果没找到, 则尝试从Defaults中返回Key对应的Val.
%% List和Defaults都是proplists()
-spec(get_value(any(), [{any(), any()}], [{any(), any()}]) -> any() | undefined) .

get_value(Key, List, Defaults) ->
    Value = proplists:get_value(Key, List),
    case Value of
        undefined ->  proplists:get_value(Key, Defaults) ;
        _Other    ->  Value
    end .

%% tests
get_value_test() ->
    ?assertEqual(get_value(key1, [], [{key1,valdefault}]), valdefault) .
