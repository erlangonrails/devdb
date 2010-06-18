%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc misc util
-module(e2d_util).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("e2d.hrl").

-export([hash/1]).
-export([bucket_range/1]).
-export([consult_str/1]).
-export([split_list/2]).
-export([any_to_list/1, any_to_binary/1]).

%% @doc gen the identify
-spec hash(Data :: binary()) -> binary().
hash(Data) when is_binary(Data) ->
    <<Id:?KEY_ID_BITS, _/bits>> = erlang:md5(Data),
    Id.

%% @doc consult string
-spec consult_str(Str :: string()) -> {'ok', any()} | {'error', any()}.
consult_str(Str) when is_list(Str) ->
    case erl_scan:string(Str) of
        {ok, Tokens, _} ->
            erl_parse:parse_term(Tokens);
        {error, _EInfo, _Line} ->
            {error, eformat}
    end.


%% @doc find the Val in L, if find the Val in pos N,
%%   then return the split list: {lists:sublist(L, N-1), Val, lists:nthtail(N, L)};
%%   if not find Val, then return {[], Val, L}
-spec split_list(L :: list(), Val :: any()) ->
    {[], any(), list()} | {list(), any(), list()}.
split_list(L, Val) ->
    split_list(L, Val, [], L).

split_list([], Val, _Pre, L) ->
    {[], Val, L};
split_list([Val | Rest], Val, Pre, _L) ->
    {lists:reverse(Pre), Val, Rest};
split_list([H | Rest], Val, Pre, L) ->
    split_list(Rest, Val, [H | Pre], L).


%% @doc the range of per bucket
-spec bucket_range(NumberofBuckets :: pos_integer()) -> pos_integer().
bucket_range(NumberofBuckets) ->
    trunc(math:pow(2, ?KEY_ID_BITS)) div NumberofBuckets.


%% @doc covert any term to list
-spec any_to_list(T :: any()) ->
    list().
any_to_list(T) when is_atom(T) ->
    erlang:atom_to_list(T);
any_to_list(T) when is_integer(T) ->
    erlang:integer_to_list(T);
any_to_list(T) when is_float(T) ->
    erlang:float_to_list(T);
any_to_list(T) when is_list(T) ->
    T;
any_to_list(_T)  ->
    "".

%% @doc covert any term to binary
-spec any_to_binary(T :: any()) ->
    binary().
any_to_binary(T) when is_binary(T) ->
    T;
any_to_binary(T) ->
    iolist_to_binary(any_to_list(T)).

-ifdef(debug).

-endif.
