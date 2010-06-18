%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the data store module use mnesia
-module(e2d_store_mnesia).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("e2d.hrl").

%% the data in db
-record(data, {
          key = 0 :: key(),
          context = [] :: any(),
          value = <<>> :: value()
         }).

%% the buckets
-record(bucket, {
          key = 0 :: bucket_id(),
          size = 0 :: non_neg_integer()
          }).


% the store module must have these functions
-export([buckets/0, keys/2]).
-export([get/1, put/3, del/1]).

% the mnesia util tools
-export([status/0, init/0, is_db_empty/0]).
-export([table_names/0]).
-export([create_tables/0]).


%% @doc get all the buckets id the local node manager
-spec buckets() -> [bucket_id()].
buckets() ->
    ?Debug("get all buckets, Mnesia info:~p", [mnesia:system_info(all)]),
    mnesia:dirty_all_keys(bucket).
    %mnesia:all_keys(bucket).

%% @doc get the keys in specify range
-spec keys(Begin :: key(), End :: key()) -> [key()].
keys(Begin, End) ->
        MatchHead = {'$1', '_'},
        Cond = [{'and',
                        {'>', '$1', Begin},
                        {'<', '$1', End}}],
        Result = ['$1'],
        mnesia:select(data, [{MatchHead, Cond, Result}]).

-spec get(Key :: key()) ->
    {'ok', {vclock(), value()}} | {'ok', 'key_not_exist'} | {'error', any()}.
get(Key) ->
    case catch mnesia:dirty_read({data, Key}) of
        [#data{value = Value, context = Context}] ->
            {ok, {Context, Value}};
        [] ->
            {error, key_not_exist};
        {'EXIT', {aborted, _Reason}} ->
            {error, aborted}
    end.

-spec put(Key :: key(), Context :: vclock(), Value::value()) ->
    {'ok', 'success'} | {'error', any()}.
put(Key, Context, Value) ->
    F = fun() ->
                mnesia:write(#data{key = Key, context = Context, value = Value})
        end,
    case mnesia:transaction(F) of
        {atomic, _} ->
            {ok, success};
        {aborted, R} ->
        ?Debug("aborted reason:~p~n", [R]),
            {error, aborted}
    end.

-spec del(Key :: key()) ->
    {'ok', 'success'} | {'error', any()}.
del(Key) ->
    F = fun() ->
                mnesia:delete({data, Key})
        end,
    case mnesia:transaction(F) of
        {atomic, _} ->
            {ok, success};
        {aborted, _} ->
            {error, aborted}
    end.


status() ->
    ok.

%% create the mneisa database and table
init() ->
    case mnesia:system_info(extra_db_nodes) of
        [] ->
            mnesia:create_schema([node()]);
        _ ->
            ok
    end,

    mnesia:start(),
    Tables = mnesia:system_info(tables),
    ?Debug("local tables:~p~n", [Tables]),
    case lists:member(data, Tables) of
        true ->
            ok;
        false ->
            ?Debug("create tables~n", []),
            case catch create_tables() of
                ok ->
                    ?Debug("create table ok~n", []);
                E ->
                    ?Debug("Create table error ~p ~n", [E])
            end
    end,

    ?Debug("wait tables~n", []),
    ok = wait_for_tables(),
    ?Debug("~p init complete~n", [?MODULE]),
    ok.

is_db_empty() ->
    lists:all(fun (Tab) -> mnesia:dirty_first(Tab) == '$end_of_table' end,
              table_names()).

table_definitions() ->
    [{data, [{disc_copies, [node()]},
             {attributes, record_info(fields, data)}]},
     {bucket, [{disc_copies, [node()]},
                 {attributes, record_info(fields, bucket)}]}
     ].

table_names() ->
    [Tab || {Tab, _} <- table_definitions()].

create_tables() ->
    ?Debug("create tables:~p~n", [table_definitions()]),
    ?Debug("node:~p~n", [node()]),
    lists:foreach(fun ({Tab, TabArgs}) ->
                          case mnesia:create_table(Tab, TabArgs) of
                              {atomic, ok} -> ok;
                              {aborted, Reason} ->
                                  throw({error, {table_creation_failed,
                                                 Tab, TabArgs, Reason}})
                          end
                  end,
                  table_definitions()),
    ok.

wait_for_tables() ->
    case mnesia:wait_for_tables(table_names(), 30000) of
        ok -> ok;
        {timeout, BadTabs} ->
            throw({error, {timeout_waiting_for_tables, BadTabs}});
        {error, Reason} ->
            throw({error, {failed_waiting_for_tables, Reason}})
    end.
