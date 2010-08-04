-module(ememcached_storage).

%% ememcached后端存储引擎, 目前只支持Mnesia存储数据, 这个存储引擎是可以扩展的, 
%% 可以扩展成其它的可以高效读取的存储方式.
%%
%% Mnesia实现:
%% 1. 内部使用名字是ememcached的disc_copies的mnesia表存储所有缓存数据.
%% 
%% 2. Key & Value的内部存储方式
%%    a. Key可以是list或者binary, 最终都会转换成binary存储.
%%    b. Value必须是list的形式, 最终也会被转换成bianry存储.
%%       Value有一种特殊情况:
%%       就是当记录自增/自减的原子操作的时候, Value以integer()的形式存储 

-export([init/1,
	 store_item/2,
	 has_item/2,
	 get_item/2,
	 delete_item/2,
	 update_item_value/4,
	 flush_items/1]).

-include("ememcached.hrl").

%% 在ememcached.hrl中定义的storage_command
%% -record(storage_command, {key, flags, exptime, bytes, data=""}).
-record(ememcached, {key, flags, exptime, data}).

%%====================================================================
%% API functions
%%====================================================================

init(mnesia) ->
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:create_table(ememcached,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, ememcached)}]).

%%
%% @doc 
%% 插入一条记录到ememcached中.
%%
-spec(store_item(atom(), #storage_command{}) -> ok | {error, any()} | any()).
store_item(mnesia, #storage_command{key=Key} = Command) when is_list(Key) ->
    store_item(mnesia, Command#storage_command{key=list_to_binary(Key)});
store_item(mnesia, #storage_command{key=Key, flags=Flags, exptime=Exptime, data=Data})
  when Exptime > 60*60*24*30 -> % (Exptime > 30 days), it is an absolute Unix time
    case mnesia:dirty_write(
	   #ememcached{key=Key,
		      flags=Flags,
		      exptime=Exptime,
                      %% Note: Data must be a list(), or this call will crash!!
		      data=list_to_binary(Data)}) of  
	{'EXIT', Reason} ->
	    {error, Reason};
	 Other ->
	    Other
    end;
store_item(mnesia, #storage_command{key=Key, flags=Flags, exptime=Exptime, data=Data})
  when Exptime =:= 0 -> %% 永远不会过期
    case mnesia:dirty_write(
	   #ememcached{key=Key,
		      flags=Flags,
		      exptime=Exptime,
                      %% Note: Data must be a list(), or this call will crash!!
		      data=list_to_binary(Data)}) of  
	{'EXIT', Reason} ->
	    {error, Reason};
	 Other ->
	    Other
    end;
store_item(mnesia, #storage_command{exptime=Exptime} = StorageCommand) ->
    % Exptime is an offset
    {MegaSecs, Secs, _MicroSecs} = now(),
    store_item(mnesia, StorageCommand#storage_command{exptime=Exptime + (MegaSecs*1000000+Secs)}).
%%
%% @doc
%% 判断一条记录是否存在
%%
-spec(has_item(atom(), list() | binary()) -> true | false).
has_item(Storage, Key) when is_list(Key) ->
    has_item(Storage, list_to_binary(Key));
has_item(Storage, Key) ->
    case get_item(Storage, Key) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

%%
%% @doc
%% 根据Key查找元素.
%% 如果数据存在则返回{ok, {Flags, Data}}
%% 没找到或者过期则返回none
%% 发生错误返回{error, Reason}
%%
-spec(get_item(atom(), list() | binary()) -> {ok, {any(), binary()} | none | {error, any()}}).
get_item(mnesia, Key) when is_list(Key) ->
    get_item(mnesia, list_to_binary(Key));
get_item(mnesia, Key) ->
    mnesia_get(Key, value, dirty).

%%
%% @doc
%% 删除元素
%% 成功删除返回ok, 数据不存在或者已经过期则返回none.
%%
-spec(delete_item(atom(), list() | binary()) -> ok | none).
delete_item(mnesia, Key) when is_list(Key) ->
    delete_item(mnesia, list_to_binary(Key));
delete_item(mnesia, Key) ->
    case has_item(mnesia, Key) of
	true ->
	    mnesia:dirty_delete({ememcached, Key});
	false ->
	    none
    end.

%%
%% @doc
%% 更新数据Operation(ItemValue, Value), 通常针对自增/自减操作.
%% 成功则返回{ok, NewValue}, NewValue是 >= 0的整数
%% 如果key对应的数据不存在, 则返回none
%%
-spec(update_item_value(atom(), list() | binary(), any(), fun()) -> {ok, integer()} | none | any()).
update_item_value(mnesia, Key, Value, Operation) when is_list(Key) ->
    update_item_value(mnesia, list_to_binary(Key), Value, Operation);
update_item_value(mnesia, Key, Value, Operation) ->
    F = fun() ->
		case mnesia_get(Key, record, write) of
		    {ok, #ememcached{data = OldValue} = Item} when is_integer(OldValue) ->
			update_value(Item, OldValue, Value, Operation);
                    {ok, #ememcached{data = Data} = Item} when is_binary(Data) ->
                        Data1 = binary_to_list(Data),
			case string:to_integer(Data1) of
			    {OldValue, ""} ->
				update_value(Item, OldValue, Value, Operation);
			    _ ->
				update_value(Item, 0, Value, Operation) %% 如果原有的Value不能转换成integer(), 则当作0处理.
			end;
		    {ok, #ememcached{data = Data} = Item} ->
			case string:to_integer(Data) of
			    {OldValue, ""} ->
				update_value(Item, OldValue, Value, Operation);
			    _ ->
				update_value(Item, 0, Value, Operation)
			end;
		    Other ->
			Other
		end
	end,
    case mnesia:transaction(F) of
	{atomic, Result} ->
	    Result;
	Other ->
	    Other
    end.

%% @doc
%% 把所有的数据都删除
%% 
-spec(flush_items(atom()) -> ok | {error, any()}).
flush_items(mnesia) ->
    case mnesia:clear_table(ememcached) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

%%====================================================================
%% Internal functions for mnesia backend
%%====================================================================

%% @doc
%% 内部的查找数据的实现.
%%
%% 如果Return = value, 则返回{ok, {Flags, Value}}
%% 如果Return = record, 则返回{ok, #ememcached{}}
%% 如果美找到或者数据过期, 返回none
%% 错误返回{error, Reason}
%%
%% LockType可以使dirty | read | write
-spec(mnesia_get(binary(), value | record, dirty | read | write) -> {ok, {any, binary()}} | {ok, #ememcached{}} | none | {error, any()}).
mnesia_get(Key, Return, LockType) ->
    Lock = case LockType of
	       dirty ->
		    % fake lock, will be run outside of a transaction
		    % anyway
		   read;
	       _ ->
		   LockType
	   end,
    F= fun () ->
	       case mnesia:read(ememcached, Key, Lock) of
		   [#ememcached{exptime=0} = Item] ->
		       {ok, item_value(Return, Item)};
		   [#ememcached{exptime=Exptime} = Item] ->
		       {MegaSecs, Secs, _MicroSecs} = now(),
		       case Exptime > MegaSecs*1000000+Secs of
			   true ->
			       {ok, item_value(Return, Item)};
			   false ->
			       mnesia:delete_object(Item),
			       none
		       end;
		   [] ->
		       none;
		   {'EXIT', Reason} ->
		       {error, Reason}
	       end
       end,
    case LockType of
	dirty ->
	    mnesia:async_dirty(F);
	_ ->
	    F()
    end.
item_value(value, #ememcached{flags = Flags, data = Data}) ->
    {Flags, Data};
item_value(record, Item) when is_record(Item, ememcached) ->
    Item.


%%
%% @doc
%% 调用Operation(OldValue, Value)来更新数据, 并把更新后的Value写入到存储中.
%% 如果成功返回{ok, NewValue}
%%
-spec(update_value(#ememcached{}, any(), any(), fun()) -> {ok, integer()} | {error, any()} | any()).
update_value(Item, OldValue, Value, Operation) ->
    NewValue = case Operation(OldValue, Value) of
		   Result when Result < 0 ->
		       0;
		   Result ->
		       Result
	       end,
    case mnesia:write(Item#ememcached{data=NewValue}) of
	{'EXIT', Reason} ->
	    {error, Reason};
	ok ->
	    {ok, NewValue};
	Other ->
	    Other
    end.
