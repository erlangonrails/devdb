-module(ememcached_storage).

% Handle all storage mechanisms (only mnesia for now).

-export([init/1,
	 store_item/2,
	 has_item/2,
	 get_item/2,
	 delete_item/2,
	 update_item_value/4,
	 flush_items/1]).

-include("ememcached.hrl").

-record(ememcached, {key, flags, exptime, data}).

%%====================================================================
%% API functions
%%====================================================================

init(mnesia) ->
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:create_table(ememcached,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, ememcached)}]).

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

store_item(mnesia, #storage_command{exptime=Exptime} = StorageCommand) ->
    % Exptime is an offset
    {MegaSecs, Secs, _MicroSecs} = now(),
    store_item(mnesia, StorageCommand#storage_command{exptime=Exptime + (MegaSecs*1000+Secs)}).

% return true if an item with Key is present
has_item(Storage, Key) when is_list(Key) ->
    has_item(Storage, list_to_binary(Key));
has_item(Storage, Key) ->
    case get_item(Storage, Key) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

% Find the item with key Key, return
% {ok, {Flags, Data}}
% if found or
% none
% if not found or if the item has expired
% (in the later case, delete the item) or
% {error, Reason}
get_item(mnesia, Key) when is_list(Key) ->
    get_item(mnesia, list_to_binary(Key));
get_item(mnesia, Key) ->
    mnesia_get(Key, value, dirty).

delete_item(mnesia, Key) when is_list(Key) ->
    delete_item(mnesia, list_to_binary(Key));
delete_item(mnesia, Key) ->
    case has_item(mnesia, Key) of
	true ->
	    mnesia:dirty_delete({ememcached, Key});
	false ->
	    none
    end.

% Update the value of item with the result of
% Operation(ItemValue, Value)
% result value will be >=0
% return {ok, Value} if ok
update_item_value(mnesia, Key, Value, Operation) when is_list(Key) ->
    update_item_value(mnesia, list_to_binary(Key), Value, Operation);
update_item_value(mnesia, Key, Value, Operation) ->
    F = fun() ->
                %% Note: 
                %% OldData should be a integer() or a string which can convert to integer(), or
                %% it will 'CONVERT' to 0.
		case mnesia_get(Key, record, write) of
		    {ok, #ememcached{data = OldValue} = Item}
		    when is_integer(OldValue) ->
			update_value(Item, OldValue, Value, Operation);
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

% if Return == value, return
% {ok, {Flags, Value}}
% if Return == record, return
% {ok, #ememcached}
% or none if not found
% LockType is dirty, read or write
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
		       case Exptime > MegaSecs*1000+Secs of
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


% Write the updated item.
% return {ok, Value} if ok
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
