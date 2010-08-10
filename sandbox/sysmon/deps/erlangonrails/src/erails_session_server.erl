-module(erails_session_server).
-behaviour(gen_server).

-export([start/0,
	 new_session/1, delete_session/1,
	 get_session_data/1, get_session_value/2, set_session_data/3,remove_session_data/2]).

%% gen_server callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-record(erails_session, {sid,data,datetime,ttl}).
-define(SID_LEN, 16).

%% Erlang On Rails的session管理模块:
%%
%% 一个Session以Sid来标示, 可以根据这个Sid存储任意的{Key, Value}形式的数据, 
%% 也可以根据Key来访问相应的数据.
%%
%% 内部维护一个erails_session的mnesia表, 存储的是#erails_session{}
%% sid: 是key, 是随机产生的GUID, 是这个Session的标示.
%% data: 是一个[{Key, Val}]的proplists.
%% ttl: 当前版本的ttl默认都是0.
%%
%%
%% 例子:
%% Sid = erails_session_server:new_session(undefined) %% 创建一个新的Session 
%% erails_session_server:set_session_data(Sid, "key1", "val1")
%% erails_session_server:set_session_data(Sid, "key2", "val2")
%% erails_session_server:get_session_data(Sid) -> [{"key2","val2"},{"key1","val1"}]
%% erails_session_server:remove_session_data(Sid, "key1")
%% erails_session_server:get_session_data(Sid) -> [{"key2","val2"}]
%% erails_session_server:delete_session(Sid)
%% erails_session_server:get_session_data(Sid) -> []



%% API
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    mnesia:create_table(erails_session,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, erails_session)}]),
    {ok, undefined}.
%%
%% @doc
%% 创建一个新的Session, 或者返回一个已经存在的Session.
%%
%% Sid: 如果Sid是undefined或者在ETS表中不存在, 则产生一个新的随机的SID.
%%      如果Sid在ETS表中已经存在, 则返回这个Sid.
-spec(new_session(string() | undefined) -> string()).
new_session(Sid) ->
    gen_server:call(?MODULE,{new_session, Sid}).

%%
%% @doc
%% 返回Session中存储的数据, 是一个proplists()
-spec(get_session_data(string()) -> [] | [{any(), any()}]).
get_session_data(Sid) ->
    gen_server:call(?MODULE,{get_session_data,Sid}).

%% 
%% @doc
%% 返回Session中某个Key对应的Value
-spec(get_session_value(string(), any()) -> any() | undefined).
get_session_value(Sid, Key) ->
    gen_server:call(?MODULE,{get_session_value,Sid, Key}).

%% @doc
%% 存储数据
-spec(set_session_data(string(), any(), any()) -> ok).
set_session_data(Sid,Key,Value) ->
    gen_server:call(?MODULE,{set_session_data,Sid,Key,Value}).

%% @doc
%% 删除session相关的数据
-spec(delete_session(string()) -> ok).
delete_session(Sid) ->
    gen_server:call(?MODULE,{delete_session,Sid}).

%% @doc
%% 删除session中Key对应的数据
-spec(remove_session_data(string(), any()) -> ok).
remove_session_data(Sid,Key) ->
    gen_server:call(?MODULE,{remove_session_data,Sid,Key}).


%%% Callbacks
handle_call({new_session, SData}, _From, _State) ->
    NewId = case SData of
		undefined ->
		    Sid = erails_guid:get_with_datetime(?SID_LEN),
                    Session = #erails_session{sid=Sid,data=[],datetime=erails_datetime:get(),ttl=0},
	            mnesia:dirty_write(Session),
                    Sid;
		Any ->
		    case mnesia:dirty_match_object(erails_session, {erails_session, Any, '_', '_', '_'}) of
			[{erails_session, Any, _, _, _}] ->
			    Any;
			[] -> 
			    Sid = erails_guid:get_with_datetime(?SID_LEN),
                            Session = #erails_session{sid=Sid,data=[],datetime=erails_datetime:get(),ttl=0},
                            mnesia:dirty_write(Session),
                            Sid
		    end
	    end,
    {reply,NewId,undefined};

handle_call({get_session_data,Sid},_From,_State) ->
    Data = case mnesia:dirty_match_object(erails_session, {erails_session, Sid, '_', '_', '_'}) of
	       [{erails_session, Sid, SessionData, _, _}] ->
		   SessionData;
	       [] ->
		   []
	   end,
    {reply,Data,undefined};

handle_call({get_session_value,Sid,Key},_From,_State) ->
    Data = case mnesia:dirty_match_object(erails_session, {erails_session, Sid, '_', '_', '_'}) of
	       [{erails_session, Sid, SessionData, _, _}] ->
		   SessionData;
	       [] ->
		   []
	   end,
    Value = proplists:get_value(Key, Data),
    {reply,Value,undefined};

handle_call({set_session_data,Sid,Key,Value},_From,_State) ->
    Data = case mnesia:dirty_match_object(erails_session, {erails_session, Sid, '_', '_', '_'}) of
	       [{erails_session, Sid, SessionData, _, _}] ->
		   SessionData;
	       [] -> []
	   end,
    Data1 = case proplists:is_defined(Key,Data) of
		true ->
		    Rest = proplists:delete(Key,Data),
		    [{Key,Value}|Rest];
		false ->
		    [{Key,Value}|Data]
	    end,
    
    mnesia:dirty_write(#erails_session{sid=Sid,data=Data1,datetime=erails_datetime:get(), ttl=0}),

    {reply,ok,undefined};


handle_call({delete_session,Sid},_From,_State) ->
    mnesia:dirty_delete(erails_session,Sid),
    {reply,ok,undefined};


handle_call({remove_session_data,Sid,Key},_From,_State) ->
    Data = case mnesia:dirty_match_object(erails_session, {erails_session, Sid, '_', '_', '_'}) of
	       [{erails_session, Sid, SessionData,_, _}] ->
		   SessionData;
	       [] -> []
	   end,
    Data1 = case proplists:is_defined(Key,Data) of
		true ->
		    proplists:delete(Key,Data);
		false ->
		    Data
	    end,
    
    mnesia:dirty_write(#erails_session{sid=Sid,data=Data1,datetime=erails_datetime:get(),ttl=0}),

    {reply,ok,undefined}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

