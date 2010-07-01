%%%-------------------------------------------------------------------
%%% File    : youbao_recommend.erl
%%% Author  : liqiang 
%%% Description : 
%%%
%%% Created : 30 Jun 2010 by liqiang
%%%-------------------------------------------------------------------
-module(youbao_recommend).

-include("ejabberd.hrl").

%% API
-export([add_to_index/2, delete_from_index/3, get_recommend/2, get_user_info/2]).


add_to_index(Server, Username) ->
    {UserInfo, KeyList} = gen_index_info(Server, Username),
    lists:foreach(fun(Item) ->
		      ejabberd_redis:set_add(Item, Username)
		  end, KeyList),
    ?DEBUG("add_to_index##username:~p, keylist:~p", [Username, KeyList]),
    {UserInfo, KeyList}.

delete_from_index(_Server, Username, {UserInfo, KeyList}) ->
     lists:foreach(fun(Item) ->
		      ejabberd_redis:set_rm(Item, Username)
                  end, KeyList),
    ?DEBUG("delete_from_index##username:~p, keylist:~p", [Username, KeyList]),
    {UserInfo, KeyList}.

get_recommend(Server, Username) ->
    UserInfo = get_user_info(Server, Username),
    KeyList = gen_index_query_keys(UserInfo),
    RosterUsers = get_roster_users(Server, Username),
    RecUsers = get_recommend_in(lists:reverse(KeyList), RosterUsers ++ [{Username}], []),
    lists:map(fun({Item}) ->
	          Item ++ "@" ++ Server
              end, RecUsers).

get_recommend_in([], _RosterUsers, RecUsers) ->
    RecUsers;
get_recommend_in([Key|Tails], RosterUsers, RecUsers) ->
    NewRecUsers = lists:foldl(fun(_I, Acc) ->
                                  case length(Acc) =:= 3 of
				      true ->
					  Acc;
				      false ->
					  case ejabberd_redis:set_random_member(Key) of
					      null ->
						  Acc;
					      BinTmpUser ->
						  TmpUser = binary_to_list(BinTmpUser),
                                                  case lists:keymember(TmpUser, 1, RosterUsers ++ Acc) of
					              true ->
						          Acc;
					              false ->
						          [{TmpUser} | Acc]
                                                  end
                                          end
                                  end
                              end, RecUsers, lists:seq(1, 6)),
    case length(NewRecUsers) =:= 3 of
	true ->
	    get_recommend_in([], RosterUsers, NewRecUsers);
	false ->
	    get_recommend_in(Tails, RosterUsers, NewRecUsers)
    end.

%% 产生'查询'需要的key列表
%% @return [Key :: string()]
%%
%% 当前算法对应的Key:
%% gen_key({"recommend", undefined, undefined})
%% 230738212701953601834872089506589540759869551080
gen_index_query_keys(undefined) ->
    Key = gen_key({"recommend", undefined, undefined}),
    [Key];
gen_index_query_keys({"", ""}) ->
    Key = gen_key({"recommend", undefined, undefined}),
    [Key];
gen_index_query_keys({"", "男"}) ->
    Key = gen_key({"recommend", undefined, undefined}),
    Key1 = gen_key({"recommend", undefined, "女"}),
    [Key, Key1];
gen_index_query_keys({"", "女"}) ->
    Key = gen_key({"recommend", undefined, undefined}),
    Key1 = gen_key({"recommend", undefined, "男"}),
    [Key, Key1];
gen_index_query_keys({Ctry, ""}) ->
    Key = gen_key({"recommend", undefined, undefined}),
    Key1 = gen_key({"recommend", Ctry, undefined}),
    [Key, Key1];
gen_index_query_keys({Ctry, "男"}) ->
    Key = gen_key({"recommend", undefined, undefined}),
    Key1 = gen_key({"recommend", undefined, "女"}),
    Key2 = gen_key({"recommend", Ctry, "女"}),
    [Key, Key1, Key2];
gen_index_query_keys({Ctry, "女"}) ->
    Key = gen_key({"recommend", undefined, undefined}),
    Key1 = gen_key({"recommend", undefined, "男"}),
    Key2 = gen_key({"recommend", Ctry, "男"}),
    [Key, Key1, Key2];
gen_index_query_keys({_Ctry, _Middle}) ->
    Key = gen_key({"recommend", undefined, undefined}),
    [Key].

%% 产生'构建索引'需要的信息: 用户的城市, 性别, 索引key列表.
%% return {{Ctry, Middle}, [Key :: string()]}
gen_index_info(Server, Username) ->
    case get_user_info(Server, Username) of
        undefined ->
	    Key = gen_key({"recommend", undefined, undefined}),
	    {{"", ""}, [Key]};
	{"", ""} ->
	    Key = gen_key({"recommend", undefined, undefined}),
	    {{"", ""}, [Key]};
	{"", "男"} ->
	    Key = gen_key({"recommend", undefined, undefined}),
	    Key1 = gen_key({"recommend", undefined, "男"}),
	    {{"", "男"}, [Key, Key1]};
	{"", "女"} ->
	    Key = gen_key({"recommend", undefined, undefined}),
	    Key1 = gen_key({"recommend", undefined, "女"}),
	    {{"", "女"}, [Key, Key1]};
	{Ctry, ""} ->
	    Key = gen_key({"recommend", undefined, undefined}),
	    Key1 = gen_key({"recommend", Ctry, undefined}),
	    {{Ctry, ""}, [Key, Key1]};
	{Ctry, "男"} ->
	    Key = gen_key({"recommend", undefined, undefined}),
	    Key1 = gen_key({"recommend", undefined, "男"}),
            Key2 = gen_key({"recommend", Ctry, "男"}),
	    {{Ctry, "男"}, [Key, Key1, Key2]};
	{Ctry, "女"} ->
	    Key = gen_key({"recommend", undefined, undefined}),
	    Key1 = gen_key({"recommend", undefined, "女"}),
            Key2 = gen_key({"recommend", Ctry, "女"}),
	    {{Ctry, "女"}, [Key, Key1, Key2]};
	{_Ctry, _Middle} ->
	    Key = gen_key({"recommend", undefined, undefined}),    
	    {{"", ""}, [Key]}
    end.

%% @spec gen_key(Data :: term()) -> string()
gen_key(Data) ->
    ShaBin = crypto:sha(term_to_binary(Data)),
    <<IndexAsInt:160/integer>> = ShaBin,
    integer_to_list(IndexAsInt).

%% 获取"城市"和"性别"
%% test script:
%% INSERT INTO vcard_search(lusername, lctry, lmiddle) values ('user1','上海','男')
get_user_info(Server, Username) ->
    case ejabberd_odbc:sql_query(Server,["select lctry, lmiddle from vcard_search where lusername='", Username, "'"]) of
	{selected, ["lctry", "lmiddle"], []} -> undefined;
	{selected, ["lctry", "lmiddle"], [{Ctry, Middle}]} -> {Ctry, Middle};
	_ -> undefined
    end.

%% return: [{"user2"}] | []
get_roster_users(Server, Username) ->
    %% {selected,["username","jid","nick","subscription","ask",
    %%            "askmessage","server","subscribe","type"],
    %%           [{"user1","user2@localhost",[],"F","N",[],"N",[],"item"}]}
    case odbc_queries:get_roster(Server, Username) of
	{selected, ["username","jid","nick","subscription","ask",
                    "askmessage","server","subscribe","type"],
	           []} ->
	    [];
	{selected, ["username","jid","nick","subscription","ask",
                    "askmessage","server","subscribe","type"],
	           Rosters} ->
	    lists:map(fun({_, Jid, _, _, _, _ ,_ ,_ ,_}) ->
		         case string:tokens(Jid, "@") of
			     [TmpUser, _] ->
				 {TmpUser};
			     _d ->
				 []
                         end
		      end, Rosters);
	_ ->
	    []
    end.
