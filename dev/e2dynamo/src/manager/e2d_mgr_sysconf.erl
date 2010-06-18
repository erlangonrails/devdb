%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc handle the system configure info
-module(e2d_mgr_sysconf).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("e2d_manager.hrl").

-export([start_link/0]).
-export([get/1, get/2]).
-export([set/1, set/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CONFFILE, "e2d_manager.conf").

-spec start_link() -> any().
start_link() ->
    ?Debug("~p start_link ~n", [?MODULE]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get(Par :: [atom()] | atom()) -> [any() | 'undefined'] | any() | 'undefined'.
get(Pars) when is_list(Pars) ->
    gen_server:call(?SERVER, {get_multi, Pars});
get(Par) when is_atom(Par) ->
    gen_server:call(?SERVER, {get, Par}).

-spec get(Par :: atom(), Def :: any()) -> any().
get(Par, Def) when is_atom(Par) ->
    case gen_server:call(?SERVER, {get, Par}) of
        undefined ->
            Def;
        Val ->
            Val
    end.

set(KVs) when is_list(KVs) ->
    lists:foreach(
        fun({Key, Val}) ->
            ?MODULE:set(Key, Val)
        end,
        KVs).

set(Key, Value) ->
    {Key2, Value2} = normalize_kv(string:to_lower(Key), Value),
    gen_server:call(?SERVER, {set, Key2, Value2}).

%% gen_server callbacks
init(_Args) ->
    case filelib:is_file(?CONFFILE) of
        true ->
            case file:consult(?CONFFILE) of
                {ok, [Config]} ->
                    {ok, Config};
                _  ->
                    ?Error("the ~p file format error!~n", [?CONFFILE]),
                    {error, e_conf_format}
            end;
        false ->
            ?Warn("not specify the ~p config file~n", [?CONFFILE]),
            {error, e_conf}
    end.

handle_call({get, Par}, _From, State) ->
    Reply = proplists:get_value(Par, State),
    {reply, Reply, State};
handle_call({get_multi, Pars}, _From, State) ->
    Reply =
    lists:map(fun(Key) ->
                      proplists:get_value(Key, State)
              end,
              Pars),
    {reply, Reply, State};
handle_call({set, Key, Value}, _From, State) ->
    State2 = lists:keyreplace(Key, 1, State, {Key, Value}),
    {reply, ok, State2};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%%
normalize_kv([Q], Value) when Q =:= $n orelse Q =:= $R orelse Q =:= $w ->
    {list_to_atom([Q]), list_to_integer(Value)};
normalize_kv("cookie", Value) ->
    {cookie, list_to_atom(Value)};
normalize_kv("bn", Value) ->    % buckets_number
    {buckets_number, list_to_integer(Value)}.
