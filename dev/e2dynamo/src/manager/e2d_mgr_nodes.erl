%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the nodes in ring
-module(e2d_mgr_nodes).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("e2d_manager.hrl").

-export([start_link/0]).
-export([add_node/1, add_node/3]).
-export([get_node/1, get_nodes/0, exclue_node/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc add node
add_node(Node = #node_info{}) ->
    gen_server:call(?SERVER, {add_node, Node}).

add_node(Id, Name, Ip)
    when is_integer(Id) andalso is_atom(Name) andalso is_list(Ip)  ->
    {ok, IpTuple} = inet_parse:address(Ip),
    add_node(#node_info{id = Id, name = Name, ip = IpTuple}).

get_node(Ip) when is_tuple(Ip) ->
    IpStr = inet_parse:ntoa(Ip),
    get_node(IpStr);
get_node(Ip) when is_list(Ip) ->
    gen_server:call(?SERVER, {get_node, Ip}).

get_nodes() ->
    gen_server:call(?SERVER, {get_all}).

exclue_node({FieldName, Value}, Nodes) ->
    Fields = record_info(fields, node_info),
    PreList = lists:takewhile(fun(F) when F =:= FieldName -> false;
                                (_F) -> true
                             end,
                            Fields),
    KeyPos = length(PreList),
    case lists:keytake(Value, KeyPos, Nodes) of
        {value, _, Nodes2} ->
            Nodes2;
        false ->
            Nodes
    end.

%% gen_server callbacks
init(_Args) ->
    {ok, []}.

handle_call({add_node, #node_info{id = Id} = Node}, _From, State) ->
    State2  = lists:keystore(Id, #node_info.id, State, Node),
    {ok, ok, State2};
handle_call({get_node, IP}, _From, State) ->
    Reply =
    case lists:keysearch(IP, #node_info.ip, State) of
        {value, Tuple} ->
            Tuple;
        false ->
            false
    end,
    {ok, Reply, State};
handle_call({get_all}, _From, State) ->
    {ok, State, State};
handle_call(_Req, _From, State) ->
    {ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
