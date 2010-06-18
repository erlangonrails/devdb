%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the tools to ctrl the manager server
-module(e2d_mgr_ctrl).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("e2d.hrl").

-export([handle/0]).

%% @doc handle the ctrl command:
%%   set config
%%      ./manager-ctrl name@manager-server -conf n 1 w 1 r 1 bm 1024
%%   add node
%%      ./manager-ctrl name@manager-server -add id name ip
%%   delete node
%%      ./manager-ctrl name@manager-server -del id
%%   ckeck the node stat
%%      ./manager-ctrl name@manager-server -stat id
%%   check all the nodes's stat
%%      ./manager-ctrl name@manager-server -stat
%% erl -s e2d_mgr_ctrl -e2dmgr-server name@manager-server -e2dmgr-conf n 1 w 1 r 1 bm 1024
handle() ->
    case catch handle_action() of
        ok ->
            ok;
        _ ->
            usage()
    end.

handle_action() ->
    {ok, [Manager]} = init:get_argument('e2dmgr-server'),
    {ok, [Conf]} = init:get_argument('e2dmgr-conf'),
    {ok, [Add]} = init:get_argument('e2dmgr-add'),
    {ok, [Del]} = init:get_argument('e2dmgr-del'),
    {ok, [Stat]} = init:get_argument('e2dmgr-stat'),
    Server = list_to_atom(Manager),
    do_actions(Server, [{conf, Conf}, {add, Add}, {del, Del}, {stat, Stat}]).

usage() ->
    Str =
    "
    Usage:\n
    set config\n
      ./manager-ctrl name@manager-server -conf n 1 w 1 r 1 bm 1024 \n
    add node\n
      ./manager-ctrl name@manager-server -add id name ip \n
    delete node\n
      ./manager-ctrl name@manager-server -del id \n
    ckeck the node stat\n
      ./manager-ctrl name@manager-server -stat id \n
    check all the nodes's stat\n
      ./manager-ctrl name@manager-server -stat\n
    ",
    io:format("~s", [Str]).

do_actions(_Manager, []) ->
    ok;
do_actions(Manager, [Action | Rest]) ->
    do_action(Manager, Action),
    do_actions(Manager, Rest).

%% do the action
do_action(Manager, {conf, Conf}) when is_list(Conf) ->
    Confs = lists:foldl(
        fun
            (E, {Acc, {}, key}) ->
                {Acc, {E, null}, value};
            (E, {Acc, {K, null}, value}) ->
                {[{K, E} | Acc], {}, key}
        end,
        {[], {}, key},
        Conf),
    rpc:call(Manager, e2d_mgr_sysconf, set, [Confs]);
do_action(Manager, {add, Add = [IdStr, NameStr, IpStr]}) when is_list(Add) ->
    Id = list_to_integer(IdStr),
    Name = list_to_atom(NameStr),
    rpc:all(Manager, e2d_mgr_nodes, add_node, [Id, Name, IpStr]).
%do_action(Manager, {stat, Stat}) when is_list(Stat) ->
%    rpc:all(Manager, e2d_mgr_nodes, add_node, [Stat]);
