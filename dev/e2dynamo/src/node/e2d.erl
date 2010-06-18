%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the e2d application and supervisor
-module(e2d).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("e2d.hrl").
-behaviour(application).
-behaviour(supervisor).

-export([start/0, start/2, stop/1]).
-export([init/1]).

-define(E2dynamo_Conf, "./conf/e2dynamo.conf").

%% @doc start the application from the erl shell
-spec start() ->
    'ok' | {'error', atom()}.
start() ->
    application:start(e2d_node).

%% @doc the application start callback
start(_Type, _Args) ->
    ?Debug("start application~n", []),
    ensure_apps(),

    ?Debug("load the config info~n", []),
    {ok, Config} =
    case catch load_config() of
        {ok, Config0} ->
            {ok, Config0};
        {'EXIT', R} ->
            ?Error("load config error:~p~n", [R]),
            exit(R)
    end,

    ?Debug("check the config ~p~n", [Config]),
    NameC = proplists:get_value(name, Config, none),
    case node() of
        'nonode@nohost' ->
            Ret = net_kernel:start([NameC, longnames]),
            ?Debug("start node:~p ret:~p~n", [NameC, Ret]);
        NameC ->
            ok;
        _ ->
            ?Error("node() mismatch with name from config :~w", [NameC]),
            exit(bad_config)
    end,

    true = erlang:is_alive(),

    ?Debug("start the supervisor e2d_sup~n", []),
    Sup = supervisor:start_link({local, e2d_sup}, ?MODULE, [Config]),

    ?Debug("connect nodes...~n", []),
    connect_nodes(),

    ?Debug("sync data...~n", []),
    sync_data(),

    ?Debug("application start complete~n", []),
    Sup.


%% @doc the application  stop callback
stop(_State) ->
    ok.

%% @doc supervisor callback
init([Config]) ->
    ?Debug("init supervisor~n", []),
    {ok, HostName} = inet:gethostname(),
    {ok, Ip} = inet:getaddr(HostName, inet),
    E2dNode =
    #e2dnode{id = proplists:get_value(id, Config),
            name = node(),
            ip = proplists:get_value(ip, Config, Ip),
            port = proplists:get_value(port, Config, 0),
            capacity = proplists:get_value(capacity, Config, 0)
            },

    HttpHandler = proplists:get_value(mod_http_handler, Config, e2d_httpd),

    Stragegy = {one_for_one, 10, 10},

    ModConfig = {e2d_config, {e2d_config, start_link, [Config]},
                transient, 1000, worker, [e2d_config]},
    ModNode = {e2d_node, {e2d_node, start_link, [E2dNode]},
                transient, 1000, worker, [e2d_node]},
    ModMember = {e2d_membership, {e2d_membership, start_link, []},
                transient, 1000, worker, [e2d_membership]},
    ModServer = {e2d_server, {e2d_server, start_link, []},
                transient, 1000, worker, [e2d_server]},
    ModStore = {e2d_store, {e2d_store, start_link, []},
                transient, 1000, worker, [e2d_store]},
    ModHttpd = {e2d_c_sup, {HttpHandler, start_link, [Config]},
                permanent, 1000, worker, [HttpHandler]},
    ModNodeSup = {e2d_node_sup, {e2d_node_sup, start_link, []},
                permanent, 1000, worker, [e2d_node_sup]},

    {ok, {Stragegy, [ModConfig,
                    ModNode,
                    ModMember,
                    ModServer,
                    ModStore,
                    ModHttpd,
                    ModNodeSup
                    ]}
    }.

%%
%% internal API
%%

load_config() ->
    {Url, PreConf, Conf} = get_env_conf(),
    {ok, Config1} = e2d_config:get_ms_config(Url, PreConf),
    {ok, Config2} = e2d_config:get_e2d_config(Conf),
    {ok, Config1 ++ Config2}.


%% first ensure some apps must start
ensure_apps() ->
    application:start(sasl),
    inets:start(),
    crypto:start(),
    ok.

%% get config info from env
get_env_conf() ->
    Url =
    case application:get_env(config_url) of
        {ok, Val} ->
            Val;
        undefined ->
            []
    end,

    PreFile =
    case application:get_env(config_file) of
        {ok, Val2} ->
            Val2;
        undefined ->
            []
    end,

    Conf =
    case application:get_env(local_conf) of
        {ok, Val3} ->
            Val3;
        undefined ->
            ?E2dynamo_Conf
    end,

    case {Url, PreFile} of
        {[], []} ->
            ?Error("agrument error, must specify config url or local file~n", []),
            throw({econfig, "please specify the config url or local file"});
        _ ->
            {Url, PreFile, Conf}
    end.


%% connect the other nodes
connect_nodes() ->
    % first set the cookie
    Cookie = e2d_config:get(cookie),
    ?Debug("Cookie:~p~n", [Cookie]),
    erlang:set_cookie(node(), Cookie),
    % connect the other nodes
    Nodes = e2d_config:get(nodes),
    case lists:member(node(), Nodes) of
        false ->
            ?Error("this node is not in the node list~n", []),
            exit(econfig);
        true ->
            lists:map(
              fun(Node) ->
                      case net_kernel:connect_node(Node) of
                          false ->
                              ?Warn("connect node :~w error!~n", [Node]),
                              {Node, false};
                          true ->
                  e2d_membership:update_node({add, Node}),
                              {Node, true}
                      end
              end,
              Nodes -- [node()])
    end,
    ok.

%% sync data
sync_data() ->
    catch e2d_sync:update_buckets().
