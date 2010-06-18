%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the e2d manager application and supervisor callback
-module(e2d_mgr).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("e2d.hrl").
-behaviour(application).
-behaviour(supervisor).

-export([start/0, start/2, stop/1]).
-export([init/1]).

%% @doc start the application from the erl shell
-spec start() ->
    'ok' | {'error', atom()}.
start() ->
    application:start(e2d_mgr).

%% @doc the application start callback
start(_Type, _Args) ->
    ?Debug("start application~n", []),
    ensure_apps(),
    supervisor:start_link({local, e2d_mgr_sup}, ?MODULE, []).


%% @doc the application  stop callback
stop(_State) ->
    ok.

%% @doc supervisor callback
init(_Args) ->
    ?Debug("init supervisor~n", []),
    Stragegy = {one_for_one, 10, 10},
    ModHttpd = {e2d_mgr_httpd, {e2d_mgr_httpd, start_link, []},
                permanent, 1000, worker, [e2d_mgr_httpd]},
    ModNode = {e2d_mgr_nodes, {e2d_mgr_nodes, start_link, []},
                permanent, 1000, worker, [e2d_mgr_nodes]},
    ModConf = {e2d_mgr_sysconf, {e2d_mgr_sysconf, start_link, []},
                permanent, 1000, worker, [e2d_mgr_sysconf]},

    {ok, {Stragegy, [ModHttpd,
                    ModNode,
                    ModConf
                    ]}
    }.


%% first ensure some apps must start
ensure_apps() ->
    application:start(sasl),
    inets:start(),
    crypto:start(),
    ok.
