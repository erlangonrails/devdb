%% @doc the client library app and supervisor callback
-module(e2d_c).
-include("e2d.hrl").

-behaviour(application).
-behaviour(supervisor).

-export([start/2, start_phase/3, stop/1]).
-export([init/1]).

%% @doc the application start callback
start(_Type, _Args) ->
    supervisor:start_link({local, e2d_c_sup}, ?MODULE, []).

%% @doc start the application in phase
start_phase(go, _Type, _Args) ->
    ok.

%% @doc callback when application has stopped
stop(_State) ->
    ok.

%% @doc supervisor init callback
init(_Args) ->
    Stragegy = {one_for_one, 1,  10},
    Http = {e2d_c_httpd, {e2d_c_httpd, start_link, []},
            permanent, 2000, worker, [e2d_c_httpd]},
    {ok, {Stragegy, [Http]}}.
