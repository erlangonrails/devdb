%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the sysmon application.

-module(sysmon_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2, stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for sysmon.
start(_Type, _StartArgs) ->
    sysmon_deps:ensure(),
    sysmon_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for sysmon.
stop(_State) ->
    ok.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
