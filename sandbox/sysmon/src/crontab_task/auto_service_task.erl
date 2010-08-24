-module(auto_service_task).
-include("../client_api/sysmon_client_api_config.hrl").
-include("../sysmon.hrl").
-export([run/2]).

-spec run({App :: atom(), TryCount :: integer()}, DepList :: [{atom(), integer()}]) -> boolean().
run({App, TryCount}, DepList) ->
    IsAlive = sysmon_client_util:check_alive(App),
    case IsAlive of
	true ->
            put_auto_service(App, true, "service is running"),
            true;
        false ->
            put_auto_service(App, false, "service is stopped"),
            case check_deps(DepList) of
	        false ->
                    false;
                true ->
                    StartRet = sysmon_client_util:start_app(App, TryCount),
                    case StartRet of
	                true ->
                            put_auto_service(App, true, "service restart success"),
                            true;
                        false ->
                            put_auto_service(App, true, "service restart failed"),
                            false
                    end
            end
    end.

%% Internal APIs:

check_deps([]) ->
    true;
check_deps(DepList) ->
    Ret = lists:foldl(fun({App, TryCount}, AccIn) ->
                          [run({App, TryCount}, []) | AccIn]
                      end, [], DepList),
    case lists:member(false, Ret) of
        true -> false;
        false -> true
    end.

%% 把日志数据put到sysmon server.
-spec put_auto_service(App :: atom(), Type :: boolean(), Value :: string()) -> undefined | ok | {error, any()}.
put_auto_service(App, Type, Value) -> 
    Ret = #auto_service{app = App,
			host = ?LOCALHOST_DEF,
			time = calendar:local_time(),
			type = Type,
	                value = Value},
    sysmon_client_api:put_service("auto_service", atom_to_list(App), Ret).

