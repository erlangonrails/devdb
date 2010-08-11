-module(monitor_app_task).
-include("../client_api/sysmon_client_api_config.hrl").
-include("../sysmon.hrl").
-export([run/2]).

-spec run(App :: atom(), Type :: atom()) -> undefined | ok | {error, any()}.
run(App, 'cpu') ->
    case sysmon_client_util:get_cpu(App) of
	undefined ->
	    undefined;
	CPU ->
	    Ret = #monitor_app{app = App,
			       host = ?LOCALHOST_DEF,
			       type = 'cpu',
			       time = calendar:local_time(),
			       value = CPU},
	    sysmon_client_api:put_service("monitor_app", atom_to_list(App), Ret)
    end;
run(App, 'memory') ->
    case sysmon_client_util:get_mem(App) of
	undefined ->
	    undefined;
	Memory ->
	    Ret = #monitor_app{app = App,
			       host = ?LOCALHOST_DEF,
			       type = 'memory',
			       time = calendar:local_time(),
			       value = Memory},
	    sysmon_client_api:put_service("monitor_app", atom_to_list(App), Ret)
    end.
    
