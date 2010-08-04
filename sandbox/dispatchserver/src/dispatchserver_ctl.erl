-module(dispatchserver_ctl).
-include("erl_logger.hrl").

-define(APP_NAME, 'dispatchserver').
-define(TCP_SERVER, 'tcp_server').


-export([get_config_file/0, get_log_file/0]).
-export([start/0, process/1]).

-define(DEFAULT_CONFIG_FILE, "dispatchserver.cfg").
-define(DEFAULT_LOG_FILE, "dispatchserver.log").
-define(STATUS_SUCCESS, 0).
-define(STATUS_ERROR,   1).
-define(STATUS_USAGE,   2).
-define(STATUS_BADRPC,  3).

get_config_file() ->
    case os:getenv("MYAPP_CONFIG_PATH") of
	false ->
	    ?DEFAULT_CONFIG_FILE;
	Path ->
	    Path
    end.

get_log_file() ->
    case os:getenv("MYAPP_LOG_PATH") of
	false ->
	    ?DEFAULT_LOG_FILE;
	Path ->
	    Path
    end.

start() ->
    case init:get_plain_arguments() of
	[SNode | Args] ->
	    Node = list_to_atom(SNode),
	    Status = case rpc:call(Node, ?MODULE, process, [Args]) of
			 {badrpc, Reason} ->
			     io:format("RPC failed on the node ~p: ~p~n", [Node, Reason]),
			     ?STATUS_BADRPC;
			 S ->
			     S
		     end,
	    halt(Status);
	_ ->
	    print_usage(),
	    halt(?STATUS_USAGE)
    end.

process(["status"]) ->
    {InternalStatus, ProvidedStatus} = init:get_status(), 
    ?PRINT("Node ~p is ~p. Status: ~p~n", 
             [node(), InternalStatus, ProvidedStatus]), 
    case lists:keysearch(?APP_NAME, 1, application:which_applications()) of 
        false -> 
            ?PRINT("~p is not running~n", [?APP_NAME]), 
            ?STATUS_ERROR; 
        {value, {_, _, Version}} ->
            ?PRINT("~p ~s is running in that node~n", [?APP_NAME, Version]),
            case lists:keysearch(?TCP_SERVER, 1, application:which_applications()) of
                false ->
                    ?PRINT("~p is not running~n", [?TCP_SERVER]), 
	            ?STATUS_ERROR;
	        {value, {_, _, Version1}} ->
                    ?PRINT("~p ~s is running in that node~n", [?TCP_SERVER, Version1]),
                    ?STATUS_SUCCESS 
            end
    end;
process(["stop"]) ->
    init:stop(),
    ?STATUS_SUCCESS;
process(["restart"]) ->
    init:restart(),
    ?STATUS_SUCCESS;
process(["req_count"]) ->
    ReqCount = dispatchserver_route:get_req_count(),
    ?PRINT("req_count: ~p~n", [ReqCount]),
    ?STATUS_SUCCESS;
process(["uptime"]) ->
    Uptime = dispatchserver_route:get_uptime(),
    ?PRINT("uptime: ~p~n", [calendar:now_to_local_time(Uptime)]),
    ?STATUS_SUCCESS;
process(["get_loglevel"]) ->
    LogLevel = erl_loglevel:get(),
    ?PRINT("loglevel: ~p~n", [LogLevel]),
    ?STATUS_SUCCESS;
process(["set_loglevel", LogLevel]) ->
    case catch list_to_integer(LogLevel) of
	{'EXIT', _} ->
	    ?PRINT("set_loglevel failed, Loglevel must be an integer~n", []),
	    ?STATUS_SUCCESS;
	Level ->
	    erl_loglevel:set(Level),
	    ?PRINT("set_loglevel success~n", []),
	    ?STATUS_SUCCESS
    end;
process(["get_listeners"]) ->
    Listeners = tcp_server:get_config(listeners, []),
    FmtListeners = lists:map(fun({listener, Host, Port, Count}) ->
			         io_lib:format("dispatch-service# ~s:~p [parallel=~p]~n", [Host, Port, Count])
                             end, Listeners),
    ?PRINT("dispatch-service listen host & port:~n" ++ FmtListeners, []),
    ?STATUS_SUCCESS;
process(["get_cluster"]) ->
    XmppSrvs = dispatchserver_route:get_cluster(),
    FmtSrvs = lists:map(fun({IP, Port}) ->
                            io_lib:format("xmpp-service# ~s:~p~n", [IP, Port])
                        end, XmppSrvs),
    ?PRINT("xmpp-service cluster:~n" ++ FmtSrvs, []),
    ?STATUS_SUCCESS;
process(["rotate_log"]) ->
    %% rotate the dispatchserver.log & sasl.log
    erl_logger_h:reopen_log(), 
    case application:get_env(sasl,sasl_error_logger) of 
        {ok, {file, SASLfile}} -> 
            error_logger:delete_report_handler(sasl_report_file_h), 
            erl_logger_h:rotate_log(SASLfile), 
            error_logger:add_report_handler(sasl_report_file_h, 
                                            {SASLfile, get_sasl_error_logger_type()}); 
        _ -> false 
    end,
    ?STATUS_SUCCESS;
process(_) ->
    print_usage(),
    ?STATUS_ERROR.


print_usage() ->
    io:format(
      "Usage: dispatchserverctl command~n"
      "~n"
      "Available commands:~n"
      "  start~n"
      "  stop~n"
      "  restart~n"
      "  status~n"
      "  debug~n"
      "  req_count~n"
      "  uptime~n"
      "  rotate_log~n"
      "  get_loglevel~n"
      "  set_loglevel Level~n"
      "  get_listeners~n"
      "  get_cluster~n"
      "  benchmark ReqCount ReqInterval(ms)"
      "~n"
      "Example:~n"
      "  dispatchserverctl restart~n"
     ).


%% Function copied from Erlang/OTP lib/sasl/src/sasl.erl which doesn't export it 
get_sasl_error_logger_type () -> 
    case application:get_env (sasl, errlog_type) of 
        {ok, error} -> error; 
        {ok, progress} -> progress; 
        {ok, all} -> all; 
        {ok, Bad} -> exit ({bad_config, {sasl, {errlog_type, Bad}}}); 
        _ -> all 
    end.
