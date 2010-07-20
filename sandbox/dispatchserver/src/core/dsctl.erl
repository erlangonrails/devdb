-module(dsctl).
-export([start/0, process/1, print_usage/0]).

-define(APP_NAME, 'ds').
-define(TCP_SERVER, 'tcp_server').

-define(STATUS_SUCCESS, 0).
-define(STATUS_ERROR,   1).
-define(STATUS_USAGE,   2).
-define(STATUS_BADRPC,  3).

%% Print in standard output
-define(PRINT(Format, Args),
    io:format(Format, Args)).

start() ->
    case init:get_plain_arguments() of 
        [SNode | Args] -> 
            SNode1 = case string:tokens(SNode, "@") of 
                         [_Node, _Server] -> 
                             SNode; 
                          _ -> 
                             case net_kernel:longnames() of 
                             true -> 
                                 SNode ++ "@" ++ inet_db:gethostname() ++ 
                                 "." ++ inet_db:res_option(domain); 
                             false -> 
                                 SNode ++ "@" ++ inet_db:gethostname(); 
                             _ -> 
                                 SNode 
                             end 
                      end, 
             Node = list_to_atom(SNode1), 
             Status = case rpc:call(Node, ?MODULE, process, [Args]) of 
                          {badrpc, Reason} -> 
                              ?PRINT("RPC failed on the node ~p: ~p~n", [Node, Reason]), ?STATUS_BADRPC; 
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
process(["restart"]) ->
    init:restart(),
    ?PRINT("dispatch server & tcp server restarted~n", []),
    ?STATUS_SUCCESS;
process(["stop"]) ->
    init:stop(),
    ?PRINT("dispatch server & tcp server stopped~n", []),
    ?STATUS_SUCCESS;
process(["req_count"]) ->
    ReqCount = ds_route:get_req_count(),
    ?PRINT("req_count: ~p~n", [ReqCount]),
    ?STATUS_SUCCESS;
process(["get_loglevel"]) ->
    LogLevel = ds_loglevel:get(),
    ?PRINT("loglevel: ~p~n", [LogLevel]),
    ?STATUS_SUCCESS;
process(["set_loglevel", LogLevel]) ->
    case catch list_to_integer(LogLevel) of
	{'EXIT', _} ->
	    ?PRINT("set_loglevel failed, Loglevel must be an integer~n", []),
	    ?STATUS_SUCCESS;
	Level ->
	    ds_loglevel:set(Level),
	    ?PRINT("set_loglevel success~n", []),
	    ?STATUS_SUCCESS
    end;
process(["get_listeners"]) ->
    Listeners = ds_config:get_listeners(),
    FmtListeners = lists:map(fun({listener, Host, Port, Count}) ->
			         io_lib:format("dispatch-service# ~s:~p [parallel=~p]~n", [Host, Port, Count])
                             end, Listeners),
    ?PRINT("dispatch-service listen host & port:~n" ++ FmtListeners, []),
    ?STATUS_SUCCESS;
process(["get_cluster"]) ->
    XmppSrvs = ds_config:get_cluster(),
    FmtSrvs = lists:map(fun({IP, Port}) ->
                            io_lib:format("xmpp-service# ~s:~p~n", [IP, Port])
                        end, XmppSrvs),
    ?PRINT("xmpp-service cluster:~n" ++ FmtSrvs, []),
    ?STATUS_SUCCESS;
process(["reopen_log"]) ->
    %% rotate the dispatchserver.log & sasl.log
    ds_logger_h:reopen_log(), 
    case application:get_env(sasl,sasl_error_logger) of 
        {ok, {file, SASLfile}} -> 
            error_logger:delete_report_handler(sasl_report_file_h), 
            ds_logger_h:rotate_log(SASLfile), 
            error_logger:add_report_handler(sasl_report_file_h, 
                                            {SASLfile, get_sasl_error_logger_type()}); 
        _ -> false 
    end,
    ?STATUS_SUCCESS;
process(_Args) ->
    print_usage(),
    ?STATUS_USAGE.

print_usage() ->
    CmdDescs = 
        [{"start", "start dispatch server & tcp server"},
         {"status", "get dispatch server status"}, 
         {"stop", "stop dispatch server"}, 
         {"restart", "restart dispatch server & tcp server"}, 
         {"req_count", "get the total requests count of dispatch server"},
	 {"get_loglevel", "get the log level"},
	 {"set_loglevel Level", "set the log level [0 - 5] [NoLog - Debug]"},
	 {"get_listeners", "get the dispatch service listen host & port"},
	 {"get_cluster", "get the xmpp service cluster"},
	 {"reopen_log", "rotate the dispatchserver.log and sasl.log to *-old.log"}],
    MaxCmdLen = lists:max(lists:map(fun({Cmd, _Desc}) -> 
                                        length(Cmd) 
                                    end, CmdDescs)), 
    NewLine = io_lib:format("~n", []), 
    FmtCmdDescs = lists:map(fun({Cmd, Desc}) -> 
                                ["  ", Cmd, 
				 string:chars($\s, MaxCmdLen - length(Cmd) + 2), 
                                 Desc, NewLine] 
                            end, CmdDescs), 
    ?PRINT( 
      "Usage: dsctl [--node nodename] command [options]~n" 
      "~n" 
      "Available commands in this dispatch server node:~n" 
      ++ FmtCmdDescs ++ 
      "~n" 
      "Examples:~n" 
      "  dsctl restart~n" 
      "  dsctl --node dispatchserver@host restart~n", 
     []). 


%% Function copied from Erlang/OTP lib/sasl/src/sasl.erl which doesn't export it 
get_sasl_error_logger_type () -> 
    case application:get_env (sasl, errlog_type) of 
        {ok, error} -> error; 
        {ok, progress} -> progress; 
        {ok, all} -> all; 
        {ok, Bad} -> exit ({bad_config, {sasl, {errlog_type, Bad}}}); 
        _ -> all 
    end.

