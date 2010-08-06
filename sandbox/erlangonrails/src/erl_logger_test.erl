-module(erl_logger_test).
-include("erl_logger.hrl").
-export([run/0]).

-define(LOG_PATH, "erl_logger_test.log").

%% 在使用erl_log前, 应该调用?SETUP_LOG/1和?SET_LOG_LEVEL/1来
%% 初始化该模块, 然后就可以使用 :)
run() ->
    ?SETUP_LOG(?LOG_PATH),

    %% scenario - 1
    ?SET_LOG_LEVEL(3), %% warning -> debug和info将不会被输出
    ?DEBUG("scenario - 1# this is ~p msg~n", ["debug"]),
    ?INFO_MSG("scenario - 1# this is ~p msg~n", ["info"]),
    ?WARNING_MSG("scenario - 1# this is ~p msg~n", ["warning"]),
    ?ERROR_MSG("scenario - 1# this is ~p msg~n", ["error"]),
    ?CRITICAL_MSG("scenario - 1# this is ~p msg~n", ["critical"]),

    %% scenario - 2
    ?SET_LOG_LEVEL(5), %% debug -> 所有的信息都会输出
    ?DEBUG("scenario - 2# this is ~p msg~n", ["debug"]),
    ?INFO_MSG("scenario - 2# this is ~p msg~n", ["info"]),
    ?WARNING_MSG("scenario - 2# this is ~p msg~n", ["warning"]),
    ?ERROR_MSG("scenario - 2# this is ~p msg~n", ["error"]),
    ?CRITICAL_MSG("scenario - 2# this is ~p msg~n", ["critical"]),

    %% scenario - 3
    error_logger:info_msg("scenario - 3# this is a error_logger msg~n", []).
