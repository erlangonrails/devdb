-module(api_controller,[Env]).

-export([handle_request/2,before_filter/0]).
-include("erl_logger.hrl").
-include("../sysmon.hrl").

-define(SRV_MONITOR_APP, "monitor_app").
-define(SERVICE_LIST, [?SRV_MONITOR_APP]). %% 定义获得许可的服务名称

%%
%% 该模块负责处理客户端的请求sysmon_client_api
%% 1. 请求的URL
%% http://host/api/Service/Key
%%
%% 2. 对于处理成功的POST请求, 一律返回HTTP_OK给客户端
%%

% Maximum recv_body() length of 1MB
-define(MAX_RECV_BODY, (1024*1024)).
-define(HTTP_OK, "success").

handle_request(Service, [Key]) ->
    case erails_var:method(Env) of
	'GET' ->
	    handle_request_get(Service, Key);
	'POST' ->
	    handle_request_post(Service, Key);
	_ ->
	    {text, "request method error!!!"}
    end;
handle_request(_Service, _) ->
    {text, "request format error!!!"}.

before_filter() ->
    case lists:member(erails_var:get_action(Env), ?SERVICE_LIST) of
	true ->
	    ok;
	false ->
	    {text, "can't find this service!!!"}
    end.

%% =========================================================
%% Internal APIs:
%% =========================================================

%% @doc
%% 获取POST请求的body数据.
-spec get_post_body(Key :: string()) -> {ok, binary()} | {error, any()}.
get_post_body(_Key) ->
    Req = erails_var:req(Env),
    case catch Req:recv_body() of
        {'EXIT', Reason} ->
	    {error, Reason};
        Body when is_binary(Body) ->
	    {ok, Body};
	_ ->
	    {error, unknown}
    end.

handle_request_get(Service, Key) ->
    %% TODO: 处理GET请求的逻辑, 返回结果给客户端
    %% 当前版本不需要处理这部分逻辑
    ?DEBUG("request_get#Service: ~s, Key: ~s", [Service, Key]),
    {text, ?HTTP_OK}.

handle_request_post(Service, Key) ->
    case get_post_body(Key) of
	{ok, BinValue} ->
	    case catch binary_to_term(BinValue) of
		{'EXIT', _} ->
		    {text, "binary_to_term error!!!"};
		TermValue ->
                    %% 处理POST请求的逻辑, 返回结果给客户端
                    %% 如果成功返回?HTTP_OK, 错误返回错误信息.
		    ?DEBUG("request_post#Service: ~s, Key: ~s, Val: ~p", [Service, Key, TermValue]),
                    handle_request_post_in(Service, Key, TermValue)
            end;
	{error, _} ->
	    {text, "post recv_body error!!!"}
    end.

%% @doc
%% 成功返回{text, ?HTTP_OK}, 失败返回{text, Reason :: string()}
handle_request_post_in(?SRV_MONITOR_APP, Key, #monitor_app{app = _App, 
							   host = Host,
							   type = Type,
							   time = Time,
							   value = _Value} = Term) ->
    LogFile = io_lib:format("~s#~s#~p#~s#~s.data", [?SRV_MONITOR_APP, Key, Type, Host, datetime_to_filename(Time)]),
    sysmon_util:log_data(LogFile, "~p.~n", [Term]),
    {text, ?HTTP_OK};
handle_request_post_in(_Service, _Key, _Term) ->
    {text, "handle_request_post_in error!!!"}.


datetime_to_filename({{Y,M,D},{_,_,_}}) ->
    integer_to_list(Y) ++ "_" ++ integer_to_list(M) ++ "_" ++ integer_to_list(D).
    
