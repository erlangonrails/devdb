-module(sysmon_client_api).
-include("sysmon_client_api_config.hrl").
-export([get/1, put/2, get_service/2, put_service/3]).

%%
%% 1. 这个模块依赖inets模块, 所以在get/put api调用的时候都会确认inets是否启动
%% 每次API请求都调用该函数效率不高, 但作为独立的Client API可以满足需求.
%%
%% 2. 所有的POST到server的data要经过term_to_binary/1编码成二进制, 服务器
%% 端会调用binary_to_term进行解码. 
%%
%% 3. URL: 
%% http://host/api/Service/Key
%%
-spec get(Srv :: string()) -> {ok, {any(), binary()}} | {error, any()}.
get(Url) ->
    ensure_inets(),
    HttpOpts = [{timeout, ?HTTP_TIMEOUT}],
    Opts = [{sync, true}, {body_format, binary}, {full_result, false}],
    case http:request(get, {Url, []}, HttpOpts, Opts) of
        {ok, {Code, Body}} ->
            {ok, {Code, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec put(Url :: string(), Data :: binary()) -> {ok, {any(), binary()}} | {error, any()}.
put(Url, Data) when is_binary(Data)  ->
    ensure_inets(),
    HttpOpts = [{timeout, ?HTTP_TIMEOUT}],
    Opts = [{sync, true}, {body_format, binary}, {full_result, false}],
    case http:request(post, {Url, [], "text/plain", Data}, HttpOpts, Opts) of
        {ok, {Code, Body}} ->
            {ok, {Code, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_service(Srv :: string(), Key :: string()) -> {ok, {any(), binary()}} | {error, any()}.
get_service(Service, Key) ->
    Url = build_url(Service, Key),
    ?MODULE:get(Url).

-spec put_service(Url :: string(), Key :: string(), Value :: any()) -> ok | {error, any()}.
put_service(Service, Key, Value) ->
    Url = build_url(Service, Key),
    case ?MODULE:put(Url, term_to_binary(Value)) of
	{ok, {200, <<"success">>}} ->  %% sysmon server端预定义的返回值 :)
	    ok;
	{ok, {200, Reason}} ->
	    {error, Reason};
	{error, Reason1} ->
	    {error, Reason1};
	_ ->
	    {error, unknown}
    end.


%% Internal APIs:
-spec build_url(Service :: string(), Key :: string()) -> string().
build_url(Service, Key) ->
    build_url(?SYSMON_HOST, Service, Key).

%% @doc
%% Url: http://Host/api/Service/Key
-spec(build_url(Host :: string(), Service :: string(), Key :: string()) -> string()).
build_url(Host, Service, Key) ->
    case Host of
        "http://" ++ _Rest ->
            lists:append([Host, "/api/", Service, "/", Key]);
        _ ->
            lists:append(["http://", Host, "/api/", Service, "/", Key])
    end.

ensure_inets() ->
    inets:start().
