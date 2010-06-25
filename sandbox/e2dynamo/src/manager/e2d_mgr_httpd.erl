%% @doc the manager server, handle the http request
-module(e2d_mgr_httpd).
-include("e2d_manager.hrl").

-export([start_link/0, start_link/1]).
-export([handle_request/1]).

%% 模块说明:
%% 这个模块就是一个基于mochiweb的httpd的后台服务, 侦听在8000端口.
%% 当node发起HTTP GET请求访问/config的时候, 返回用户它的配置信息.
%%
%% 如何知道当前是哪个node在发起GET /config请求?
%% e2d_mgr_nodes模块管理所有的node, 并提供了根据IP搜索node信息的功能,
%% 当一个node发起HTTP GET请求的时候, 我们从Req中解析出其IP, 调用
%% e2d_mgr_nodes:get_node(Ip)来返回其信息.
%% 
%% 是如下格式的字符串: 
%% (注意: node模块可以通过e2d_util:consult_str(Str)来还原出这个原始的list term)
%% [{name, Name},
%%  {id, Id},
%%  {nodes, Nodes},   %% node_info list(除去自身之外的nodes的集合)
%%  {n, N},
%%  {w, W},
%%  {r, R}, 
%%  {cookie, Cookie},
%%  {api_timeout, Timeout},
%%  {buckets_number, Number}]

%% @doc start the http server
start_link() ->
    start_link([{name, "e2d_manager_httpd"}, {ip, "0.0.0.0"}, {port, 8000}]).

%% @doc start the http server
start_link(Opts) ->
    Loop = fun(Req) -> ?MODULE:handle_request(Req) end,
    case mochiweb_http:start([{loop, Loop} | Opts]) of
	{ok, Res} ->
            ?Debug("mochiweb start: ~p~n", [Res]),
            {ok, Res};
        {error, Reason} ->
            ?Warn("could not start Mochiweb, reason: ~p, check option: ~p~n",
                  [Reason, Opts]),
            {error, Reason}
    end.

%% @doc handle the http request
-spec handle_request(Req :: atom()) -> 'ok'.
handle_request(Req) ->
    {Path, _Q, _F} = mochiweb_util:urlsplit_path(Req:get(raw_path)),
    Method = Req:get(method),
    {Main, Minor} = Req:get(version),
    ?Log("~s ~s HTTP ~B.~B ", [Method, Path, Main, Minor]),
    ?Debug("~s ~s HTTP ~B.~B  ~n", [Method, Path, Main, Minor]),

    {ok, Resp} =
    case catch handle_request(Req, Method, Path) of
        {ok, Resp0} ->
	    {ok, Resp0};
        Error ->
            send_error_rsp(Req, Error)
    end,

    ?Log("~s -- ~s ~s ~B", [Req:get(peer),
			    Req:get(method),
			    Path,
			    Resp:get(code)]).


%%
%% internal API
%%

%% handle the request
handle_request(Req, _Method, "/") ->
    handle_welcom(Req);
handle_request(Req, 'GET', "/config") ->  % get the config info
    handle_config(Req);
handle_request(Req, _Method, _Path) ->  % other unknow
    send_json(Req, 401, <<"Unknown Command">>).


handle_welcom(Req) ->
    send_json(Req, 200, <<"Welcom to use E2dynamo! version: v0.1">>).

%% handle the config request
handle_config(Req) ->
    % get the request ip
    ClientIp = Req:get(peer),

    % get the system config info
    {Id, Name} =
    case e2d_mgr_nodes:get_node(ClientIp) of
        #node_info{id = Id0, name = Name0} ->
            {Id0, Name0};
        _->
            throw(not_in_cluster)
    end,
    Keys1 = [n, w, r, cookie, api_timeout, buckets_number],
    Values1 = e2d_mgr_sysconf:get(Keys1),
    Conf1 = lists:zip(Keys1, Values1), 

    Nodes0 = e2d_mgr_nodes:get_nodes(),
    Nodes = e2d_mgr_nodes:exclude_node({name, Name}, Nodes0),


    Conf = [{name, Name},{id, Id},{nodes, Nodes} | Conf1],
    ?Debug("Conf is:~p~n", [Conf]),
    send_respond(Req, term_to_binary(Conf)).

%% send the error msg
send_error_rsp(Req, Error) ->
    {Code, Json} = error_to_json(Error),
    send_json(Req, Code, Json).


%% error to json
error_to_json(Error) ->
    {HttpCode, Atom, Reason} = error_to_json0(Error),
    FormattedReason =
    case (catch io_lib:format("~s", [Reason])) of
        List when is_list(List) ->
            List;
        _ ->
            io_lib:format("~p", [Reason]) % else term to text
    end,

    Json = {struct, [{error, iolist_to_binary(atom_to_list(Atom))},
                     {reason, iolist_to_binary(FormattedReason)}]},
    {HttpCode, Json}.

error_to_json0(not_in_cluster) ->
    {401, error, "This Node is not in the Cluster"};
error_to_json0(_Error) ->
    {500, error, "unknown error"}.


send_respond(Req, Data) ->
    send_respond(Req, 200, Data).

send_respond(Req, Code, Data) ->
    send_respond(Req, Code, [], Data).

send_respond(Req, Code, Headers, Data) ->
    DefaultHeaders = [
        {"Content-Type", negotiate_content_type(Req)},
        {"Cache-Control", "must-revalidate"}
    ] ++ server_header(),
    Resp = Req:respond({Code, Headers ++ DefaultHeaders, Data}),
    {ok, Resp}.


%% send json object to peer
%% Value必须是一个json_term()
%%
%% @type iolist() = [char() | binary() | iolist()]
%% @type iodata() = iolist() | binary()
%% @type json_string() = atom | binary()
%% @type json_number() = integer() | float()
%% @type json_array() = [json_term()]
%% @type json_object() = {struct, [{json_string(), json_term()}]}
%% @type json_iolist() = {json, iolist()}
%% @type json_term() = json_string() | json_number() | json_array() |
%%                     json_object() | json_iolist()
send_json(Req, Value) ->
    send_json(Req, 200, Value).

send_json(Req, Code, Value) ->
    send_json(Req, Code, [], Value).

send_json(Req, Code, Headers, Value) ->
     DefaultHeaders = [
        {"Content-Type", negotiate_content_type(Req)},
        {"Cache-Control", "must-revalidate"}
    ] ++ server_header(),
    Body = mochijson2:encode(Value),
    Resp = Req:respond({Code, DefaultHeaders ++ Headers, Body}),
    {ok, Resp}.

negotiate_content_type(Req) ->
    %% Determine the appropriate Content-Type header for a JSON response
    %% depending on the Accept header in the request. A request that explicitly
    %% lists the correct JSON MIME type will get that type, otherwise the
    %% response will have the generic MIME type "text/plain"
    AcceptedTypes = case Req:get_header_value("Accept") of
        undefined       -> [];
        AcceptHeader    -> string:tokens(AcceptHeader, ", ")
    end,
    case lists:member("application/json", AcceptedTypes) of
        true  -> "application/json";
        false -> "text/plain;charset=utf-8"
    end.

server_header() ->
    OTPVersion = "R" ++ integer_to_list(erlang:system_info(compat_rel)) ++ "B",
    [{"Server", "E2Dyanmo Manager/v0.1" ++
                " (Erlang OTP/" ++ OTPVersion ++ ")"}].
