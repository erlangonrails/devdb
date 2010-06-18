%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the manager server, handle the http request
-module(e2d_mgr_httpd).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("e2d_manager.hrl").

-export([start_link/0, start_link/1]).
-export([handle_request/1]).

%% @doc start the http server
start_link() ->
    start_link([{name, "e2d_manager_httpd"}, {ip, "0.0.0.0"}, {port, 8000}]).

%% @doc start the http server
start_link(Opts) ->
    Loop = fun(Req) -> ?MODULE:handle_request(Req) end,
    mochiweb_http:start([{loop, Loop} | Opts]).

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

    ?Log("~s -- ~s ~s ~B", [Req:get(peer)
                        , Req:get(method)
                        , Path
                        , Resp:get(code)]).


%%
%% internal API
%%

%% handle the request
handle_request(Req, _Method, "/") ->
    handle_welcom(Req);
handle_request(Req, 'GET', "/config") ->  % get the config info
    handle_config(Req);
handle_request(Req, _Method, _Path) ->  % other unknow
    send_respond(Req, 401, "Unknown Command").

handle_welcom(Req) ->
    send_respond(Req, "welcome to e2dynamo manager !").

%% handle the config request
handle_config(Req) ->
    % get the request ip
    ClientIp = Req:get(peer),

    % get the system config info
    {Id, Name} =
    case e2d_mgr_nodes:get_node(ClientIp) of
        #node_info{id = Id, name = Name} ->
            {Id, Name};
        _->
            Id = Name = 0,
            throw(not_in_cluster)
    end,
    Keys1 = [n, w, r, cookie, api_timeout, buckets_number],
    Values1 = e2d_mgr_sysconf:get(Keys1),
    Conf1 = lists:zip(Keys1, Values1),

    Nodes0 = e2d_mgr_nodes:get_nodes(),
    Nodes = e2d_mgr_nodes:exclue_node({name, Name}, Nodes0),


    Conf = [{name, Name}, {id, Id}, {nodes, Nodes} | Conf1],
    ConfStr = io_lib:format("~p", [Conf]),
    ?Debug("Conf Str is:~s~n", [ConfStr]),
    {ok, ConfStr}.

%% send the error msg
send_error_rsp(Req, Error) ->
    {Code, Msg} = error_to_str(Error),
    send_respond(Req, Code, Msg).

error_to_str(not_in_cluster) ->
    {401, "This Node is not in the Cluster"};
error_to_str(Error) ->
    {401, "unknow error"}.

%%  send json object to peer
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
    [{"Server", "E2Dyanmo Manager/" ++ e2d_server:get_version() ++
                " (Erlang OTP/" ++ OTPVersion ++ ")"}].
