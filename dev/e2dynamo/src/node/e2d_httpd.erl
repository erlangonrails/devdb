%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc handle the http request (use mochiweb)
-module(e2d_httpd).
-author('litaocheng@gmail.com').
-vsn('0.1').

-behaviour(gen_httpd).

-include("e2d.hrl").

-export([start_link/1]).
-export([handle_request/1]).

start_link(Config) ->
    ?Debug("start httpd..~n", []),
    gen_httpd:init(?MODULE, ?MODULE, Config).

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
handle_request(Req, Method, Path) ->
    "/" ++ KeyStr = Path,
    {ok, Key} =
    case catch list_to_integer(KeyStr) of
        {'EXIT', _R} ->
            throw({badarg, keyid_not_number});
        Key0->
            {ok, Key0}
    end,

    case Method of
        'GET' ->
            handle_req_get(Key, Req);
        'POST' ->
            handle_req_post(Key, Req);
        'PUT' ->
            handle_req_put(Key, Req);
        'DELETE' ->
            handle_req_delete(Key, Req);
        _Else ->
            throw({method_not_allowed, "GET,POST,PUT,DELETE"})
    end.

%% handle welcome info
handle_welcom(Req) ->
    send_json(Req, 200, <<"Welcom to use E2dynamo! version:",
                (iolist_to_binary(e2d_server:get_version()))/binary >>).

%% handle the get  command
handle_req_get(Key, Req) ->
    case e2d_server:get(Key) of
        {ok,  [{_Context, Value}]} ->
            ?Debug("e2d_http handle get return:~p~n", [Value]),
            send_json(Req, {struct, [{ok, true}, {value, Value}]});
        {ok, Values = [_|_]} -> % reversion confict
            Values2 = lists:foldl(
                fun({Context, Value}, Acc) ->
                     [{struct, [{context, Context}, {value, Value}]} |  Acc]
                end,
                [],
                Values),
            send_json(Req, {struct, [{ok, true} | {value, Values2}]});
         {error, {T, R}} ->
            send_json(Req, {struct, [{error, e2d_util:any_to_binary(T)}, {reason, e2d_util:any_to_binary(R)}]})
    end.

%% handle the put command
handle_req_put(Key, Req) ->
    case catch Req:recv_body() of
        {'EXIT', {body_too_large, _Len} = R} ->
            ?Warn("body too large : ~B~n", [_Len]),
            throw(R);
        {'EXIT', {unknown_transfer_encoding, _Unknown} = R} ->
            ?Warn("unknown_transfer_encoding : ~p~n", [_Unknown]),
            throw(R);
        Body when is_binary(Body) ->
            ?Debug("Body:~n~s~n", [Body]),
            {struct, Props} = mochijson2:decode(Body),
            Context = proplists:get_value(<<"context">>, Props),
            Value = proplists:get_value(<<"value">>, Props),
            case e2d_server:put(Key, Context, Value) of
                ok ->
                    send_json(Req, {struct, [{ok, true}]});
                {error, {T, R}} ->
                    %?Debug("put error: ~p ~n ~p", [T, R]),
                    send_json(Req, {struct,  [{error, e2d_util:any_to_binary(T)}, {reason, e2d_util:any_to_binary(R)}]})
            end
    end.

%% the same as the put command
handle_req_post(Key, Req) ->
    handle_req_put(Key, Req).

%% handle the delete command
handle_req_delete(Key, Req) ->
    case e2d_server:del(Key) of
        ok ->
            send_json(Req, {struct, [{ok, true}]});
        {error, {T, R}} ->
            send_json(Req, {struct,  [{error, e2d_util:any_to_binary(T)}, {reason, e2d_util:any_to_binary(R)}]})
    end.

%% send the error msg
send_error_rsp(Req, {method_not_allowed, Methods}) ->
    {ok, Req:respond({405, [{"Allow", Methods}] ++ server_header(), <<>>})};
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
                            {reason, iolist_to_binary(FormattedReason)}
                            ]},
    {HttpCode, Json}.

error_to_json0({badarg, keyid_not_number}) ->
    {401, bad_request, "key id must be integer"};
error_to_json0(Error) ->
    {500, error, Error}.

%%  send json object to peer
send_json(Req, Value) ->
    send_json(Req, 200, Value).

send_json(Req, Code, Value) ->
    send_json(Req, Code, [], Value).

send_json(Req, Code, Headers, Value) ->
     DefaultHeaders = [
        {"Content-Type", gen_httpd:negotiate_content_type(Req)},
        {"Cache-Control", "must-revalidate"}
    ] ++ server_header(),
    Body = mochijson2:encode(Value),
    Resp = Req:respond({Code, DefaultHeaders ++ Headers, Body}),
    {ok, Resp}.

server_header() ->
    OTPVersion = "R" ++ integer_to_list(erlang:system_info(compat_rel)) ++ "B",
    [{"Server", "E2Dyanmo/" ++ e2d_server:get_version() ++
                " (Erlang OTP/" ++ OTPVersion ++ ")"}].
