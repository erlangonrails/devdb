%% @doc a simple module access the e2d node via http
-module(e2d_http_api).

-export([get/2, put/2, del/2]).
-define(TIMEOUT, 5000).

%% @doc get the key from the server
-spec get(Srv :: string(), Key :: string()) ->
    {'ok', any()} | {'error', atom()}.
get(Srv, Key) when is_list(Srv)  ->
    Url = build_url(Srv, Key),
    HttpOpts = [{timeout, ?TIMEOUT}],
    Opts = [{sync, true}, {body_format, binary}, {full_result, false}],
    case http:request(get, {Url, []}, HttpOpts, Opts) of
        {ok, {Code, Body}} ->
            parse_get_body(Code, Body);
        {error, _Reason} ->
            {error, http_get}
    end.

%% @doc add the key value to the server
put(Srv, {Key, _Context, Value}) when is_list(Srv) ->
    Url = build_url(Srv, Key),
    HttpOpts = [{timeout, ?TIMEOUT}],
    Opts = [{sync, true}, {body_format, binary}, {full_result, false}],

    %% any question???
    ValueBin = e2d_util:any_to_binary(Value),
    PostData = {struct, [{context, <<>>}, {value, ValueBin}]},
    PostJson = mochijson2:encode(PostData),
    %%io:format("Post Data:~p Len:~p~n", [PostJson, iolist_size(PostJson)]),
    case http:request(post, {Url, [], "application/json", iolist_to_binary(PostJson)}, HttpOpts, Opts) of
        {ok, {Code, Body}} ->
            parse_put_body(Code, Body);
        {error, _Reason} ->
            {error, http_post}
    end.

%% @doc delete the value from the server
del(Srv, Key) ->
    Url = build_url(Srv, Key),
    HttpOpts = [{timeout, ?TIMEOUT}],
    Opts = [{sync, true}, {body_format, binary}, {full_result, false}],
    case http:request(delete , {Url, []}, HttpOpts, Opts) of
        {ok, {Code, Body}} ->
            parse_del_body(Code, Body);
        {error, _Reason} ->
            {error, http_del}
    end.

build_url(Srv, Key) ->
    KeyStr =
    if is_list(Key) ->
        Key;
       is_integer(Key) ->
        integer_to_list(Key)
    end,
    case Srv of
        "http://" ++ _Rest ->
            lists:append([Srv, "/", KeyStr]);
        _ ->
            lists:append(["http://", Srv, "/", KeyStr])
    end.

%% {ok, value}
parse_get_body(Code, Body) ->
    {struct, PropList} = mochijson2:decode(Body),
    case proplists:get_value(<<"ok">>, PropList) of
        true -> % return value
            {ok, proplists:get_value(<<"value">>, PropList, <<>>)};
        false ->   % the value is in confict state, return {ok, ValueList}
            {ok, proplists:get_value(<<"value">>, PropList, [])};
        undefined ->
            Error = proplists:get_value(<<"error">>, PropList),
            Reason = proplists:get_value(<<"reason">>, PropList),
            {error, {Code, Error, Reason}}
    end.

parse_put_body(Code, Body) ->
    {struct, PropList} = mochijson2:decode(Body),
    case proplists:get_value(<<"ok">>, PropList) of
        true -> % return value
            {ok, success};
        undefined ->
            Error = proplists:get_value(<<"error">>, PropList),
            Reason = proplists:get_value(<<"reason">>, PropList),
            {error, {Code, Error, Reason}}
    end.

parse_del_body(Code, Body) ->
    {struct, PropList} = mochijson2:decode(Body),
    case proplists:get_value(<<"ok">>, PropList) of
        true -> % return value
            {ok, success};
        undefined ->
            Error = proplists:get_value(<<"error">>, PropList),
            Reason = proplists:get_value(<<"reason">>, PropList),
            {error, {Code, Error, Reason}}
    end.
