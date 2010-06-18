%% @author litao cheng litaocheng@gmail.com
%%% @copyright 2008 toquick.com.
%%% @doc some util function for network
-module(net_util).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("e2d.hrl").

-export([http_get/2, http_get/3]).


-spec http_get(Url :: string(), Timeout :: pos_integer()) ->
    {'ok', string()} | {'error', any()}.
http_get(Url, Timeout) when is_list(Url) andalso is_integer(Timeout) ->
    http_get(Url, Timeout, true).

-spec http_get(Url :: string(), Timeout :: pos_integer(), Sync :: bool()) ->
    {'ok', string()} | {'error', any()}.
http_get(Url, Timeout, Sync) ->
    Options = [{sync, Sync}, {body_format, binary}],
    HttpOptions = [{timeout, Timeout}, {autoredirect, true}],
    case http:request(get, {Url, [{"connection", "close"}]}, HttpOptions, Options) of
        {ok, Result} ->
            {ok, Result};
        Error ->
            Error
    end.


-ifdef(debug).

basic_test_() ->
    _Url = "http://www.erlang.org/doc/",

    [
    ].

-endif.
