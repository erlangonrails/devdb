%% @doc handle the http request (use mochiweb)
-module(e2d_c_httpd).
-include("e2d.hrl").

-export([start_link/0]).
-export([handle_request/1]).

%% @doc start the http server
start_link() ->
    Options = get_mochiweb_options(),
    Loop = fun(Req) -> ?MODULE:handle_request(Req) end,
    mochiweb_http:start([{loop, Loop}, {name, e2d_c_httpd} | Options]).

%% @doc handle the http request
-spec handle_request(Req :: atom()) -> 'ok'.
handle_request(_Req) ->
    ok.

%% get the mochiweb http server options from e2d.config
get_mochiweb_options() ->
    Envs = application:get_all_env(),
    lists:foldl(fun({P, _V} = Env, Acc) ->
                        case P of
                            ip ->
                                [Env | Acc];
                            port ->
                                [Env | Acc];
                            backlog ->
                                [Env | Acc];
                            max ->
                                [Env | Acc];
                            _ ->
                                Acc
                        end
                end,
                [],
                Envs).
