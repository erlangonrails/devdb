%%% @doc handle the commumicate with other nodes
-module(e2d_comm).
-include("e2d.hrl").

-export([request/2]).

%% @doc send request to other node
-spec request(Node :: atom(), Req :: any()) ->
    {'error', atom()} | {'ok', any()}.
request(Node, Req) when is_atom(Node) ->
    case catch gen_server:call({e2d_server, Node}, {node_req, Req}) of
        {'EXIT', _Reason, _} ->
            ?Error("request node ~w :~w   error:~w~n", [Node, Req, _Reason]),
            {error, request};
        Reply ->
            {ok, Reply}
    end.
