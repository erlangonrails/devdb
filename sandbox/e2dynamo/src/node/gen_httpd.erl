-module(gen_httpd).

-include("e2d.hrl").

-export([init/3, negotiate_content_type/1]).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
     % handle http request
     {handle_request, 1}
    ];
behaviour_info(_Other) ->
    undefined.

init(Name, Module, Config) ->
    Option = get_option(Config),
    ?Debug("init the httpd...~p~n", [Option]),
    Loop = fun(Req) -> Module:handle_request(Req) end,
    case mochiweb_http:start([{loop, Loop}, {name, Name} | Option]) of
        {ok, Res} ->
            ?Debug("mochiweb start: ~p~n", [Res]),
            {ok, Res};
        {error, Reason} ->
            ?Warn("could not start Mochiweb, reason: ~p, check option: ~p~n",
                  [Reason, Option]),
            {error, Reason}
    end.

%% @doc get the mochiweb httpd config from the Config
-spec get_option(Config :: list()) -> list().
get_option(Config) ->
    case proplists:get_value(httpd, Config) of
        undefined ->
            [];
        Config0 ->
            lists:filter(
                fun({Key, _Value}) ->
                    lists:member(Key, [ip, port, name, backlog, max])
                end,
                Config0)
    end.

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
