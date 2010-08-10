-module(home_controller,[Env]).
-export([handle_request/2,before_filter/0]).

handle_request("index",[]) ->
    {render,"home/index.html",[{data, "home"}]}.

before_filter() ->
    FilterOnly = ["index"],
    case lists:member(erails_var:get_action(Env), FilterOnly) of
	true ->
	    ok;
	false ->
	    {text, "no action!!!"}
    end.
