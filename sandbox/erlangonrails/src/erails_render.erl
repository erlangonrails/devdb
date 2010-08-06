-module(erails_render).
-include("erl_logger.hrl").
-export([run/3]).

%%
%% @doc Render the template with the given data.
%% This is called from YOUR_APP_web.erl automatically.
%%    
%% ViewFile = index.html         -> index_view
%%            user/new.html      -> user_new_view
%% 
run(FullPathToFile, ViewFile, Data) ->
    Pieces = string:tokens(ViewFile, "/"),
    Name = string:join(Pieces, "_"),
    Name1 = filename:basename(Name,".html"),
    ModName = list_to_atom(Name1 ++ "_view"),

    case erlydtl:compile(FullPathToFile, ModName) of
	ok ->
	    case ModName:render(Data) of
		{ok, Result} ->
		    Result;
		{error, Error} ->
		    ?ERROR_MSG("~p:render error ~p~n", [ModName, Error]),
		    io_lib:format("~p:render error ~p~n", [ModName, Error])
            end;
	{error, Reason} ->
	    ?ERROR_MSG("erlydtl:compile/2 error ~p~n", [Reason]),
	    io_lib:format("erlydtl:compile/2 error ~p~n", [Reason])
    end.
