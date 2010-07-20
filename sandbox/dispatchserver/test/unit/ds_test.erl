-module(ds_test).
-export([get/0]).

-define(DS_PORT, 5001).

get() ->
    {ok, Socket} = gen_tcp:connect("localhost", ?DS_PORT, 
				   [binary, {packet, 0}]),
    receive
	{tcp, Socket, Bin} ->
	    io:format("~p~n", [Bin]),
	    gen_tcp:close(Socket)
    end.
