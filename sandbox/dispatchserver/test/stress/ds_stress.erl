-module(ds_stress).
-export([gen_load/2, get/1]).

-define(DS_PORT, 5001).
-define(DEF_INTERVAL, 20).
-define(EXPECT_RESPONSE_1, <<"127.0.0.1:5222">>).
-define(EXPECT_RESPONSE_2, <<"192.168.0.13:5222">>).


gen_load(Count, Interval) ->
    lists:foreach(fun(_) ->
                      timer:sleep(?DEF_INTERVAL),
                      spawn(fun() -> ?MODULE:get(Interval) end)
                  end, lists:seq(1, Count)),
    ok.

get(Interval) ->
    timer:sleep(Interval),
    {ok, _NewSocket} = gen_tcp:connect("localhost", ?DS_PORT, 
				   [binary, {packet, 0}]),
    receive
	{tcp, Socket, ?EXPECT_RESPONSE_1} ->
	    %%io:format("passed# ~p~n", [?EXPECT_RESPONSE_1]),
	    gen_tcp:close(Socket),
            ?MODULE:get(Interval);
	{tcp, Socket, ?EXPECT_RESPONSE_2} ->
	    %%io:format("passed# ~p~n", [?EXPECT_RESPONSE_2]),
	    gen_tcp:close(Socket),
            ?MODULE:get(Interval);
	{tcp_closed, Socket} ->
            gen_tcp:close(Socket),
            ?MODULE:get(Interval);
	Any ->
	    io:format("failed# ~p~n", [Any]),
            ?MODULE:get(Interval)
    end.
