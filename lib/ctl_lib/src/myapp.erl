-module(myapp).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0, loop/0]).

start() ->
    application:start(sasl), 
    application:start(myapp).

start(normal, _StartArgs) ->
    case proc_lib:spawn_link(?MODULE, loop, []) of
	Pid -> 
	    {ok, Pid}
    end.

stop(_State) ->
    ok.

loop() ->
    timer:sleep(1000 * 5),
    io:format("myapp is running...~n", []),
    loop().


