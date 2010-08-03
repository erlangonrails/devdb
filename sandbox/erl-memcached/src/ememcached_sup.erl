-module(ememcached_sup).

% Supervisor of the ememcached listener

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{one_for_one, 1, 10},
          [{ememcached_listener, {ememcached_listener, start_link, []},
            permanent, brutal_kill, worker, [ememcached_listener]}]
	 }}.
