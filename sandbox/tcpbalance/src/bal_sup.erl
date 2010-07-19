-module(bal_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(StartArgs) ->
    supervisor:start_link({local, balance_sup}, ?MODULE, StartArgs).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%----------------------------------------------------------------------

init(ArgList) ->
    BalProxy = {bal_proxy, {bal_proxy, start_link, [ArgList]},
                permanent, 2000, worker, [bal_proxy]},
    {ok, {{one_for_all, 5, 60}, [BalProxy]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
