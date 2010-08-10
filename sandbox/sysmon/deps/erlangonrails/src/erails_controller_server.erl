-module(erails_controller_server).
-behaviour(gen_server).
-include("erl_logger.hrl").

%%
%% Erlang On Rails的Name-controller映射模块
%%
%% 例如:
%% "login" -> login_controller
%% "logout" -> logout_controller
%% 
%% 可以防止由于DoS攻击而产生的大量atom()对global atom() table的填充.
%% Dos攻击会产生大量请求, 每一个请求都会产生atom(), 将迅速填充系统的atom() table.

%% API
-export([start/1, get_controller/1, list_controller/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% 
%% BaseDir是Erlang On Rails app的根目录.
%%
start(BaseDir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, BaseDir,[]).

%%
%% 返回 ControllerName:atom() | no_controller
%% Controller:string()
%%
%% 例如:
%% get_controller("login") -> login_controller
-spec(get_controller(string()) -> atom() | no_controller).
get_controller(Controller) ->
    gen_server:call(?MODULE, {get_controller, Controller}).

%%
%% 辅助函数: Logger所有的name-controller映射.
%%
list_controller() ->
    gen_server:call(?MODULE, view).

%% @hidden
init(BaseDir) ->
    {ok, load_controllers(BaseDir)}.

%% ---------Callbacks------------------------------

%% @hidden
handle_call({get_controller, Controller},_From, State) ->
    Reply = case lists:keysearch(Controller,1,State) of
		{value,{_,C}} ->
		    {ok,C};
		false ->
		    no_controller
	    end,
    {reply, Reply, State};

%% @hidden
handle_call(view,_From,State) ->
    ?INFO_MSG("Controller Map:~n~p",[State]),
    {reply,ok,State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.
%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.
%% @hidden
terminate(_Reason, _State) ->
    ok.
%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal APIs:
list_controllers(BaseDir) ->
    Path = filename:join([BaseDir, "src/controllers", "*_controller.erl"]),
    %% returns a list of all files that match Unix-style wildcard-string Wildcard.
    filelib:wildcard(Path).

%% @return [{"login", login_controller}, {"logout", logout_controller}]    
load_controllers(BaseDir) ->
    lists:foldl(fun(File,Acc) ->
			OrgName = filename:basename(File, ".erl"),
			KeyName = string:substr(OrgName, 1, length(OrgName) - length("_controller")), 
			AtomName = list_to_atom(OrgName),
			[{KeyName,AtomName}|Acc]
		end,
		[],
		list_controllers(BaseDir)).
    
