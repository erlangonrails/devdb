-module(erails_router).
-export([dispatch/1]).

%% Erlang On Rails的URL-mapping的路由器
%% 这个模块会被MochiWeb自动调用来路由requests. 不同的请求会路由到不同的Controllers上处理.
%% 例如:
%% /user/show
%% 会交给user_controller的(action)show来处理.
%% /user
%% 会交给user_controller的index处理
%% /
%% 会交给home_controller的index处理.
%%

%%
%% 路由一个Request给合适的Controller处理, 返回处理结果:
%%
%% @spec dispatch(Env::record()) -> tuple()
%%
dispatch(Env) ->
    PathComponents = erails_var:path_components(Env),
    {ControllerName,ActionName,Args}  = case PathComponents of
					    [] ->
						{"home","index",[]};
					    [C] ->
						{C,"index",[]};
					    [C,A | Params]  ->
						{C,A,Params}
					end,
    case erails_controller_server:get_controller(ControllerName) of
	{ok, Controller} ->
	    process_request(Env,Controller,ActionName,Args);
	no_controller ->
	    %% Try static content using the PATH_INFO
	    F = erails_var:path(Env),
	    {static,  F}
    end.

%% ControllerName:atom()
%% ActionName是处理请求的名字
process_request(Env,ControllerName,ActionName,Args) ->
    Env1 = erails_var:set_action(Env, ActionName),

    %% 参数化模板:
    Controller = ControllerName:new(Env1),
   
    %% 处理逻辑:
    %% <1> 如果Controller:before_filter()返回ok, 则交给Controller:handle_request/2处理
    %% <2> 如果Controller:before_filter()也可以直接渲染, 这样就会跳过Controller:handle_request/2返回结果,
    %%     这在忽略ActionName和Args的时候"整体渲染"很有用处
    %% 
    case try_filter(Controller) of
	ok ->
	    case catch(Controller:handle_request(ActionName,Args)) of
		{'EXIT',_} ->
		    {error,no_action};
		Response -> Response
	    end;
	Any ->
	    Any
    end.

try_filter(ControllerName) ->
    case catch(ControllerName:before_filter()) of
	{'EXIT', {undef,_}} -> ok;
	Any -> Any
    end.   
	    
	    
