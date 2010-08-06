-module(erails_var).
-export([path/1,
	 path_components/1,
	 method/1,
	 content_type/1,
	 content_length/1,
	 remote_addr/1,
	 get_all_headers/1,
	 get_cookie/2,
	 get_param/2, %% GET/POST Data

	 get_session_id/1,
	 set_session_id/2,
	 set_session_data/3,
	 get_session_data/2,
	 remove_session_data/2,
	 get_all_session_data/1,

	 get_action/1,
	 set_action/2,
	 flash/2,
	 get_flash/1]).

-define(AUTO_SESSION_ID, "auto_erails_sid").
-define(AUTO_ACTION_NAME, "action_name").
-define(ERAILS_FLASH, "erails_flash").

%%
%% @doc Return the Path
%% 对应Req:get(path)
%%
%% 返回值为string(),并且进行了urldecode,只包含URI中的path,不包含query string和fragment部分.
%%
%% 举例:
%% 假设请求URL为:
%% http://domain.com/book%20food/my?user=id#sec1
%% 则path为 "/book food/my"
%%
-spec(path([{any(), any()}]) -> string()).
path(Env) ->
    proplists:get_value("PATH_INFO",Env).

%%
%% @doc Return the Path as an array parsed on the "/"
%% 我们可以根据这个组件进行请求的路由 :)
-spec(path_components([{any(), any()}]) -> [] | [string()]).
path_components(Env) ->
    string:tokens(path(Env),"/").



%%
%% @doc Return the request method: 'GET','PUT','POST','DELETE'
%% 对应Req:get(method)
-spec(method([{any(), any()}]) -> atom()).
method(Env) ->
    proplists:get_value("REQUEST_METHOD", Env).

%% 
%% @doc Return the content-type
%%
content_type(Env) ->
    proplists:get_value("CONTENT_TYPE", Env).

%%
%% @doc Return the content_length
%%
content_length(Env) ->
    proplists:get_value("CONTENT_LENGTH", Env).

%%
%% @doc Return the Remote address of the client
%% 相当于Req:get(peer), 返回值为client的ip
-spec(remote_addr([{any(), any()}]) -> string()).
remote_addr(Env) ->
    proplists:get_value("REMOTE_ADDR", Env).

%%
%% @doc Return all Headers
%%
get_all_headers(Env) ->
    lists:foldl(fun({"HTTP_" ++ _, _}=Pair, Hdrs) ->
                        [Pair|Hdrs];
                   (_, Hdrs) ->
                        Hdrs
                end, [], Env).

%% 
%% @doc Return a request Value for a given Key. This contains information
%% from a form POST OR GET query string
%%
-spec(get_param(string(), [{any(), any()}]) -> string() | undefined).
get_param(Key,Env) ->
    Params = proplists:get_value("ERAILS_DATA",Env),
    proplists:get_value(Key,Params).

%% @doc Return a request Value for a given Key. This contains infromations
%% from cookies.
-spec(get_cookie(string(), [{any(), any()}]) -> string() | undefined).
get_cookie(Key, Env) ->
    Cookies = proplists:get_value("COOKIES", Env),
    proplists:get_value(Key, Cookies).


%% 'falsh' message就是在render view的时候会传递这样的参数: {flash, [{Key, Val}]}

%%
%% @doc Set a 'flash' message for use in your template. 
%% All flash message are wrapped in a List
%% 
%% 
-spec(flash({any(), any()}, [{any(), any()}]) -> [{any(), any()}]).
flash(Term,Env) ->
    Flash = case get_session_data(?ERAILS_FLASH,Env) of
		undefined ->
		    [Term];
		ExistingFlash ->
		    [Term|ExistingFlash]
	    end,
    set_session_data(?ERAILS_FLASH,Flash,Env).


%% Get and clear the flash
%% 返回[{Key, Val}]或者none, 并同时清除Evn中的flash信息.
-spec(get_flash([{any(), any()}]) -> none | [{any(), any()}]).
get_flash(Env) ->
    Sid = get_session_id(Env),
    case get_session_data(?ERAILS_FLASH, Env) of
	undefined ->
	    %% No flash data
	    none;
	Data ->
	    erails_session_server:remove_session_data(Sid, ?ERAILS_FLASH),
	    Data
    end.


%%
%% @doc Get the current session id
%%
get_session_id(Env) ->
    proplists:get_value(?AUTO_SESSION_ID,Env).

%% 
%% Sets the session id. This is done internally
%% and should not be used manually
%% @hidden
%%  
set_session_id(Value,Env) ->
    case lists:keysearch(?AUTO_SESSION_ID,1,Env) of
	{value,_} ->
	    set_value(?AUTO_SESSION_ID,Value,Env);
	false ->
	    [proplists:property({?AUTO_SESSION_ID, Value})|Env]
    end.

%% 
%% @doc Set a Key,Value in the session
%%
set_session_data(Key,Value,Env) ->
    Sid = get_session_id(Env),
    erails_session_server:set_session_data(Sid,Key,Value).

%%
%% @doc Return all session data
%%
get_all_session_data(Env) ->
    Sid = get_session_id(Env),
    erails_session_server:get_session_data(Sid).

%%
%% @doc Get the session data for a given key
%%
get_session_data(Key,Env) ->
    proplists:get_value(Key,get_all_session_data(Env)).


%%
%% @doc Remove a Key,Value in the session
%%
remove_session_data(Key,Env) ->
    Sid = get_session_id(Env),
    erails_session_server:remove_session_data(Sid,Key).


%%
%% @doc Return the current requested action
%%
get_action(Env) ->
    proplists:get_value(?AUTO_ACTION_NAME,Env).

%%
%% @doc Warning! Should not be set manually. This is 
%% done automatically in the dispather.
%% @hidden
%%
set_action(Env,Value) ->
    case lists:keysearch(?AUTO_ACTION_NAME,1,Env) of
	{value,_} ->
	    set_value(?AUTO_ACTION_NAME,Value,Env);
	false ->
	    [proplists:property({?AUTO_ACTION_NAME, Value})|Env]
    end.

%% Internal APIs:

%% 
%% @doc Set an Key,Value in the environment.
%% Used internally
%% @hidden
set_value(Key, Val, Env) ->
     lists:keyreplace(Key, 1, Env, {Key, Val}).

