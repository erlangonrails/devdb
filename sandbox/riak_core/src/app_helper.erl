-module(app_helper).

-export([get_env/1, get_env/2, get_env/3]).

%%
%% 获取application配置选项的helper模块
%%

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Retrieve all Key/Value pairs in the env for the specified app.
-spec get_env(App :: atom()) -> [{Key :: atom(), Value :: term()}].
get_env(App) ->
    application:get_all_env(App).

%% @doc The official way to get a value from the app's env.
%%      Will return the 'undefined' atom if that key is unset.
-spec get_env(App :: atom(), Key :: atom()) -> term().
get_env(App, Key) ->
    get_env(App, Key, undefined).

%% @doc The official way to get a value from this application's env.
%%      Will return Default if that key is unset.
-spec get_env(App :: atom(), Key :: atom(), Default :: term()) -> term().
get_env(App, Key, Default) ->
    case application:get_env(App, Key) of
	{ok, Value} ->
            Value;
        _ ->
            Default
    end.
