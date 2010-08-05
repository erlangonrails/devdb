-module(erails_gen).
-export([new_controller/3]).
-include_lib("kernel/include/file.hrl").

%% External APIs:
new_controller(BaseDir, Controller, AppName) ->
    gen_controller(BaseDir, Controller, AppName),
    gen_view(BaseDir, Controller, AppName).

%% Internal APIs:
gen_controller(BaseDir, Controller, AppName) ->
    ControllerTpl = filename:join([BaseDir, "support", AppName ++ "_controller.tpl"]),
    ControllerRes = filename:join([BaseDir, "src/controllers", string:to_lower(Controller) ++ "_controller.erl"]),
    case file:read_file_info(ControllerTpl) of
	{ok, #file_info{type=regular, mode=Mode}} ->
	    {ok, B} = file:read_file(ControllerTpl),
            S = re:replace(binary_to_list(B), "xxxxxx", Controller,
                           [{return, list}, global]),
            ok = file:write_file(ControllerRes, list_to_binary(S)),
            ok = file:write_file_info(ControllerRes, #file_info{mode=Mode}),
	    io:format("gen controller ~p success!~n", [Controller]),
            ok;
	_ ->
	    io:format("gen controller ~p error!~n", [Controller]),
	    error
    end.

gen_view(BaseDir, View, AppName) ->
    ViewTpl = filename:join([BaseDir, "support", AppName ++ ".tpl"]),
    ok = ensuredir(filename:join([BaseDir, "src/views", View])),
    ViewRes = filename:join([BaseDir, "src/views", View, "index.html"]),
    case file:read_file_info(ViewTpl) of
	{ok, #file_info{type=regular, mode=Mode}} ->
	    {ok, B} = file:read_file(ViewTpl),
            ok = file:write_file(ViewRes, B),
            ok = file:write_file_info(ViewRes, #file_info{mode=Mode}),
	    io:format("gen view ~p success!~n", [View]),
            ok;
	_ ->
	    io:format("gen view ~p error!~n", [View]),
	    error
    end.

ensuredir(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        E ->
            E
    end.
    
