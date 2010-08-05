-module(erails_skel).
-export([skelcopy/2]).
-include_lib("kernel/include/file.hrl").


%% External API
%% Usage:
%% skelcopy("/var/erails", "blog")

skelcopy(DestDir, AppName) ->
    ok = ensuredir(DestDir),
    Name = string:to_lower(AppName),
    DepsDestDir = DestDir ++ "/" ++ Name ++ "/deps",
    
    LDst = case length(filename:dirname(DestDir)) of
               1 -> %% handle case when dirname returns "/"
                   0;
               N ->
                   N + 1
           end,

    %% copy priv/skel
    skelcopy(src(), DestDir, Name, LDst),

    %% copy deps/*
    io:format("~s~n", [lists:nthtail(LDst, DepsDestDir ++ "/mochiweb/*")]),
    io:format("~s~n", [lists:nthtail(LDst, DepsDestDir ++ "/erlydtl/*")]),
    io:format("~s~n", [lists:nthtail(LDst, DepsDestDir ++ "/erlangonrails/*")]),
    depscopy(mochiweb_src(), DepsDestDir ++ "/mochiweb"),
    depscopy(erlydtl_src(), DepsDestDir ++ "/erlydtl"),
    erailscopy(erlangonrails_src(), DepsDestDir ++ "/erlangonrails").

%% Internal API

%% e.g. 
%% /home/username/erlangonrails/ebin/../priv/skel
src() ->
    Dir = filename:dirname(code:which(?MODULE)),
    filename:join(Dir, "../priv/skel").

mochiweb_src() ->
    Dir = filename:dirname(code:which(?MODULE)),
    filename:join(Dir, "../deps/mochiweb").

erlydtl_src() ->
    Dir = filename:dirname(code:which(?MODULE)),
    filename:join(Dir, "../deps/erlydtl").

erlangonrails_src() ->
    Dir = filename:dirname(code:which(?MODULE)),
    filename:join(Dir, "..").

skel() ->
    "skel".

skelcopy(Src, DestDir, Name, LDst) ->
    Dest = re:replace(filename:basename(Src), skel(), Name, [global, {return, list}]),
    case file:read_file_info(Src) of
        {ok, #file_info{type=directory, mode=Mode}} ->
            Dir = DestDir ++ "/" ++ Dest,
            EDst = lists:nthtail(LDst, Dir),
            ok = ensuredir(Dir),
            ok = file:write_file_info(Dir, #file_info{mode=Mode}),
            case filename:basename(Src) of
		%% skip the ebin directory
                "ebin" ->
                    ok;
                _ ->
                    {ok, Files} = file:list_dir(Src),
                    io:format("~s/~n", [EDst]),
		    %% skip the hide files: .*
                    lists:foreach(fun ("." ++ _) -> ok;
                                      (F) ->
                                          skelcopy(filename:join(Src, F),
                                                   Dir,
                                                   Name,
                                                   LDst)
                                  end,
                                  Files),
                        ok
            end;
        {ok, #file_info{type=regular, mode=Mode}} ->
            OutFile = filename:join(DestDir, Dest),
            {ok, B} = file:read_file(Src),
            S = re:replace(binary_to_list(B), skel(), Name,
                           [{return, list}, global]),
            ok = file:write_file(OutFile, list_to_binary(S)),
            ok = file:write_file_info(OutFile, #file_info{mode=Mode}),
            io:format("    ~s~n", [filename:basename(Src)]),
            ok;
        {ok, _} ->
            io:format("ignored source file: ~p~n", [Src]),
            ok;
	{error, _} ->
	    io:format("skelcopy error: ~p~n", [Src]),
	    ok
    end.

depscopy(Src, DestDir) ->
    case file:read_file_info(Src) of
        {ok, #file_info{type=directory, mode=Mode}} ->
            ok = ensuredir(DestDir),
            ok = file:write_file_info(DestDir, #file_info{mode=Mode}),
            case filename:basename(Src) of
                "ebin" ->
                    ok;
                _ ->
                    {ok, Files} = file:list_dir(Src),
                    lists:foreach(fun ("." ++ _) -> ok;
                                      (F) ->
                                          depscopy(filename:join(Src, F),
						   filename:join(DestDir, F))
                                  end,
                                  Files),
                        ok
            end;
        {ok, #file_info{type=regular, mode=Mode}} ->
            {ok, B} = file:read_file(Src),
            ok = file:write_file(DestDir, B),
            ok = file:write_file_info(DestDir, #file_info{mode=Mode}),
            ok;
        {ok, _} ->
            ok;
	{error, _} ->
	    io:format("depscopy error: ~p~n", [Src]),
	    ok
    end.

erailscopy(Src, DestDir) ->
    case file:read_file_info(Src) of
        {ok, #file_info{type=directory, mode=Mode}} ->
            case filename:basename(Src) of
                "ebin" ->
		    ok = ensuredir(DestDir),
                    ok = file:write_file_info(DestDir, #file_info{mode=Mode}),
                    ok;
		"deps" ->
		    ok;
		"doc" ->
		    ok;
		"priv" ->
		    ok;
		"script" ->
		    ok;
		"example" ->
		    ok;
                _ ->
		    ok = ensuredir(DestDir),
                    ok = file:write_file_info(DestDir, #file_info{mode=Mode}),
                    {ok, Files} = file:list_dir(Src),
                    lists:foreach(fun ("." ++ _) -> ok;
                                      (F) ->
                                          erailscopy(filename:join(Src, F),
						     filename:join(DestDir, F))
                                  end,
                                  Files),
                        ok
            end;
        {ok, #file_info{type=regular, mode=Mode}} ->
            {ok, B} = file:read_file(Src),
            ok = file:write_file(DestDir, B),
            ok = file:write_file_info(DestDir, #file_info{mode=Mode}),
            ok;
        {ok, _} ->
            ok;
	{error, _} ->
	    io:format("erailscopy error: ~p~n", [Src]),
	    ok
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
