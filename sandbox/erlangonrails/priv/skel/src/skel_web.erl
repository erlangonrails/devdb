%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for skel.

-module(skel_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).
-include("erlangonrails.hrl").

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, _DocRoot) ->
    %% Setup env...
    InitialEnv = erails_env:setup(Req),
    Env = erails_session:setup(InitialEnv),
    

    %% Possible return values
    %% {render, View, Data}
    %% {render, View, Data, Options}
    %% {text, Data}
    %% {redirect, Url}
    %% {static, File}
    %% {error, _} 

    %% TODO:
    %% {json, data}
    case erails_router:dispatch(Env) of
	{render,View,Data} ->
	    Content = render_template(View,Data,Env),
	    Req:respond({200,
			 [{"Content-Type","text/html"}|[erails_cookie:gen_cookie_of_session(Env)]],
			 Content});
	{render,View,Data,Options} ->
	    {Status,ContentType,Headers} = extract_options(Options),
	    Content = render_template(View,Data,Env),
	    Req:respond({Status,
			 [{"Content-Type",ContentType}|[erails_cookie:gen_cookie_of_session(Env)|Headers]],
			 Content});
	{text,Content} ->
	    Req:respond({200,
			 [{"Content-Type","text/plain"}|[erails_cookie:gen_cookie_of_session(Env)]],
			 Content});
	{redirect,Url} ->
	    Req:respond({302, 
                         [{"Location", Url}, 
                          {"Content-Type", "text/html; charset=UTF-8"}], 
                         ""});
	{static, File} ->
	    "/" ++ StaticFile = File,
	    Req:serve_file(StaticFile, filename:join(skel_deps:get_base_dir(), "priv/www"));
	{error,_} ->
	    Req:respond({500,[],"Server Error "})
    end.

render_template(ViewFile,Data,Env) -> 
    %% Copy flash into Data and clear from Session
    Data1 = set_and_clear_flash(Data,Env),
    FullPathToFile = filename:join([skel_deps:get_base_dir(), "src/views", ViewFile]),
    erails_render:run(FullPathToFile, ViewFile, Data1).

%% {Status, ContentType, Headers}
extract_options(Options) ->
    {proplists:get_value(status,Options,200),
     proplists:get_value(content_type,Options,"text/html"),
     proplists:get_value(headers,Options,[])}.

set_and_clear_flash(Data, Env) ->
    case erails_var:get_flash(Env) of
	none -> Data;
	Flash ->
	    [{flash, Flash} | Data]
    end.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

