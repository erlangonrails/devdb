-module(nick_sync_repro).
-export([init/0, run/0, run_repro/0]).

-define(BIN_PATH, "../../../src").

%% Usage:
%% erlc bug_sync_repro.erl
%% erl -pa ../../../src
%% nick_sync_repro:init()
%% nick_sync_repro:run()
%% nick_sync_repro:run_repro()  %% 重现Bug


init() ->
    erl_ddll:load_driver(?BIN_PATH, "expat_erl").

run() ->
    io:format("xml#~p~n", [xml_doc()]),
    xml_stream:parse_element(xml_doc()).

run_repro() ->
    io:format("xml#~p~n", [xml_doc_repro()]),
    xml_stream:parse_element(xml_doc_repro()).

%% Internal APIs:
xml_doc_repro() ->
    "<stream>" ++
      "<testtag attr1='val1'>、?﹏丶小雪 </testtag>" ++
    "</stream>".

xml_doc() ->
    "<stream>" ++
      "<testtag attr1='val1'>data</testtag>" ++
    "</stream>".
