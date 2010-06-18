%%% File    : e2d_test.erl
%%% Author  : litao <litaocheng@gmail.com>
%%% Description : test all the modules
%%% Created :  2 Jan 2009 by litao <>

-module(e2d_test).
-export([start/0]).


start() ->
    e2d_membership:test(),
    ok.
