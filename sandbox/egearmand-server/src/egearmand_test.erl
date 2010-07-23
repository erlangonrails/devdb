-module(egearmand_test).

%% @doc 
%% egearmand_test:test()会执行所有Module中的test/0.

-include_lib("eunit/include/eunit.hrl").

-spec(all_test_() -> any()).

all_test_() ->           
    [{module, 'proplists_extensions'},
     {module, 'lists_extensions'}].
