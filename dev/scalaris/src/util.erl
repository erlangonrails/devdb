% @copyright 2007-2010 Konrad-Zuse-Zentrum fuer Informationstechnik Berlin

%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%       http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.

%%% @author Thorsten Schuett <schuett@zib.de>
%%% @doc    Utility Functions.
%%% @end
%% @version $Id: util.erl 953 2010-08-03 13:52:39Z kruber@zib.de $
-module(util).

-author('schuett@zib.de').
-vsn('$Id: util.erl 953 2010-08-03 13:52:39Z kruber@zib.de $').

-include("scalaris.hrl").

-export([escape_quotes/1,
         min/2, max/2, logged_exec/1,
         randomelem/1, pop_randomelem/1, pop_randomelem/2,
         wait_for_unregister/1, get_stacktrace/0, dump/0, dump2/0, dump3/0,
         get_nodes/0, minus/2,
         sleep_for_ever/0, shuffle/1, get_proc_in_vms/1,random_subset/2,
         gb_trees_largest_smaller_than/2, gb_trees_foldl/3, pow/2, parameterized_start_link/2,
         zipfoldl/5,
         split_unique/2, split_unique/3, split_unique/4,
         ssplit_unique/2, ssplit_unique/3, ssplit_unique/4,
         smerge2/2, smerge2/3, smerge2/4,
         for_each_line_in_file/4]).
-export([sup_worker_desc/3, sup_worker_desc/4, sup_supervisor_desc/3, sup_supervisor_desc/4, tc/3]).
-export([get_pids_uid/0]).
-export([get_global_uid/0]).

%% @doc Applies start_link in the given Module with the given Parameters.
-spec parameterized_start_link(Module::module(), Parameters::list()) -> term() | none().
parameterized_start_link(Module, Parameters) ->
    apply(Module, start_link, Parameters).

%% @doc Creates a worker description for a supervisor.
-spec sup_worker_desc(Name::atom() | string(), Module::module(), Function::atom())
        -> {Name::atom() | string(), {Module::module(), Function::atom(), Options::[]},
            permanent, brutal_kill, worker, []}.
sup_worker_desc(Name, Module, Function) ->
    sup_worker_desc(Name, Module, Function, []).

%% @doc Creates a worker description for a supervisor.
-spec sup_worker_desc(Name::atom() | string(), Module::module(), Function::atom(), Options::list())
        -> {Name::atom() | string(), {Module::module(), Function::atom(), Options::list()},
            permanent, brutal_kill, worker, []}.
sup_worker_desc(Name, Module, Function, Options) ->
    {Name, {Module, Function, Options}, permanent, brutal_kill, worker, []}.

%% @doc Creates a supervisor description for a supervisor.
-spec sup_supervisor_desc(Name::atom() | string(), Module::module(), Function::atom())
        -> {Name::atom() | string(), {Module::module(), Function::atom(), Options::[]},
            permanent, brutal_kill, supervisor, []}.
sup_supervisor_desc(Name, Module, Function) ->
    sup_supervisor_desc(Name, Module, Function, []).

%% @doc Creates a supervisor description for a supervisor.
-spec sup_supervisor_desc(Name::atom() | string(), Module::module(), Function::atom(), Options::list())
        -> {Name::atom() | string(), {Module::module(), Function::atom(), Options::list()},
            permanent, brutal_kill, supervisor, []}.
sup_supervisor_desc(Name, Module, Function, Args) ->
    {Name, {Module, Function, Args}, permanent, brutal_kill, supervisor, []}.

%% @doc Escapes quotes in the given string.
-spec escape_quotes(String::string()) -> string().
escape_quotes(String) ->
    lists:foldr(fun escape_quotes_/2, [], String).

-spec escape_quotes_(String::string(), Rest::string()) -> string().
escape_quotes_($", Rest) -> [$\\, $" | Rest];
escape_quotes_(Ch, Rest) -> [Ch | Rest].

-spec max(plus_infinity, any()) -> plus_infinity;
         (any(), plus_infinity) -> plus_infinity;
         (T, T) -> T.
max(plus_infinity, _) -> plus_infinity;
max(_, plus_infinity) -> plus_infinity;
max(minus_infinity, X) -> X;
max(X, minus_infinity) -> X;
max(A, B) ->
    case A > B of
        true -> A;
        false -> B
    end.

-spec min(minus_infinity, any()) -> minus_infinity;
         (any(), minus_infinity) -> minus_infinity;
         (T, T) -> T.
min(minus_infinity, _) -> minus_infinity;
min(_, minus_infinity) -> minus_infinity;
min(plus_infinity, X) -> X;
min(X, plus_infinity) -> X;
min(A, B) ->
    case A < B of
        true -> A;
        false -> B
    end.

-spec pow(integer(), non_neg_integer()) -> integer();
         (float(), non_neg_integer()) -> number().
pow(_X, 0) ->
    1;
pow(X, 1) ->
    X;
pow(X, 2) ->
    X * X;
pow(X, 3) ->
    X * X * X;
pow(X, Y) ->
    case Y rem 2 of
        0 ->
            Half = pow(X, Y div 2),
            Half * Half;
        1 ->
            Half = pow(X, Y div 2),
            Half * Half * X
    end.

-spec logged_exec(Cmd::string() | atom()) -> ok.
logged_exec(Cmd) ->
    Output = os:cmd(Cmd),
    OutputLength = length(Output),
    if
        OutputLength > 10 ->
            log:log(info, "exec", Cmd),
            log:log(info, "exec", Output),
            ok;
        true ->
            ok
    end.

-spec wait_for_unregister(atom()) -> ok.
wait_for_unregister(PID) ->
    case whereis(PID) of
        undefined ->
            ok;
        _ ->
            wait_for_unregister(PID)
    end.

-spec get_stacktrace() -> [{Module::atom(), Function::atom(), ArityOrArgs::integer() | [term()]}].
get_stacktrace() ->
    erlang:get_stacktrace().

%% @doc Extracts a given ItemInfo from an ItemList that has been returned from
%%      e.g. erlang:process_info/2 for the dump* methods.
-spec dump_extract_from_list
        ([{Item::atom(), Info::term()}], ItemInfo::memory | message_queue_len | stack_size | heap_size) -> non_neg_integer();
        ([{Item::atom(), Info::term()}], ItemInfo::messages) -> [tuple()];
        ([{Item::atom(), Info::term()}], ItemInfo::current_function) -> Fun::{Module::atom(), FunName::atom(), Arity::non_neg_integer()}.
dump_extract_from_list(List, Key) ->
    element(2, lists:keyfind(Key, 1, List)).

%% @doc Returns a list of all currently executed functions and the number of
%%      instances for each of them.
-spec dump() -> [{Fun::{Module::atom(), FunName::atom(), Arity::non_neg_integer()}, FunExecCount::pos_integer()}].
dump() ->
    Info = [element(2, Fun) || X <- processes(),
                               Fun <- [process_info(X, current_function)],
                               Fun =/= [undefined]],
    FunCnt = dict:to_list(lists:foldl(fun(Fun, DictIn) ->
                                              dict:update_counter(Fun, 1, DictIn)
                                      end, dict:new(), Info)),
    lists:reverse(lists:keysort(2, FunCnt)).

%% @doc Returns information about all processes' memory usage.
-spec dump2() -> [{PID::pid(), Mem::non_neg_integer(), Fun::{Module::atom(), FunName::atom(), Arity::non_neg_integer()}}].
dump2() ->
    Info = 
        [{Pid,
          dump_extract_from_list(Data, memory),
          dump_extract_from_list(Data, current_function)}
        || Pid <- processes(),
           Data <- [process_info(Pid, [memory, current_function])],
           Data =/= undefined],
    lists:reverse(lists:keysort(2, Info)).

%% @doc Returns various data about all processes.
-spec dump3() -> [{PID::pid(), Mem::non_neg_integer(), MsgQLength::non_neg_integer(),
                   StackSize::non_neg_integer(), HeapSize::non_neg_integer(),
                   Messages::[atom()],
                   Fun::{Module::atom(), FunName::atom(), Arity::non_neg_integer()}}].
dump3() ->
    Info = 
        [{Pid,
          dump_extract_from_list(Data, memory),
          dump_extract_from_list(Data, message_queue_len),
          dump_extract_from_list(Data, stack_size),
          dump_extract_from_list(Data, heap_size),
          [element(1, Y) || Y <- dump_extract_from_list(Data, messages)],
          dump_extract_from_list(Data, current_function)}
        || Pid <- processes(),
           Data <- [process_info(Pid, [memory, message_queue_len, stack_size,
                                       heap_size, messages, current_function])],
           Data =/= undefined],
    lists:reverse(lists:keysort(2, Info)).

%% @doc minus(M,N) : { x | x in M and x notin N}
-spec minus([T], [T]) -> [T].
minus([], _ExcludeList) ->
    [];
minus([_|_] = L, ExcludeList) ->
    ExcludeSet = ordsets:from_list(ExcludeList),
    [E || E <- L, not ordsets:is_element(E, ExcludeSet)].

-spec get_nodes() -> [comm:mypid()].
get_nodes() ->
    get_proc_in_vms(bench_server).

-spec get_proc_in_vms(atom()) -> [comm:mypid()].
get_proc_in_vms(Proc) ->
    boot_server:node_list(),
    Nodes =
        receive
            {get_list_response, X} -> X
        after 2000 ->
            {failed}
        end,
    lists:usort([comm:get(Proc, DHTNode) || DHTNode <- Nodes]).

-spec sleep_for_ever() -> none().
sleep_for_ever() ->
    timer:sleep(5000),
    sleep_for_ever().

%% @doc Returns a random element from the given (non-empty!) list according to
%%      a uniform distribution.
-spec randomelem(List::[X,...]) -> X.
randomelem(List)->
    Length = length(List) + 1,
    RandomNum = randoms:rand_uniform(1, Length),
    lists:nth(RandomNum, List).
    
%% @doc Removes a random element from the (non-empty!) list and returns the
%%      resulting list and the removed element.
-spec pop_randomelem(List::[X,...]) -> {NewList::[X], PoppedElement::X}.
pop_randomelem(List) ->
    pop_randomelem(List, length(List)).
    
%% @doc Removes a random element from the first Size elements of a (non-empty!)
%%      list and returns the resulting list and the removed element. 
-spec pop_randomelem(List::[X,...], Size::non_neg_integer()) -> {NewList::[X], PoppedElement::X}.
pop_randomelem(List, Size) ->
    {Leading, [H | T]} = lists:split(randoms:rand_uniform(0, Size), List),
    {lists:append(Leading, T), H}.

%% @doc Returns a random subset of Size elements from the given list.
-spec random_subset(Size::pos_integer(), [T]) -> [T].
random_subset(0, _List) ->
    % having this special case here prevents unnecessary calls to erlang:length()
    [];
random_subset(Size, List) ->
    ListSize = length(List),
    shuffle_helper(List, [], Size, ListSize).

%% @doc Fisher-Yates shuffling for lists.
-spec shuffle([T]) -> [T].
shuffle(List) ->
    ListSize = length(List),
    shuffle_helper(List, [], ListSize, ListSize).

%% @doc Fisher-Yates shuffling for lists helper function: creates a shuffled
%%      list of length ShuffleSize.
-spec shuffle_helper(List::[T], AccResult::[T], ShuffleSize::non_neg_integer(), ListSize::non_neg_integer()) -> [T].
shuffle_helper([], Acc, _Size, _ListSize) ->
    Acc;
shuffle_helper([_|_] = _List, Acc, 0, _ListSize) ->
    Acc;
shuffle_helper([_|_] = List, Acc, Size, ListSize) ->
    {Leading, [H | T]} = lists:split(randoms:rand_uniform(0, ListSize), List),
    shuffle_helper(lists:append(Leading, T), [H | Acc], Size - 1, ListSize - 1).

%% @doc Find the largest key in GBTree that is smaller than Key.
%%      Note: gb_trees offers only linear traversal or lookup of exact keys -
%%      we implement a more flexible binary search here despite gb_tree being
%%      defined as opaque.
-spec gb_trees_largest_smaller_than(Key, gb_tree()) -> {value, Key, Value::any()} | nil.
gb_trees_largest_smaller_than(_Key, {0, _Tree}) ->
    nil;
gb_trees_largest_smaller_than(MyKey, {_Size, InnerTree}) ->
    gb_trees_largest_smaller_than_iter(MyKey, InnerTree, true).

-spec gb_trees_largest_smaller_than_iter(Key, {Key, Value, Smaller::term(), Bigger::term()}, RightTree::boolean()) -> {value, Key, Value} | nil.
gb_trees_largest_smaller_than_iter(_SearchKey, nil, _RightTree) ->
    nil;
gb_trees_largest_smaller_than_iter(SearchKey, {Key, Value, Smaller, Bigger}, RightTree) ->
    case Key < SearchKey of
        true when RightTree andalso Bigger =:= nil ->
            % we reached the right end of the whole tree
            % -> there is no larger item than the current item
            {value, Key, Value};
        true ->
            case gb_trees_largest_smaller_than_iter(SearchKey, Bigger, RightTree) of
                {value, _, _} = AValue -> AValue;
                nil -> {value, Key, Value}
            end;
        _ ->
            gb_trees_largest_smaller_than_iter(SearchKey, Smaller, false)
    end.

%% @doc Foldl over gb_trees.
-spec gb_trees_foldl(fun((Key::any(), Value::any(), Acc) -> Acc), Acc, gb_tree()) -> Acc.
gb_trees_foldl(F, Acc, GBTree) ->
    gb_trees_foldl_iter(F, Acc, gb_trees:next(gb_trees:iterator(GBTree))).

-spec gb_trees_foldl_iter(fun((Key, Value, Acc) -> Acc), Acc,
                          {Key, Value, Iter::term()} | none) -> Acc.
gb_trees_foldl_iter(_F, Acc, none) ->
    Acc;
gb_trees_foldl_iter(F, Acc, {Key, Val, Iter}) ->
    gb_trees_foldl_iter(F, F(Key, Val, Acc), gb_trees:next(Iter)).

-spec tc(module(), atom(), list()) -> {integer(), any()}.
tc(M, F, A) ->
    Before = erlang:now(),
    Val = apply(M, F, A),
    After = erlang:now(),
    {timer:now_diff(After, Before), Val}.

-spec get_pids_uid() -> pos_integer().
get_pids_uid() ->
    Result = case erlang:get(pids_uid_counter) of
                 undefined -> 1;
                 Any -> Any + 1
             end,
    erlang:put(pids_uid_counter, Result),
    Result.

-spec get_global_uid() -> {pos_integer(), comm:mypid()}.
get_global_uid() ->
    _Result = {get_pids_uid(), comm:this()}
    %% , term_to_binary(_Result)
    .

-spec zipfoldl(ZipFun::fun((X, Y) -> Z), FoldFun::fun((Z, Acc) -> Acc), L1::[X], L2::[Y], Acc) -> Acc.
zipfoldl(ZipFun, FoldFun, [L1H | L1R], [L2H | L2R], AccIn) ->
    zipfoldl(ZipFun, FoldFun, L1R, L2R, FoldFun(ZipFun(L1H, L2H), AccIn));
zipfoldl(_ZipFun, _FoldFun, [], [], AccIn) ->
    AccIn.

%% @doc Splits L1 into a list of elements that are not contained in L2, a list
%%      of elements that both lists share and a list of elements unique to L2.
%%      Returned lists are sorted and contain no duplicates.
-spec split_unique(L1::[X], L2::[X]) -> {UniqueL1::[X], Shared::[X], UniqueL2::[X]}.
split_unique(L1, L2) ->
    split_unique(L1, L2, fun erlang:'=<'/2).

%% @doc Splits L1 into a list of elements that are not contained in L2, a list
%%      of elements that are equal in both lists (according to the ordering
%%      function Lte) and a list of elements unique to L2.
%%      When two elements compare equal, the element from List1 is picked.
%%      Lte(A, B) should return true if A compares less than or equal to B in
%%      the ordering, false otherwise.
%%      Returned lists are sorted according to Lte and contain no duplicates.
-spec split_unique(L1::[X], L2::[X], Lte::fun((X, X) -> boolean())) -> {UniqueL1::[X], Shared::[X], UniqueL2::[X]}.
split_unique(L1, L2, Lte) ->
    split_unique(L1, L2, Lte, fun(E1, _E2) -> E1 end).

%% @doc Splits L1 into a list of elements that are not contained in L2, a list
%%      of elements that are equal in both lists (according to the ordering
%%      function Lte) and a list of elements unique to L2.
%%      When two elements compare equal, EqSelect(element(L1), element(L2))
%%      chooses which of them to take.
%%      Lte(A, B) should return true if A compares less than or equal to B in
%%      the ordering, false otherwise.
%%      Returned lists are sorted according to Lte and contain no duplicates.
-spec split_unique(L1::[X], L2::[X], Lte::fun((X, X) -> boolean()), EqSelect::fun((X, X) -> X)) -> {UniqueL1::[X], Shared::[X], UniqueL2::[X]}.
split_unique(L1, L2, Lte, EqSelect) ->
    L1Sorted = lists:usort(Lte, L1),
    L2Sorted = lists:usort(Lte, L2),
    ssplit_unique_helper(L1Sorted, L2Sorted, Lte, EqSelect, {[], [], []}).

%% @doc Splits L1 into a list of elements that are not contained in L2, a list
%%      of elements that both lists share and a list of elements unique to L2.
%%      Both lists must be sorted. Returned lists are sorted as well.
-spec ssplit_unique(L1::[X], L2::[X]) -> {UniqueL1::[X], Shared::[X], UniqueL2::[X]}.
ssplit_unique(L1, L2) ->
    ssplit_unique(L1, L2, fun erlang:'=<'/2).

%% @doc Splits L1 into a list of elements that are not contained in L2, a list
%%      of elements that are equal in both lists (according to the ordering
%%      function Lte) and a list of elements unique to L2.
%%      When two elements compare equal, the element from List1 is picked.
%%      Both lists must be sorted according to Lte. Lte(A, B) should return
%%      true if A compares less than or equal to B in the ordering, false
%%      otherwise.
%%      Returned lists are sorted according to Lte.
-spec ssplit_unique(L1::[X], L2::[X], Lte::fun((X, X) -> boolean())) -> {UniqueL1::[X], Shared::[X], UniqueL2::[X]}.
ssplit_unique(L1, L2, Lte) ->
    ssplit_unique(L1, L2, Lte, fun(E1, _E2) -> E1 end).

%% @doc Splits L1 into a list of elements that are not contained in L2, a list
%%      of elements that are equal in both lists (according to the ordering
%%      function Lte) and a list of elements unique to L2.
%%      When two elements compare equal, EqSelect(element(L1), element(L2))
%%      chooses which of them to take.
%%      Both lists must be sorted according to Lte. Lte(A, B) should return true
%%      if A compares less than or equal to B in the ordering, false otherwise.
%%      Returned lists are sorted according to Lte.
-spec ssplit_unique(L1::[X], L2::[X], Lte::fun((X, X) -> boolean()), EqSelect::fun((X, X) -> X)) -> {UniqueL1::[X], Shared::[X], UniqueL2::[X]}.
ssplit_unique(L1, L2, Lte, EqSelect) ->
    ssplit_unique_helper(L1, L2, Lte, EqSelect, {[], [], []}).

%% @doc Helper function for ssplit_unique/4.
-spec ssplit_unique_helper(L1::[X], L2::[X], Lte::fun((X, X) -> boolean()), EqSelect::fun((X, X) -> X), {UniqueOldL1::[X], SharedOld::[X], UniqueOldL2::[X]}) -> {UniqueL1::[X], Shared::[X], UniqueL2::[X]}.
ssplit_unique_helper(L1 = [H1 | T1], L2 = [H2 | T2], Lte, EqSelect, {UniqueL1, Shared, UniqueL2}) ->
    LteH1H2 = Lte(H1, H2),
    LteH2H1 = Lte(H2, H1),
    case LteH1H2 andalso LteH2H1 of
        true ->
            ssplit_unique_helper(T1, L2, Lte, EqSelect, {UniqueL1, [EqSelect(H1, H2) | Shared], UniqueL2});
        false when LteH1H2 ->
            ssplit_unique_helper(T1, L2, Lte, EqSelect, {[H1 | UniqueL1], Shared, UniqueL2});
        false when LteH2H1 ->
            % the top of the shared list could be the same as the top of L2!
            case (Shared =:= []) orelse not (Lte(hd(Shared), H2) andalso Lte(H2, hd(Shared))) of
                true  -> ssplit_unique_helper(L1, T2, Lte, EqSelect, {UniqueL1, Shared, [H2 | UniqueL2]});
                false -> ssplit_unique_helper(L1, T2, Lte, EqSelect, {UniqueL1, Shared, UniqueL2})
            end
    end;
ssplit_unique_helper(L1, [], _Lte, _EqSelect, {UniqueL1, Shared, UniqueL2}) ->
    {lists:reverse(UniqueL1, L1), lists:reverse(Shared), lists:reverse(UniqueL2)};
ssplit_unique_helper([], L2 = [H2 | T2], Lte, EqSelect, {UniqueL1, Shared, UniqueL2}) ->
    % the top of the shared list could be the same as the top of L2 since
    % elements are only removed from L2 if an element of L1 is larger
    case Shared =:= [] orelse not (Lte(hd(Shared), H2) andalso Lte(H2, hd(Shared))) of
        true  ->
            {lists:reverse(UniqueL1), lists:reverse(Shared), lists:reverse(UniqueL2, L2)};
        false ->
            ssplit_unique_helper([], T2, Lte, EqSelect, {UniqueL1, Shared, UniqueL2})
    end.

-spec smerge2(L1::[X], L2::[X]) -> MergedList::[X].
smerge2(L1, L2) ->
    smerge2(L1, L2, fun erlang:'=<'/2).

-spec smerge2(L1::[X], L2::[X], Lte::fun((X, X) -> boolean())) -> MergedList::[X].
smerge2(L1, L2, Lte) ->
    smerge2(L1, L2, Lte, fun(E1, _E2) -> [E1] end).

-spec smerge2(L1::[X], L2::[X], Lte::fun((X, X) -> boolean()), EqSelect::fun((X, X) -> [X])) -> MergedList::[X].
smerge2(L1, L2, Lte, EqSelect) ->
    smerge2_helper(L1, L2, Lte, EqSelect, []).

%% @doc Helper function for merge2/4.
-spec smerge2_helper(L1::[X], L2::[X], Lte::fun((X, X) -> boolean()), EqSelect::fun((X, X) -> [X]), OldMergedList::[X]) -> MergedList::[X].
smerge2_helper(L1 = [H1 | T1], L2 = [H2 | T2], Lte, EqSelect, ML) ->
    LteH1H2 = Lte(H1, H2),
    LteH2H1 = Lte(H2, H1),
    case LteH1H2 andalso LteH2H1 of
        true ->
            smerge2_helper(T1, L2, Lte, EqSelect, lists:reverse(EqSelect(H1, H2)) ++ ML);
        false when LteH1H2 ->
            smerge2_helper(T1, L2, Lte, EqSelect, [H1 | ML]);
        false when LteH2H1 ->
            % the top of ML could be equal to the top of L2 (if so, the decision
            % about H2 has already been made and we omit it here, otherwise H2
            % needs to be added)
            case (ML =:= []) orelse not (Lte(hd(ML), H2) andalso Lte(H2, hd(ML))) of
                true  -> smerge2_helper(L1, T2, Lte, EqSelect, [H2 | ML]);
                false -> smerge2_helper(L1, T2, Lte, EqSelect, ML) 
            end
    end;
smerge2_helper(L1, [], _Lte, _EqSelect, ML) ->
    lists:reverse(ML, L1);
smerge2_helper([], L2 = [H2 | T2], Lte, EqSelect, ML) ->
    % The top of ML could be equal to the top of L2 (if so, the decision about
    % H2 has already been made and we omit it here, otherwise H2 needs to be
    % added).
    % This is because elements are only removed from L2 if an element of L1 is
    % larger.
    case ML =:= [] orelse not (Lte(hd(ML), H2) andalso Lte(H2, hd(ML))) of
        true  -> lists:reverse(ML, L2);
        false -> smerge2_helper([], T2, Lte, EqSelect, ML)
    end.

-spec for_each_line_in_file(Name::file:name(), Proc::fun((string(), AccT) -> AccT),
                            Mode::[read | write | append | raw | binary |
                                   {delayed_write, Size::integer(), Delay::integer()} |
                                   delayed_write | {read_ahead, Size::integer()} |
                                   read_ahead | compressed],
                            AccT) -> AccT.
for_each_line_in_file(Name, Proc, Mode, Accum0) ->
    {ok, Device} = file:open(Name, Mode),
    for_each_line(Device, Proc, Accum0).

-spec for_each_line(Device::file:io_device(), Proc::fun((string(), AccT) -> AccT), AccT) -> AccT.
for_each_line(Device, Proc, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> 
        CleanLine = string:substr(Line, 1, string:len(Line) - 1),
        NewAccum = Proc(CleanLine, Accum),
        for_each_line(Device, Proc, NewAccum)
    end.
