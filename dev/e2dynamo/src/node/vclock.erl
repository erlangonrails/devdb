%%% vector clock
%%% reference : http://en.wikipedia.org/wiki/Vector_Clock
%%% @author litaocheng@gmail.com
%%% @copyright toquick.com

-module(vclock).
-author('litaocheng@gmail.com').
-vsn('0.2').
-include("vclock.hrl").

-export([new/0, increase/2, descends/2, merge/2, merge/1, extend/2]).
-export([size/1, get_counter/2, get_stamp/2]).

%% @doc return an empty vector clock
-spec new() -> vclock().
new() ->
    [].

%% @doc increase node's counter
-spec increase(Node :: vc_node(), VClock:: vclock()) -> vclock().
increase(Node, VClock) ->
    Entry2 =
        case get_entry(Node, VClock) of
            {value, Entry = #vc_entry{counter = Counter}} ->
                Entry#vc_entry{counter = Counter +1, stamp = stamp()};
            false ->
                #vc_entry{node = Node, counter = 1, stamp = stamp()}
        end,
    lists:keystore(Node, 1, VClock, Entry2).

%% @doc if the Va descends from the Vb return true,
%%   if vector clocks can't syntactically reconciled, return divergent,
%%   otherwise return false
-spec descends(Va :: vclock(), Vb :: vclock()) -> 'true' | 'false' | 'divergent'.
descends(_Va, []) ->
    % all the clocks descend from the empty clock
    true;
descends(Va, [#vc_entry{node = NodeB, counter = CoutnerB} | Vb]) ->
    CounterA =
        case get_entry(NodeB, Va) of
            {value, #vc_entry{counter = Counter}} ->
                Counter;
            false ->
                0
        end,
    case CounterA of
        0 -> divergent;
        C when is_integer(C) ->
            if
                CounterA < CoutnerB ->
                    false;
                true ->
                    descends(Va, Vb)
            end
    end.

%% @doc merge the two clocks
-spec merge(Va :: vclock(), Vb :: vclock()) -> vclock().
merge(Va, Vb) ->
    merge([Va | [Vb]]).

%% @doc merge the two clocks
-spec merge(Vs :: [vclock()]) -> vclock().
merge(Vs) ->
    merge1(Vs, []).

merge1([], NVClock) -> NVClock;
merge1([VClock | VRest], NVClock) ->
    merge1(VRest, lists:foldl(fun(E, A) -> extend(E, A) end, NVClock, VClock)).

%% @doc add the new entry to vector clock
-spec extend(Entry :: vc_entry(), VClock :: vclock()) -> vclock().
extend(#vc_entry{} = Entry, []) ->
    [Entry];
extend(Entry = #vc_entry{node = Node, counter = Counter, stamp = Stamp},
       VClock) ->
    Entry2 =
        case get_entry(Node, VClock) of
            {value, Etr = #vc_entry{counter = C2}} ->
                if Counter > C2 -> Etr#vc_entry{counter = Counter, stamp = Stamp};
                   true -> Etr
                end;
            false ->
                Entry
        end,
    lists:keystore(Node, #vc_entry.node, VClock, Entry2).

%% @doc return number of entry in vector clock
-spec size(V :: vclock()) -> non_neg_integer().
size(V) ->
    length(V).

%% @doc return the node's Counter
-spec get_counter(Node :: vc_node(), V :: vclock()) -> {'value', pos_integer()} | 'false'.
get_counter(Node, V) ->
    case get_entry(Node, V) of
        {value, #vc_entry{counter = Counter}} ->
            Counter;
        false ->
            false
    end.

%% @doc return the node's last access stamp
-spec get_stamp(Node :: vc_node(), V :: vclock()) -> {'value', pos_integer()} | 'false'.
get_stamp(Node, V) ->
    case get_entry(Node, V) of
        {value, #vc_entry{stamp = Stamp}} ->
            {value, Stamp};
        false ->
            false
    end.

%%
%% internal API
%%

%% get one entry from the vector clock pairs
get_entry(Node, VClock) ->
    lists:keysearch(Node, #vc_entry.node, VClock).

stamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

-ifdef(debug).
test_() ->
        A = vclock:new(),
        B = vclock:new(),
        A1 = vclock:increase(a, A),
        B1 = vclock:increase(b, B),
        A1desA = vclock:descends(A1, A),
        B2 = vclock:increase(b2, B1),
        B3 = vclock:increase(b, B2),
        B2desB1 = vclock:descends(B2, B1),
        B3desB2 = vclock:descends(B3, B2),
        2 = vclock:size(B2),
        2 = vclock:size(B3),
        2 = vclock:get_counter(b, B3),
        AB = vclock:merge(A1, B3),
        3 = vclock:size(AB),
        2 = vclock:get_counter(b, AB),
        [
         ?_assert(A1desA),
         ?_assert(B2desB1),
         ?_assert(B3desB2)
        ].
-endif.
