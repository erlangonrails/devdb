% @copyright 2010 Konrad-Zuse-Zentrum fuer Informationstechnik Berlin

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

%% @author Thorsten Schuett <schuett@zib.de>
%% @doc Handling all scalaris nodes inside an erlang VM.
%% @version $Id: service_per_vm.erl 953 2010-08-03 13:52:39Z kruber@zib.de $
-module(service_per_vm).
-author('schuett@zib.de').
-vsn('$Id: service_per_vm.erl 953 2010-08-03 13:52:39Z kruber@zib.de $').

-behaviour(gen_component).

-export([dump_node_states/0, kill_nodes/1, get_round_trip/2]).

-export([start_link/0, init/1, on/2]).

% state of the module
-type(state() :: ok).

% accepted messages the module
-type(message() :: {get_dht_nodes, Pid::comm:mypid()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc ask all local nodes for their state
-spec dump_node_states() -> [term()].
dump_node_states() ->
    [gen_component:get_state(Pid)
     || Pid <- process_dictionary:find_all_dht_nodes()].

-spec kill_nodes(No::non_neg_integer()) -> ok.
kill_nodes(No) ->
    Childs = lists:sublist([X || X <- supervisor:which_children(main_sup),
                                 is_list(element(1, X))], No),
    [supervisor:terminate_child(main_sup, element(1, Child)) || Child <- Childs],
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Server process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_component:start_link(?MODULE, [], [{register_native, service_per_vm}]).

-spec init(any()) -> state().
init(_Arg) ->
    ok.

-spec on(Message::message(), State::state()) -> state().
on({get_dht_nodes, Pid}, ok) ->
    case comm:is_valid(Pid) of
        true ->
            Nodes = get_live_dht_nodes(),
            comm:send(Pid, {get_dht_nodes_response, Nodes});
        false ->
            ok
    end,
    ok.

-spec get_live_dht_nodes() -> [comm:mypid()].
get_live_dht_nodes() ->
    [comm:make_global(Pid) || Pid <- process_dictionary:find_all_dht_nodes(), element(1, gen_component:get_state(Pid)) =:= state].

-spec get_round_trip(GPid::comm:mypid(), Iterations::pos_integer()) -> float().
get_round_trip(GPid, Iterations) ->
    Start = erlang:now(),
    [ begin
          comm:send(GPid, {ping, comm:this()}),
          receive
              _Any -> ok
          end
      end
      || _ <- lists:seq(1, Iterations) ],
    End = erlang:now(),
    timer:now_diff(End, Start) / Iterations.
