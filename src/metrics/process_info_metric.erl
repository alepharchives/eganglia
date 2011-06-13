%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc erlang:process_info/1 metrics
%%% @end
%%%-------------------------------------------------------------------

%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
-module(process_info_metric).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behavior(gen_metric).

-record(state, {info :: term(), op :: fun(([integer() | float()]) -> integer() | float())}).
-opaque state() :: #state{}.

-export([add/3, delete/3]).
-export([init/1, handle_metric/1, handle_call/2, handle_cast/2, terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Adds the metric to the group.
%%      The value will come roughly from <code>OpName([element(2, erlang:process_info(Pid, Info)) || Pid &lt;- erlang:processes()])</code>
-spec add(metric_group:group(), avg | sum | max, atom()) -> ok | {error, already_present | term()}.
add(Group, OpName, Info) ->
  metric_group:add_metric(Group, {?MODULE, OpName, Info}, ?MODULE, {OpName, Info}).

%% @doc Removes the metric from the group
-spec delete(metric_group:group(), avg | sum | max, atom()) -> ok | {error, not_found}.
delete(Group, OpName, Info) ->
  metric_group:delete_metric(Group, {?MODULE, OpName, Info}, normal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec init({atom(), avg | sum | max}) -> {ok, binary(), state()}.
init({OpName, Info}) ->
  {ok, erlang:iolist_to_binary(io_lib:format("~p ~p", [OpName, Info])),
   #state{info  = Info,
          op    = case OpName of
                    avg -> fun(L) -> case erlang:length(L) of
                                       0 -> 0;
                                       LL -> lists:sum(L) / LL
                                     end
                           end;
                    sum -> fun lists:sum/1;
                    max -> fun lists:max/1
                  end}}.

%% @private
-spec handle_metric(state()) -> {ok, pos_integer(), state()}.
handle_metric(State = #state{info = Info, op = Op}) ->
  Values = [erlang:process_info(Pid, Info) || Pid <- erlang:processes()],
  {ok, Op([V || {I, V} <- Values, I =:= Info]), State}.

%% @private
-spec handle_call(Any, state()) -> {stop, {unexpected_call, Any}, {unexpected_call, Any}, state()}.
handle_call(Any, State) -> {stop, {unexpected_call, Any}, {unexpected_call, Any}, State}.

%% @private
-spec handle_cast(Any, state()) -> {stop, {unexpected_cast, Any}, state()}.
handle_cast(Any, State) -> {stop, {unexpected_cast, Any}, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.
