%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc erlang:statistics/1 metrics
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
-module(statistics_metric).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behavior(gen_metric).

-type kind() :: last | total | as_is.
-export_type([kind/0]).

-record(state, {info :: term(), kind :: kind()}).
-opaque state() :: #state{}.

-export([add/3, delete/3]).
-export([init/1, handle_metric/1, handle_call/2, handle_cast/2, terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Adds the metric to the group. The value will come from erlang:statistics(Info)
-spec add(metric_group:group(), kind(), term()) -> ok | {error, already_present | term()}.
add(Group, Kind, Info) ->
  metric_group:add_metric(Group, {?MODULE, Kind, Info}, ?MODULE, {Kind, Info}).

%% @doc Removes the metric from the group
-spec delete(metric_group:group(), kind(), term()) -> ok | {error, not_found}.
delete(Group, Kind, Info) ->
  metric_group:delete_metric(Group, {?MODULE, Kind, Info}, normal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec init({kind(), term()}) -> {ok, binary(), state()}.
init({Kind, Info}) ->
  {ok, erlang:iolist_to_binary(io_lib:format("~p ~p ~p", [node(), Kind, Info])),
   #state{info = Info, kind = Kind}}.

%% @private
-spec handle_metric(state()) -> {ok, pos_integer(), state()}.
handle_metric(State = #state{info = Info, kind = Kind}) ->
  {ok, case {Kind, erlang:statistics(Info)} of
         {last, {_Total, Last}} -> Last;
         {total, {Total, _Last}} -> Total;
         {as_is, Value} -> Value;
         {_, Other} -> Other
       end, State}.

%% @private
-spec handle_call(Any, state()) -> {stop, {unexpected_call, Any}, {unexpected_call, Any}, state()}.
handle_call(Any, State) -> {stop, {unexpected_call, Any}, {unexpected_call, Any}, State}.

%% @private
-spec handle_cast(Any, state()) -> {stop, {unexpected_cast, Any}, state()}.
handle_cast(Any, State) -> {stop, {unexpected_cast, Any}, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.
