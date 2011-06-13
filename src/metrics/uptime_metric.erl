%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc Uptime as string
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
-module(uptime_metric).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behavior(gen_metric).

-record(state, {}).
-opaque state() :: #state{}.

-define(SECONDS_PER_MIN, 60).
-define(SECONDS_PER_HOUR, (?SECONDS_PER_MIN*60)).
-define(SECONDS_PER_DAY, (?SECONDS_PER_HOUR*24)).
-define(R(V,N), string:right(integer_to_list(V),N,$0)).

-export([add/1, delete/1]).
-export([init/1, handle_metric/1, handle_call/2, handle_cast/2, terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Adds the metric to the group
-spec add(metric_group:group()) -> ok | {error, already_present | term()}.
add(Group) ->
  metric_group:add_metric(Group, ?MODULE, []).

%% @doc Removes the metric from the group
-spec delete(metric_group:group()) -> ok | {error, not_found}.
delete(Group) ->
  metric_group:delete_metric(Group, ?MODULE, normal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec init([]) -> {ok, binary(), state()}.
init([]) -> {ok, <<"uptime">>, #state{}}.

%% @private
-spec handle_metric(state()) -> {ok, string(), state()}.
handle_metric(State) -> {ok, format(erlang:statistics(wall_clock)), State}.

%% @private
-spec handle_call(Any, state()) -> {stop, {unexpected_call, Any}, {unexpected_call, Any}, state()}.
handle_call(Any, State) -> {stop, {unexpected_call, Any}, {unexpected_call, Any}, State}.

%% @private
-spec handle_cast(Any, state()) -> {stop, {unexpected_cast, Any}, state()}.
handle_cast(Any, State) -> {stop, {unexpected_cast, Any}, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.

format({Millis, _}) ->
  SecTime = Millis div 1000,
  Days = ?R(SecTime div ?SECONDS_PER_DAY,3),
  Hours = ?R((SecTime rem ?SECONDS_PER_DAY) div ?SECONDS_PER_HOUR,2),
  Minutes = ?R(((SecTime rem ?SECONDS_PER_DAY) rem ?SECONDS_PER_HOUR) div ?SECONDS_PER_MIN, 2),
  Seconds = ?R(((SecTime rem ?SECONDS_PER_DAY) rem ?SECONDS_PER_HOUR) rem ?SECONDS_PER_MIN, 2),
  io_lib:format("~s:~s:~s:~s",[Days,Hours,Minutes,Seconds]).