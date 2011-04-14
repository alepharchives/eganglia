%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc A dumb {@link gen_metric} implementation
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
-module(constant_metric).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behavior(gen_metric).

-record(state, {constant :: term()}).
-opaque state() :: #state{}.

-export([add/2, delete/1, constant/1, constant/2]).
-export([init/1, handle_metric/1, handle_call/2, handle_cast/2, terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Adds the metric to the group
-spec add(metric_group:group(), term()) -> ok | {error, already_present | term()}.
add(Group, Constant) ->
  metric_group:add_metric(Group, ?MODULE, <<"Constant Metric">>, ?MODULE, Constant).

%% @doc Removes the metric from the group
-spec delete(metric_group:group()) -> ok | {error, not_found}.
delete(Group) ->
  metric_group:delete_metric(Group, ?MODULE, normal).

%% @doc Gets the constant
-spec constant(metric_group:group()) -> term() | {error, bad_metric}.
constant(Group) ->
  metric_group:call(Group, ?MODULE, constant).

%% @doc Sets the constant
-spec constant(metric_group:group(), term()) -> ok.
constant(Group, Constant) ->
  metric_group:cast(Group, ?MODULE, Constant).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
-spec init(Constant) -> {ok, #state{constant :: Constant}}.
init(Constant) -> {ok, #state{constant = Constant}}.

%% @private
-spec handle_metric(#state{constant :: Constant}) -> {ok, Constant, #state{constant :: Constant}}.
handle_metric(State = #state{constant = Constant}) -> {ok, Constant, State}.

%% @private
-spec handle_call(constant, #state{constant :: Constant}) -> {reply, Constant, #state{constant :: Constant}}.
handle_call(constant, State = #state{constant = Constant}) -> {reply, Constant, State}.

%% @private
-spec handle_cast(Constant, #state{}) -> {noreply, #state{constant :: Constant}}.
handle_cast(Constant, State) -> {noreply, State#state{constant = Constant}}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.