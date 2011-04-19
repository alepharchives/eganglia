%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc eganglia application.
%%%      When started, it starts the metric groups defined in the environment variables
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

-module(eganglia).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').
-vsn("0.1").

-behaviour(application).

%% API
-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% API (Global)
%% ===================================================================
%% @doc  Starts the application
-spec start() -> ok | {error, term()}.
start() -> application:start(?MODULE).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%% @hidden
-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) -> eganglia_sup:start_link().

%% @hidden
-spec stop([]) -> ok.
stop([]) -> ok.