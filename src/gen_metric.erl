%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc Generic metric server for the Ganglia Monitoring System
%%%
%%% The user module should export:
%%%<ul>
%%%   <li>
%%%   <pre>init(Args::term()) -> {@link init_result()}</pre>
%%%     Opens and/or initializes the server.<br/>
%%%   </li><li>
%%%   <pre>handle_metric(State::term()) -> {@link metric_result()}</pre>  
%%%     Called each time the metric should be collected<br/>
%%%   </li><li>
%%%   <pre>handle_call(Msg::term(), State::term()) -> {@link call_result()}</pre>
%%%     Called from <code>gen_metric:call/2</code><br/>
%%%   </li><li>
%%%   <pre>handle_cast(Msg::term(), State::term()) -> {@link cast_result()}</pre>
%%%     Called from <code>gen_metric:cast/2</code><br/>
%%%   </li><li>
%%%   <pre>terminate(Reason :: term(), State) -> _</pre>
%%%     Let the user module clean up. Always called when server terminates.<br/>
%%%   </li>
%%% </ul>
%%% Timeouts (expressed in milliseconds) are used everywhere to determine the next time the metric should be announced
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

-module(gen_metric).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').
-vsn("0.1").

-type init_result()   :: {ok, State::term()}.
-type metric_result() :: {ok, Value::term(), State::term()}.
-type call_result()   :: {reply, Reply::term(), State::term()} | {stop, Reason::term(), Reply::term(), State::term()}.
-type cast_result()   :: {noreply, State::term()} | {stop, Reason::term(), State::term()}.
-export_type([init_result/0, cast_result/0, call_result/0, metric_result/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% BEHAVIOUR
-export([behaviour_info/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% @hidden
-spec behaviour_info(callbacks | term()) -> undefined | [{atom(), non_neg_integer()}].
behaviour_info(callbacks) ->
  [{init, 1}, {handle_metric, 1}, {handle_cast, 2}, {handle_call, 2}, {terminate, 2}];
behaviour_info(_Other) ->
  undefined.