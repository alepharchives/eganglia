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
%%%   <pre>handle_cast(Msg::term(), State::term()) -> {@link handler_result()}</pre>
%%%     Called from <code>gen_metric:cast/2</code><br/>
%%%   </li><li>
%%%   <pre>handle_info(Msg::term(), State::term()) -> {@link handler_result()}</pre>
%%%     Called each time an erlang message is received<br/>
%%%   </li><li>
%%%   <pre>terminate(Reason :: normal | shutdown | term(), State) -> _</pre>
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

-behaviour(gen_server).

-type gen_start_option() :: {timeout, non_neg_integer() | infinity | hibernate} |
                            {debug, [trace | log | {logfile, string()} | statistics | debug]}.
-type start_option() :: gmetric:option() | gen_start_option().
-type start_result() :: {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
-export_type([gen_start_option/0, start_option/0, start_result/0]).

-type init_result()     :: {ok, State::term(), Timeout::pos_integer()} | ignore | {stop, Reason::term()}.
-type metric_result()   :: {ok, Value::term(), State::term(), Timeout::pos_integer()} | {ignore, State::term(), Timeout::pos_integer()}.
-type call_result()     :: {reply, Reply::term(), State::term(), Timeout::pos_integer()} | {stop, Reason::term(), Reply::term(), State::term()}.
-type handler_result()  :: {noreply, State::term(), Timeout::pos_integer()} | {stop, Reason::term(), State::term()}.
-export_type([init_result/0, handler_result/0, call_result/0, metric_result/0]).

-type server() :: atom() | pid() | {global, atom()}.
-export_type([server/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% BEHAVIOUR
-export([behaviour_info/1]).
%% API
-export([start/4, start/5, start_link/4, start_link/5, call/2, call/3, cast/2, cast/3]).
%% GEN SERVER
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNALS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {module        :: atom(), % Callback module
                mod_state     :: term(), % Callback module state
                metric        :: string(),
                options = []  :: [gmetric:option()]
               }).
-opaque state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% @hidden
-spec behaviour_info(callbacks | term()) -> undefined | [{atom(), non_neg_integer()}].
behaviour_info(callbacks) ->
  [{init, 1}, {handle_metric, 1}, {handle_cast, 2},
   {handle_info, 2}, {handle_call, 2}, {terminate, 2}];
behaviour_info(_Other) ->
  undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% @doc  Starts a generic server.
-spec start(Mod::atom(), Args::term(), Metric::string(), Options::[start_option()]) -> start_result().
start(Mod, Args, Metric, Options) ->
  {MetricOptions, OtherOptions} = parse_start_options(Options),
  gen_server:start(?MODULE, {Mod, Args, Metric, MetricOptions}, OtherOptions).

%%% @doc  Starts a named generic server.
-spec start(Name::{local|global, atom()}, Mod::atom(), Args::term(), Metric::string(), Options::[start_option()]) -> start_result().
start(Name, Mod, Args, Metric, Options) ->
  {MetricOptions, OtherOptions} = parse_start_options(Options),
  gen_server:start(Name, ?MODULE, {Mod, Args, Metric, MetricOptions}, OtherOptions).

%%% @doc  Starts and links a generic server.
-spec start_link(Mod::atom(), Args::term(), Metric::string(), Options::[start_option()]) -> start_result().
start_link(Mod, Args, Metric, Options) ->
  {MetricOptions, OtherOptions} = parse_start_options(Options),
  gen_server:start_link(?MODULE, {Mod, Args, Metric, MetricOptions}, OtherOptions).

%%% @doc  Starts and links a named generic server.
-spec start_link(Name::{local|global, atom()}, Mod::atom(), Args::term(), Metric::string(), Options::[start_option()]) -> start_result().
start_link(Name, Mod, Args, Metric, Options) ->
  {MetricOptions, OtherOptions} = parse_start_options(Options),
  gen_server:start_link(Name, ?MODULE, {Mod, Args, Metric, MetricOptions}, OtherOptions).

%%% @doc Make a call to a generic server.
%%% If the server is located at another node, that node will be monitored.
%%% If the client is trapping exits and is linked server termination is handled here
-spec call(Server::server(), Request::term()) -> Response::term().
call(Server, Request) ->
  gen_server:call(Server, {call, Request}).

%%% @doc Make a call to a generic server.
%%% If the server is located at another node, that node will be monitored.
%%% If the client is trapping exits and is linked server termination is handled here
-spec call(Server::server(), Request::term(), Timeout::non_neg_integer()|infinity) -> Response::term().
call(Server, Request, Timeout) ->
  gen_server:call(Server, {call, Request}, Timeout).

%%% @doc Make a cast to a generic server.
%%% Returns ok immediately
-spec cast(Server::server(), Request::term()) -> Response::term().
cast(Server, Request) ->
  gen_server:cast(Server, {cast, Request}).

%%% @doc Make a cast to a generic server.
%%% Returns ok immediately
-spec cast(Server::server(), Request::term(), Timeout::non_neg_integer()|infinity) -> Response::term().
cast(Server, Request, Timeout) ->
  gen_server:cast(Server, {cast, Request}, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN SERVER FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec init({atom(), term(), string(), [gmetric:option()]}) -> {ok, state(), pos_integer()} | ignore | {stop, term()}.
init({Mod, InitArgs, Metric, Options}) ->
  case Mod:init(InitArgs) of
    {ok, ModState, Timeout} ->
      {ok, #state{module    = Mod,
                  mod_state = ModState,
                  metric    = Metric,
                  options   = Options}, Timeout};
    Other ->
      Other
  end.

%% @hidden
-spec handle_call({call, term()}, reference(), state()) -> call_result().
handle_call({call, Request}, _From, State = #state{module = Mod, mod_state = ModState}) ->
  try Mod:handle_call(Request, ModState) of
    {reply, Reply, NewModSt, Timeout} ->
      {reply, Reply, State#state{mod_state = NewModSt}, Timeout};
    {stop, Reason, Reply, NewModSt} ->
      {stop, Reason, Reply, State#state{mod_state = NewModSt}}
  catch
    _:{reply, Reply, NewModSt, Timeout} ->
      {reply, Reply, State#state{mod_state = NewModSt}, Timeout};
    _:{stop, Reason, Reply, NewModSt} ->
      {stop, Reason, Reply, State#state{mod_state = NewModSt}}
  end.

%% @hidden
-spec handle_cast({cast, term()}, state()) -> handler_result().
handle_cast({cast, Request}, State = #state{module = Mod, mod_state = ModState}) ->
  try Mod:handle_cast(Request, ModState) of
    {noreply, NewModSt, Timeout} ->
      {noreply, State#state{mod_state = NewModSt}, Timeout};
    {stop, Reason, NewModSt} ->
      {stop, Reason, State#state{mod_state = NewModSt}}
  catch
    _:{noreply, NewModSt, Timeout} ->
      {noreply, State#state{mod_state = NewModSt}, Timeout};
    _:{stop, Reason, NewModSt} ->
      {stop, Reason, State#state{mod_state = NewModSt}}
  end.

%% @hidden
-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_info(timeout, State = #state{module = Mod, mod_state = ModState,
                                    metric = Metric, options = Options}) ->
  try Mod:handle_metric(ModState) of
    {ok, Value, NewModSt, Timeout} ->
      case gmetric:announce(Metric, Value, Options) of
        ok ->
          {noreply, State#state{mod_state = NewModSt}, Timeout};
        {error, Reason} ->
          {stop, {gmetric_error, Reason}, State#state{mod_state = NewModSt}}
      end;
    {ignore, NewModSt, Timeout} ->
      {noreply, State#state{mod_state = NewModSt}, Timeout}
  catch
    _:{ok, Value, NewModSt, Timeout} ->
      case gmetric:announce(Metric, Value, Options) of
        ok ->
          {noreply, State#state{mod_state = NewModSt}, Timeout};
        {error, Reason} ->
          {stop, {gmetric_error, Reason}, State#state{mod_state = NewModSt}}
      end;
    _:{ignore, NewModSt, Timeout} ->
      {noreply, State#state{mod_state = NewModSt}, Timeout}
  end;
handle_info(Info, State = #state{module = Mod, mod_state = ModState}) ->
  try Mod:handle_info(Info, ModState) of
    {noreply, NewModSt, Timeout} ->
      {noreply, State#state{mod_state = NewModSt}, Timeout};
    {stop, Reason, NewModSt} ->
      {stop, Reason, State#state{mod_state = NewModSt}}
  catch
    _:{noreply, NewModSt, Timeout} ->
      {noreply, State#state{mod_state = NewModSt}, Timeout};
    _:{stop, Reason, NewModSt} ->
      {stop, Reason, State#state{mod_state = NewModSt}}
  end.

%% @hidden
-spec terminate(any(), state()) -> any().
terminate(Reason, #state{module = Mod, mod_state = ModState}) ->
  Mod:terminate(Reason, ModState).

%% @hidden
-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_start_options(Options) ->
  lists:partition(fun({K, _V}) ->
                          not lists:member(K, [timeout, debug]);
                     (_) ->
                          true
                  end, Options).
