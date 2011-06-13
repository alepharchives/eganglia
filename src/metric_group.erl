%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc Metric group server for the Ganglia Monitoring System.
%%%      Intended to be used with {@link gen_metric} modules
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

-module(metric_group).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').
-vsn("1.0").

-behaviour(gen_server).

-type start_option() :: {timeout, non_neg_integer() | infinity | hibernate} |
                        {debug, [trace | log | {logfile, string()} | statistics | debug]}.
-type start_result() :: {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
-export_type([start_option/0, start_result/0]).

-type group() :: atom() | pid() | {global, atom()}.
-export_type([group/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([add_metric/3, add_metric/4, add_metric/5, add_metric/6, delete_metric/3,
         call/3, call/4, cast/3,
         start_link/4, start_link/5, start/4, start/5, stop/1,
         which_metrics/1]).
%% GEN SERVER
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNALS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(metric, {id                     :: term(),
                 title                  :: binary(),
                 module                 :: atom(),
                 mod_state              :: term(),
                 value_threshold = 0.0  :: float(),
                 options = []           :: [gmetric:option()],
                 last_value             :: term()}).
-type metric() :: #metric{}.
-record(state, {collect_timer   :: collect_once | timer:tref(),
                announce_timer  :: reference(),
                time_threshold  :: pos_integer(),
                metrics = []    :: [metric()],
                default_options :: [gmetric:option()],
                bin_node        :: binary()
               }).
-opaque state() :: #state{}.
-export_type([metric/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% @doc  Starts a metric group.
-spec start(once | pos_integer(), pos_integer(), [gmetric:option()], [start_option()]) -> start_result().
start(CollectEvery, TimeThreshold, DefaultOptions, Options) ->
  gen_server:start(?MODULE, {CollectEvery, TimeThreshold, DefaultOptions}, Options).

%%% @doc  Starts a named metric group.
-spec start({local|global, atom()}, once | pos_integer(), pos_integer(), [gmetric:option()], [start_option()]) -> start_result().
start(Name, CollectEvery, TimeThreshold, DefaultOptions, Options) ->
  gen_server:start(Name, ?MODULE, {CollectEvery, TimeThreshold, DefaultOptions}, Options).

%%% @doc  Starts and links a metric group.
-spec start_link(once | pos_integer(), pos_integer(), [gmetric:option()], [start_option()]) -> start_result().
start_link(CollectEvery, TimeThreshold, DefaultOptions, Options) ->
  gen_server:start_link(?MODULE, {CollectEvery, TimeThreshold, DefaultOptions}, Options).

%%% @doc  Starts and links a named generic server.
-spec start_link({local|global, atom()}, once | pos_integer(), pos_integer(), [gmetric:option()], [start_option()]) -> start_result().
start_link(Name, CollectEvery, TimeThreshold, DefaultOptions, Options) ->
  gen_server:start_link(Name, ?MODULE, {CollectEvery, TimeThreshold, DefaultOptions}, Options).

%% @doc  Adds a metric to the group
%% @equiv add_metric(Group, Module, Module, InitArgs)
-spec add_metric(group(), atom(), term()) -> ok | {error, already_present | term()}.
add_metric(Group, Module, InitArgs) ->
  add_metric(Group, Module, Module, InitArgs).

%% @doc  Adds a metric to the group
%% @equiv add_metric(Group, MetricId, Module, InitArgs, [])
-spec add_metric(group(), term(), atom(), term()) -> ok | {error, already_present | term()}.
add_metric(Group, MetricId, Module, InitArgs) ->
  add_metric(Group, MetricId, Module, InitArgs, []).

%% @doc  Adds a metric to the group
%% @equiv add_metric(Group, MetricId, Module, InitArgs, Options, 0.0)
-spec add_metric(group(), term(), atom(), term(), [gmetric:option()]) -> ok | {error, already_present | term()}.
add_metric(Group, MetricId, Module, InitArgs, Options) ->
  add_metric(Group, MetricId, Module, InitArgs, Options, 0.0).

%% @doc  Adds a metric to the group
-spec add_metric(group(), term(), atom(), term(), [gmetric:option()], float()) -> ok | {error, already_present | term()}.  
add_metric(Group, MetricId, Module, InitArgs, Options, Threshold) ->
  gen_server:call(Group, {add_metric, MetricId, Module, InitArgs, Options, Threshold}).

%%% @doc Makes a synchronous call to a particular metric.
-spec call(group(), term(), term()) -> Response::term() | {error, bad_metric | term()}.
call(Group, MetricId, Request) ->
  gen_server:call(Group, {call, MetricId, Request}).

%%% @doc Makes a synchronous call to a particular metric.
-spec call(group(), term(), term(), non_neg_integer()|infinity) -> term() | {error, bad_metric | term()}.
call(Group, MetricId, Request, Timeout) ->
  gen_server:call(Group, {call, MetricId, Request}, Timeout).

%%% @doc Make a cast to a paricular metric.
%%% Returns ok immediately, even if the metric doesn't exist
-spec cast(group(), term(), term()) -> ok.
cast(Group, MetricId, Request) ->
  gen_server:cast(Group, {cast, MetricId, Request}).

%% @doc  Removes a metric to the group.
%%       If the metric doesn't exist it just does nothing at all.
-spec delete_metric(group(), term(), term()) -> ok | {error, not_found}.  
delete_metric(Group, MetricId, Reason) ->
  gen_server:call(Group, {delete_metric, MetricId, Reason}).

%% @doc  Returns a list of all metrics in the group
-spec which_metrics(group()) -> [metric()].
which_metrics(Group) ->
  gen_server:call(Group, which_metrics).

%% @doc  Stops the metric group
-spec stop(group()) -> ok.
stop(Group) ->
  gen_server:cast(Group, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN SERVER FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec init({once | pos_integer(), pos_integer(), [gmetric:option()]}) -> {ok, state(), hibernate}.
init({CollectEvery, TimeThreshold, DefaultOptions}) ->
  CTimer =
    case CollectEvery of
      once -> collect_once;
      ESeconds ->
        {ok, TRef} = timer:send_interval(ESeconds * 1000, collect),
        TRef
    end,
  ATimer = erlang:send_after(TimeThreshold * 1000, self(), announce),
  {ok, #state{collect_timer   = CTimer,
              announce_timer  = ATimer,
              time_threshold  = TimeThreshold,
              default_options = DefaultOptions,
              bin_node        = hd(binary:split(atom_to_binary(node(), utf8), <<"@">>, [trim])),
              metrics         = []}, hibernate}.

%% @hidden
-spec handle_call({delete_metric, term(), term()} | {add_metric, term(), binary(), atom(), term(), [gmetric:option()], float()} | which_metrics | {call, term(), term()}, reference(), state()) -> {reply, Reply::term(), State::term()}.
handle_call({add_metric, MetricId, Module, InitArgs, Options, Threshold}, _From, State) ->
  #state{metrics = Metrics, collect_timer = CTimer,
         default_options = DefaultOptions, bin_node = BinNode} = State,
  case lists:keymember(MetricId, #metric.id, Metrics) of
    true ->
      {reply, {error, already_present}, State};
    false ->
      try Module:init(InitArgs) of
        {ok, SubTitle, ModState} ->
          {Value, ModState1} =
            case CTimer of
              collect_once ->
                collect_metric(Module, ModState, undefined);
              _ ->
                {undefined, ModState}
            end,
          Metric =
            #metric{id              = MetricId,
                    title           = <<BinNode/binary, $\s, SubTitle/binary>>,
                    module          = Module,
                    mod_state       = ModState1,
                    value_threshold = Threshold,
                    options         = merge(Options, DefaultOptions),
                    last_value      = Value},
          {reply, ok, State#state{metrics = [Metric|Metrics]}};
        Other ->
          {reply, {error, {bad_init_return, Module, Other}}, State}
      catch
        _:Error ->
          {reply, {error, {init_exception, Module, Error}}, State}
      end
  end;
handle_call({delete_metric, MetricId, Reason}, _From, State = #state{metrics = Metrics}) ->
  case lists:keytake(MetricId, #metric.id, Metrics) of
    false ->
      {reply, {error, not_found}, State};
    {value, #metric{module = Mod, mod_state = ModState}, Rest} ->
      catch Mod:terminate(Reason, ModState),
      {reply, ok, State#state{metrics = Rest}}
  end;
handle_call(which_metrics, _From, State = #state{metrics = Metrics}) ->
  {reply, Metrics, State};
handle_call({call, MetricId, Request}, _From, State = #state{metrics = Metrics}) ->
  case lists:keytake(MetricId, #metric.id, Metrics) of
    false ->
      {reply, {error, bad_metric}, State};
    {value, Metric = #metric{module = Mod, mod_state = ModState}, Rest} ->
      try Mod:handle_call(Request, ModState) of
        {reply, Reply, NewModSt} ->
          {reply, Reply, State#state{metrics = [Metric#metric{mod_state = NewModSt} |Rest]}};
        {stop, Reason, Reply, NewModSt} ->
          catch Mod:terminate(Reason, NewModSt),
          {reply, Reply, State#state{metrics = Rest}}
      catch
        _:{reply, Reply, NewModSt} ->
          {reply, Reply, State#state{metrics = [Metric#metric{mod_state = NewModSt} |Rest]}};
        _:{stop, Reason, Reply, NewModSt} ->
          catch Mod:terminate(Reason, NewModSt),
          {reply, Reply, State#state{metrics = Rest}}
      end
  end.

%% @hidden
-spec handle_cast(stop | {cast, term(), term()}, state()) -> {noreply, state()} | {stop, normal, state()}.
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({cast, MetricId, Request}, State = #state{metrics = Metrics}) ->
  case lists:keytake(MetricId, #metric.id, Metrics) of
    false ->
      error_logger:warning_msg("Cast to unexistent metric ~p: ~p~n", [MetricId, Request]),
      {noreply, State};
    {value, Metric = #metric{module = Mod, mod_state = ModState}, Rest} ->
      try Mod:handle_cast(Request, ModState) of
        {noreply, NewModSt} ->
          {noreply, State#state{metrics = [Metric#metric{mod_state = NewModSt} |Rest]}};
        {stop, Reason, NewModSt} ->
          catch Mod:terminate(Reason, NewModSt),
          {noreply, State#state{metrics = Rest}}
      catch
        _:{noreply, NewModSt} ->
          {noreply, State#state{metrics = [Metric#metric{mod_state = NewModSt} |Rest]}};
        _:{stop, Reason, NewModSt} ->
          catch Mod:terminate(Reason, NewModSt),
          {noreply, State#state{metrics = Rest}}
      end
  end.

%% @hidden
-spec handle_info(collect | announce | term(), state()) -> {noreply, state()}.
handle_info(collect, State = #state{metrics = Metrics}) ->
  {Announce, NewMetrics} =
    lists:foldl(fun(Metric, {AccAnnounce, AccMetrics}) ->
                  case collect_metric(Metric) of
                    {announce, NewMetric} ->
                      {announce, [NewMetric | AccMetrics]};
                    {not_announce, NewMetric} ->
                      {AccAnnounce, [NewMetric | AccMetrics]}
                  end
                end, {not_announce, []}, Metrics),
  case Announce of
    not_announce ->
      {noreply, State#state{metrics = NewMetrics}};
    announce ->
      handle_info(announce, State#state{metrics = NewMetrics})
  end;
handle_info(announce, State = #state{metrics = Metrics, announce_timer = ATimer, time_threshold = TimeThreshold}) ->
  _ = erlang:cancel_timer(ATimer),
  lists:foreach(fun announce_metric/1, Metrics),
  NewATimer = erlang:send_after(TimeThreshold * 1000, self(), announce),
  {noreply, State#state{announce_timer = NewATimer}};
handle_info(Info, State) ->
  error_logger:info_msg("Ignored info: ~p~n", [Info]),
  {noreply, State}.

%% @hidden
-spec terminate(any(), state()) -> any().
terminate(Reason, #state{metrics = Metrics}) ->
  lists:foreach(
    fun(#metric{module = Mod, mod_state = ModState}) ->
      catch Mod:terminate(Reason, ModState)
    end, Metrics).

%% @hidden
-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
announce_metric(#metric{id = MetricId, title = Title, last_value = Value, options = Options}) ->
  case gmetric:announce(Title, Value, Options) of
    ok -> ok;
    {error, Reason} ->
      error_logger:error_msg("Error announcing ~p (value: ~p): ~p~n", [MetricId, Value, Reason])
  end.

collect_metric(#metric{module = Module, mod_state = ModState, value_threshold = Threshold,
                       last_value = LastValue} = Metric) ->
  {NewValue, NewModSt} = collect_metric(Module, ModState, LastValue),
  NewMetric = Metric#metric{last_value = NewValue, mod_state = NewModSt},
  try {erlang:abs(LastValue - NewValue), erlang:abs(LastValue * Threshold / 100)} of
    {Diff, Min} when Diff >= Min ->
      {announce, NewMetric};
    _ ->
      {not_announce, NewMetric}
  catch
    _:badarith ->
      {not_announce, NewMetric}
  end.

collect_metric(Module, ModState, LastValue) ->
  try Module:handle_metric(ModState) of
    {ok, Value, NewModState} ->
      {Value, NewModState};
    Other ->
      error_logger:warning_msg("Bad metric result from ~p: ~p.~nPreserving last value: ~p~n", [Module, Other, LastValue]),
      {LastValue, ModState}
  catch
    _:{ok, Value, NewModState} ->
      {Value, NewModState};
    _:Error ->
      error_logger:warning_msg("Error on ~p:handle_metric/1 (State: ~p): ~p.~nPreserving last value: ~p~nStack: ~p~n", [Module, ModState, Error, LastValue, erlang:get_stacktrace()]),
      {LastValue, ModState}
  end.

merge(Options, DefaultOptions) ->
  lists:foldl(fun({Key, Value}, AccOptions) ->
                      lists:keystore(Key, 1, AccOptions,
                                     {Key, proplists:get_value(Key, AccOptions, Value)});
                 (Key, AccOptions) ->
                      case lists:member(Key, AccOptions) of
                        true -> AccOptions;
                        false -> [Key | AccOptions]
                      end
              end, Options, DefaultOptions).