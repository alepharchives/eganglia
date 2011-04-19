%%%-------------------------------------------------------------------
%%% @hidden
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc eganglia main supervisor. 
%%% @end
%%%-------------------------------------------------------------------
-module(eganglia_sup).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(supervisor).

-type startlink_err() :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

-define(DEFAULT_THRESHOLD, 3600).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-type metric() :: {atom(), term()}
                | {term(), atom(), term()}
                | {term(), atom(), term()}
                | {term(), atom(), term(), [gmetric:option()]}
                | {term(), atom(), term(), [gmetric:option()], float}.
-type metric_group() :: {{local|global, atom()}, once | pos_integer(), pos_integer(), [metric()]}
                      | {{local|global, atom()}, once | pos_integer(), [metric()]}.

%% ===================================================================
%% API functions
%% ===================================================================
%% @hidden
-spec start_link() -> startlink_ret().
start_link() ->
  Groups =
    case application:get_env(eganglia, groups) of
      undefined -> [];
      {ok, G} -> G
    end,
  case supervisor:start_link({local, ?MODULE}, ?MODULE, Groups) of
    {ok, Pid} ->
      lists:foreach(fun add_metrics/1, Groups),
      {ok, Pid};
    Other ->
      Other
  end.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
%% @hidden
-spec init([metric_group()]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init(Groups) ->
  Children =
    lists:map(fun({{_, Name} = Reg, CollectEvery, TimeThreshold, _Metrics}) ->
                      {Name, {metric_group, start_link, [Reg, CollectEvery, TimeThreshold, []]},
                       permanent, 1000, worker, [metric_group]};
                 ({{_, Name} = Reg, CollectEvery, _Metrics}) ->
                      {Name, {metric_group, start_link, [Reg, CollectEvery, ?DEFAULT_THRESHOLD, []]},
                       permanent, 1000, worker, [metric_group]}
              end, Groups),
  error_logger:info_msg("Inititalizing eganglia supervisor with ~p metric groups...~n", [length(Children)]),
  {ok, {{one_for_one, 5, 10}, Children}}.

%% ===================================================================
%% Private functions
%% ===================================================================
add_metrics({{local, Name}, _, _, Metrics}) -> add_metrics(Name, Metrics);
add_metrics({Name, _, _, Metrics}) -> add_metrics(Name, Metrics);
add_metrics({{local, Name}, _, Metrics}) -> add_metrics(Name, Metrics);
add_metrics({Name, _, Metrics}) -> add_metrics(Name, Metrics).

add_metrics(Name, Metrics) -> lists:foreach(fun(Metric) -> ok = add_metric(Name, Metric) end, Metrics).

add_metric(Group, {Module, InitArgs}) ->
  metric_group:add_metric(Group, Module, InitArgs);
add_metric(Group, {MetricId, Module, InitArgs}) ->
  metric_group:add_metric(Group, MetricId, Module, InitArgs);
add_metric(Group, {MetricId, Module, InitArgs, Options}) ->
  metric_group:add_metric(Group, MetricId, Module, InitArgs, Options);
add_metric(Group, {MetricId, Module, InitArgs, Options, Threshold}) ->
  metric_group:add_metric(Group, MetricId, Module, InitArgs, Options, Threshold).
