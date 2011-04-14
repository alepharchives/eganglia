%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Kotoko Group
%%% @doc gmetric wrapper
%%% @reference <a href="http://linux.die.net/man/1/gmetric">gmetric</a>
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

-module(gmetric).
-author('Fernando Benavides <greenmellon@gmail.com>').

%% Types
-type type() :: string | int8 | uint8 | int16 | uint16 | int32 | uint32 | float | double.
-type slope() :: zero | positive | negative | both.
-type option() :: {conf,  iodata()}       %% The configuration file to use for finding send channels (default='/usr/etc/gmond.conf')
                | {type,  type()}         %% Metric type (for defaults see {@link type/1})
                | {units, iodata()}       %% Unit of measure for the value e.g. Kilobytes, Celcius  (default='')
                | {slope, slope()}        %% Either zero|positive|negative|both  (default='both')
                | {tmax,  pos_integer()}  %% The maximum time in seconds between gmetric calls (default=60)
                | {dmax,  pos_integer()}  %% The lifetime in seconds of this metric  (default=0)
                | {spoof, iodata()}       %% IP address and name of host/device (colon separated) we are spoofing  (default='')
                | heartbeat.              %% Spoof a heartbeat message (use with spoof option)
-export_type([type/0]).

%% Static functions
-export([version/0, announce/2, announce/3, type/1]).

%% ===================================================================
%% API (Static)
%% ===================================================================
%% @doc Announces a metric
%% @equiv announce(Name, Value, [])
-spec announce(Name::iodata(), Value::term()) -> ok | {error, term()}.
announce(Name, Value) ->
  announce(Name, Value, []).

%% @doc Announces a metric
-spec announce(Name::iodata(), Value::term(), Options::[option()]) -> ok | {error, term()}.
announce(Name, Value, Options) ->
  AllOptions =
    [{name, Name}, {value, Value} |
       case proplists:get_value(type, Options) of
         undefined -> [{type, type(Value)} | Options];
         _ -> Options
       end],
  case run(AllOptions) of
    [] -> ok;
    Error -> {error, Error}
  end.

%% @doc Returns gmetric version number
-spec version() -> string().
version() ->
  run([version]).

%% @doc Infers a gmetric type for the given value.
%%      The inferred type is the most general possible one
-spec type(term()) -> type().
type(Value) when is_integer(Value) -> int32;
type(Value) when is_float(Value) -> float;
type(_) -> string.

%% ===================================================================
%% Private
%% ===================================================================
run(Options) ->
  Command = erlang:binary_to_list(
              erlang:iolist_to_binary(
                "gmetric" ++ lists:map(fun parse_option/1, Options))),
  io:format("~p~n", [Command]),
  os:cmd(Command).

parse_option({Name, Value}) ->
  io_lib:format(" --~s=", [Name]) ++ parse_value(Value);
parse_option(Name) ->
  io_lib:format(" --~s", [Name]).

parse_value(Value) when is_atom(Value) -> atom_to_list(Value);
parse_value(Value) when is_integer(Value) -> integer_to_list(Value);
parse_value(Value) when is_float(Value) -> float_to_list(Value);
parse_value(Value) -> [$\", Value, $\"].