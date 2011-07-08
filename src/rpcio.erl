%%-------------------------------------------------------------------
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2011 InakaLabs SRL
%% @doc RPC.io main interface
%% @end
%%-------------------------------------------------------------------
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
-module(rpcio).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').
-vsn('0.1').

-include("elog.hrl").

-behaviour(application).

-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([count_clients/0]).
-export([eval/1]).

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc Starts the application
-spec start() -> ok | {error, {already_started, rpcio}}.
start() ->
  _ = application:start(public_key),
  _ = application:start(ssl),
  application:start(rpcio).

%% @doc How many connected clients we've got
-spec count_clients() -> non_neg_integer().
count_clients() ->
  rpcio_sup:count_clients().

%% @doc Evaluates the command
-spec eval(iolist()) -> {ok, term()} | {error, binary()}.
eval(Command) ->
  case lib:eval_str(iolist_to_binary([Command, "\n"])) of
    {ok, Result} -> {ok, Result};
    {error, Reason} -> {error, rpcio_util:safe_term_to_binary(Reason)}
  end.

%% @doc Stops the application
-spec stop() -> ok.
stop() -> application:stop(rpcio).

%%-------------------------------------------------------------------
%% BEHAVIOUR CALLBACKS
%%-------------------------------------------------------------------
%% @private
-spec start(any(), any()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
  ?INFO("~p starting~n", [?MODULE]),
  case rpcio_sup:start_link() of
    {ok, Pid} -> {ok, Pid};
    Error -> {error, Error}
  end.

%% @private
-spec stop(any()) -> ok.
stop(_State) -> ok.