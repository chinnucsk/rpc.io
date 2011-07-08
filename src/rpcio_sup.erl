%%-------------------------------------------------------------------
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2011 InakaLabs SRL
%% @doc RPC.io main supervisor
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
-module(rpcio_sup).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(supervisor).

-export([start_link/0, count_clients/0, init/1]).

-include("elog.hrl").

%% @doc  Starts a new supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec count_clients() -> non_neg_integer().
count_clients() ->
  lists:sum(
    [proplists:get_value(active, supervisor:count_children(ClientSup), 0) ||
       {undefined,Sup,supervisor,[socketio_listener_sup]} <-
         supervisor:which_children(socketio_listener_sup_sup),
       {socketio_client_sup,ClientSup,supervisor,[socketio_client_sup]} <-
         supervisor:which_children(Sup)]).

%% @hidden
-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
  {MinPort, MaxPort} = rpcio_util:get_env(socketio_port_range),
  Listeners =
    [{erlang:list_to_atom("rpcio_client_listener-" ++ erlang:integer_to_list(Port)),
      {rpcio_client_listener, start_link, [Port]},
      permanent, 1000, worker, [rpcio_client_listener]} ||
     Port <- lists:seq(MinPort, MaxPort)],
  {ok, {{one_for_one, 5, 10}, Listeners}}.