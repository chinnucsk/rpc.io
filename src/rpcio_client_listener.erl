%%-------------------------------------------------------------------
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2011 InakaLabs SRL
%% @doc RPC.io socket.io listener
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
-module(rpcio_client_listener).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(gen_event).

-export([start_link/1]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
-export([handle_request/3]).

-include("elog.hrl").

-record(state, {port :: pos_integer()}).
-type state() :: #state{}.

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc Starts a listener
-spec start_link(pos_integer()) -> {ok, pid()}.
start_link(Port) ->
  {ok, Pid} = socketio_listener:start([{http_port, Port},
                                       {default_http_handler, ?MODULE}]),
  ok = gen_event:add_handler(socketio_listener:event_manager(Pid), ?MODULE, Port),
  {ok, Pid}.

%%-------------------------------------------------------------------
%% BEHAVIOUR CALLBACKS
%%-------------------------------------------------------------------
%% @private
-spec init(pos_integer()) -> {ok, state()}.
init(Port) -> {ok, #state{port = Port}}.

%% @private
-spec handle_event({client|disconnect, pid()} | term(), state()) -> {ok, state()}.
handle_event({client, Pid}, State) ->
  ok = rpcio_client_handler:start(Pid),
  {ok, State};
handle_event({disconnect, Pid}, State) ->
  ?DEBUG("~p disconnecting...~n", [Pid]),
  {ok, State};
handle_event(Event, State) ->
  ?INFO("Ignored socketio event: ~p~n", [Event]),
  {ok, State}.

%% @private
-spec handle_call(term(), state()) -> {ok, ok, state()}.
handle_call(_Request, State) -> {ok, ok, State}.
%% @private
-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(_Info, State) -> {ok, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(Reason, #state{port = Port}) ->
  ?WARN("Socket IO on port ~p terminating: ~p~n", [Port, Reason]).

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% @private
-spec handle_request(atom(), [string()], term()) -> term().
handle_request('GET', [], Req) ->
  handle_request('GET', ["console.html"], Req);
handle_request('GET', Path, Req) ->
  ?INFO("~p~n", [Path]),
  Req:file(filename:join(["wwwroot"| Path]));
handle_request(Method, Path, Req) ->
  ?WARN("~p: ~s~n\t~p~n", [Method, string:join(Path, "/"), Req]),
  Req:respond(405). %% Method not allowed