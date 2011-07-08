%%-------------------------------------------------------------------
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2011 InakaLabs SRL
%% @doc RPC.io socket.io client handler
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
-module(rpcio_client_handler).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(gen_event).

-export([start/1]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include("elog.hrl").
-include("socketio.hrl").
-include("misultin.hrl").

-record(state, {}).
-type state() :: #state{}.

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc Starts a handler for the given connection
-spec start(pid()) -> ok.
start(ClientPid) ->
  case socketio_client:request(ClientPid) of
    {misultin_req, #req{peer_addr = Peer, uri = {abs_path, Uri}}, _Pid} ->
      ?INFO("New client: From ~s using ~s~n", [inet_parse:ntoa(Peer), Uri]);
    {misultin_ws, #ws{peer_addr = Peer, path = Uri}, _Pid} ->
      ?INFO("New client: From ~s using ~s~n", [inet_parse:ntoa(Peer), Uri]);
    Other ->
      ?INFO("New client: using~n\t~p~n", [Other])
  end,
  gen_event:add_handler(socketio_client:event_manager(ClientPid), ?MODULE, []).

%% ====================================================================
%% Gen Event functions
%% ====================================================================
%% @hidden
-spec init(reference()) -> {ok, state()}.
init([]) -> {ok, #state{}}.

%% @hidden
-spec handle_event({message, pid(), #msg{}}, state()) -> {ok, state()}.
handle_event({message, ClientPid, SocketioMsg}, State) ->
  Command =
      case SocketioMsg of
        #msg{content = MsgProps, json = true} ->
          rpcio_util:safe_term_to_binary(proplists:get_value(<<"cmd">>, MsgProps, <<>>));
        #msg{content = Text, json = false} ->
          rpcio_util:safe_term_to_binary(Text)
      end,
  case rpcio:eval(Command) of
    {ok, Result} ->
      socketio_client:send(ClientPid,
                           #msg{json = true,
                                content = [{<<"error">>, false},
                                           {<<"result">>, iolist_to_binary(io_lib:format("~p", [Result]))}]});
    {error, Reason} ->
      socketio_client:send(ClientPid,
                           #msg{json = true,
                                content = [{<<"error">>, true}, {<<"desc">>, Reason}]})
  end,
  {ok, State};
handle_event(Event, State) ->
  ?INFO("Ignored socketio event: ~p~n", [Event]),
  {ok, State}.

%% @hidden
-spec handle_call(term(), state()) -> {ok, ok, state()}.
handle_call(_Request, State) -> {ok, ok, State}.

%% @hidden
-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(_Info, State) -> {ok, State}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.