%%-------------------------------------------------------------------
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2011 InakaLabs SRL
%% @doc RPC.io utility functions
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

-module(rpcio_util).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-include("elog.hrl").

-type datetime() :: {{pos_integer(), 1..12, 1..31}, {0..23, 0..59, 0..59}}.
-export_type([datetime/0]).

-export([rfc2882/0, rfc2882/1, rfc3339/1, iso8601/0, iso8601/1, dateadd/2,
         safe_term_to_binary/1, safe_list_to_float/1, to_lower/1,
         uuid_utc/0, now/0, get_all_env/0, get_env/1, set_env/2, stop_timer/1]).

%% @doc provides an UUID based on UTC
-spec uuid_utc() -> binary().
uuid_utc() ->
  Now = {_, _, Micro} = erlang:now(),
  Nowish = calendar:now_to_universal_time(Now),
  Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
  Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
  list_to_binary(Prefix ++ integer_to_list(Micro) ++ mochihex:to_hex(crypto:rand_bytes(9))).

%% @doc Converts a list to float even when the list represents an integer
-spec safe_list_to_float(string()) -> float().
safe_list_to_float(String) ->
  try erlang:list_to_float(String)
  catch _:badarg -> erlang:list_to_integer(String) * 1.0
  end.

%% @doc Converts anything to its binary representation... if possible
-spec safe_term_to_binary(integer() | tuple() | atom() | binary() | iolist()) -> binary().
safe_term_to_binary(I) when is_integer(I) ->
  list_to_binary(integer_to_list(I));
safe_term_to_binary(L) when is_tuple(L) -> 
  <<>>;
safe_term_to_binary(L) when is_list(L) ->
  unicode:characters_to_binary(L);
safe_term_to_binary(undefined) -> 
  <<>>;
safe_term_to_binary(A) when is_atom(A) -> 
  list_to_binary(atom_to_list(A));
safe_term_to_binary(A) when is_binary(A) -> A.

%% @doc Returns current date in ISO8601 format
-spec iso8601() -> string().
iso8601() ->
  [EEEandComma, DD, MMM, YYYY, Time, "GMT"] = string:tokens(httpd_util:rfc1123_date(), " "),
  EEE = lists:reverse(erlang:tl(lists:reverse(EEEandComma))),
  string:join([EEE, MMM, DD, Time, "+0000", YYYY], " ").  

%% @doc Returns a date in ISO8601 format
-spec iso8601(datetime()) -> string().
iso8601(T) ->
  [EEEandComma, DD, MMM, YYYY, Time, "GMT"] = string:tokens(httpd_util:rfc1123_date(T), " "),
  EEE = lists:reverse(erlang:tl(lists:reverse(EEEandComma))),
  string:join([EEE, MMM, DD, Time, "+0000", YYYY], " ").  

%% @doc Returns current date in RFC2882 format
-spec rfc2882() -> string().
rfc2882() ->
  re:replace(httpd_util:rfc1123_date(),"GMT","+0000",[{return,list}]).

%% @doc Returns a date in RFC2882 format
-spec rfc2882(datetime()) -> string().
rfc2882(T) ->
  re:replace(httpd_util:rfc1123_date(T),"GMT","+0000",[{return,list}]).

%% @doc Returns a date in RFC3339 format
-spec rfc3339(datetime()) -> string().
rfc3339({{Year, Month, Day}, {Hour, Min, Sec}}) ->
  lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w+0000",
                              [Year,Month,Day, Hour, Min, Sec])).  

%% @doc Current milliseconds
-spec now() -> integer().
now() ->
  {_, _, MicroSecs} = erlang:now(),
  Millis = erlang:trunc(MicroSecs/1000),
  calendar:datetime_to_gregorian_seconds(
    calendar:universal_time()) * 1000 + Millis.

%% @doc Adds seconds to a datetime
-spec dateadd(datetime(), integer()) -> datetime().
dateadd(Date, Seconds) ->
  calendar:gregorian_seconds_to_datetime(
    calendar:datetime_to_gregorian_seconds(Date) + Seconds).

%% @equiv application:get_all_env(rpcio)
-spec get_all_env() -> [{atom(), term()}].
get_all_env() ->
  application:get_all_env(rpcio).

%% @doc Returns application:get_env(rpcio, Field) or its default value
-spec get_env(atom()) -> term().
get_env(Field) ->
  case application:get_env(rpcio, Field) of
    {ok, Value} ->
      ?DEBUG("~p := ~p~n", [Field, Value]),
      Value;
    _ ->
      Value = get_env_default(Field),
      ?DEBUG("~p := ~p~n", [Field, Value]),
      Value
  end.

%% @private
-spec get_env_default(atom()) -> term().
get_env_default(socketio_port_range) -> {8001, 8001};
get_env_default(Field) ->
  throw({env_undefined, Field}).

%% @equiv application:set_env(rpcio, Field, Value)
-spec set_env(atom(), term()) -> ok.
set_env(Field, Value) ->
  application:set_env(rpcio, Field, Value).

%% @doc Stops a timer disregarding if it came from {@link erlang} or {@link timer} functions
-spec stop_timer(undefined | reference() | timer:tref()) -> ok.
stop_timer(undefined) -> ok;
stop_timer(Ref) when is_reference(Ref) ->
  case erlang:cancel_timer(Ref) of
    false -> ok;
    _Time -> ok
  end;
stop_timer(Timer) ->
  case timer:cancel(Timer) of
    {ok, cancel} -> ok;
    {error, Reason} ->
      ?WARN("Couldn't stop timer ~p: ~p~n", [Timer, Reason]),
      ok
  end.

%% @doc Unicode sensitive binary lower caser
-spec to_lower(binary()) -> binary().
to_lower(Bin) ->
  to_lower(Bin, <<>>).

%% @private
to_lower(<<>>, Acc) ->
  Acc;
to_lower(<<C, Rest/binary>>, Acc) when $A =< C, C =< $Z ->
  to_lower(Rest, <<Acc/binary, (C+32)>>);
to_lower(<<195, C, Rest/binary>>, Acc) when 128 =< C, C =< 150 -> %% A-0 with tildes plus enye
  to_lower(Rest, <<Acc/binary, 195, (C+32)>>);
to_lower(<<195, C, Rest/binary>>, Acc) when 152 =< C, C =< 158 -> %% U and Y with tilde plus greeks
  to_lower(Rest, <<Acc/binary, 195, (C+32)>>);
to_lower(<<C, Rest/binary>>, Acc) ->
  to_lower(Rest, <<Acc/binary, C>>).
