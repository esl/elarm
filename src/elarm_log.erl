%%%-------------------------------------------------------------------
%%% Copyright (C) 2013-2014, Erlang Solutions Ltd.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @author Anders Nygren <anders.nygren@erlang-solutions.com>
%%% @doc
%%% Alarm log plugin.
%%% @end
%%% FIXME: Implement the functions of this module.
%%% Created : 30 Jul 2013 by Anders Nygren <anders.nygren@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(elarm_log).

%% API
-export([init/1,
         new_alarm/2,
         repeat_alarm/2,
         acknowledge/3,
         unacknowledge/3,
         add_comment/3,
         clear/3,
         search/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("elarm/include/elarm.hrl").

-record(log_state,{}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(proplists:proplist()) -> {ok, #log_state{}} | {error, term()}.
init(_Opts) ->
    {ok, #log_state{}}.

-spec new_alarm(alarm(), #log_state{}) -> {ok, #log_state{}}.
new_alarm(_Alarm, State) ->
    {ok,State}.

-spec repeat_alarm(alarm(), #log_state{}) -> {ok, #log_state{}}.
repeat_alarm(_Alarm, State) ->
    {ok,State}.

-spec acknowledge(alarm(), ack_info(), #log_state{}) ->
          {ok, #log_state{}} | {error, term()}.
acknowledge(_Alarm, _AckInfo, State) ->
    {ok, State}.

-spec unacknowledge(alarm(), ack_info(), #log_state{}) ->
          {ok, #log_state{}} | {error, term()}.
unacknowledge(_Alarm, _AckInfo, State) ->
    {ok, State}.

%% Add a comment to an alarm
-spec add_comment(alarm(), comment(), #log_state{}) ->
          {ok, #log_state{}} | {error, term()}.
add_comment(_Alarm, _Comment, State) ->
    {ok, State}.

%% Automatically clear an alarm
-spec clear(alarm(), clear_reason(), #log_state{}) -> {ok, #log_state{}} | {error, term()}.
clear(_Alarm, _Reason, State) ->
    {ok, State}.

%% search the log
-spec search(log_filter(), #log_state{}) -> {ok, [alarm()], #log_state{}} |
                                            {error, term()}.
search(_Filter, State) ->
    {ok, [], State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%===================================================================
%%% EUnit Tests
%%%===================================================================
