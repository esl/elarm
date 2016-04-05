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
%%% Alarm List Plugin.
%%% @end
%%% Created : 30 Jul 2013 by Anders Nygren <erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(elarm_alarmlist).

-ignore_xref([{erlang, now, 0}]).

%% API
-export([init/1,
         new_alarm/2,
         repeat_alarm/2,
         acknowledge/4,
         unacknowledge/4,
         add_comment/4,
         clear/3,
         get_alarm/2,
         get_alarm/3,
         get_alarms/1]).

-include_lib("elarm/include/elarm.hrl").

-record(al_state, {alarmlist,
                   event_ids}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(proplists:proplist()) -> {ok, #al_state{}} | {error, term()}.
init(_Opts) ->
    AList = ets:new(alarmlist, [set]),
    EventIds = ets:new(event_ids, [set]),
    {ok, #al_state{ alarmlist = AList,
                    event_ids = EventIds }}.

-spec new_alarm(alarm(), #al_state{}) -> {ok, #al_state{}} | {error, term()}.
new_alarm(#alarm{ alarm_id = AlId, src = Src, event_id = EventId } = Alarm,
          #al_state{ alarmlist = AList, event_ids = EventIds } = State) ->
    Key = {AlId, Src},
    true = ets:insert(AList, {Key, Alarm}),
    true = ets:insert(EventIds, {EventId, Key}),
    {ok, State}.

-spec repeat_alarm(alarm(), #al_state{}) -> {ok, #al_state{}} | {error, term()}.
repeat_alarm(_Alarm, State) ->
    {ok, State}.

-spec acknowledge(alarm_id(), alarm_src(), ack_info(), #al_state{}) ->
          {ok, #al_state{}} | {error, term()}.
acknowledge(AlarmId, Src, AckInfo,
            #al_state{ alarmlist = AList } = State) ->
    [{Key, Alarm}] = ets:lookup(AList, {AlarmId, Src}),
    true = ets:insert(AList, {Key, Alarm#alarm{ ack_info = AckInfo,
                                                state = acknowledged }}),
    {ok, State}.

-spec unacknowledge(alarm_id(), alarm_src(), ack_info(), #al_state{}) ->
          {ok, #al_state{}} | {error, term()}.
unacknowledge(AlarmId, Src, AckInfo,
              #al_state{ alarmlist = AList } = State) ->
    [{Key, Alarm}] = ets:lookup(AList, {AlarmId, Src}),
    true = ets:insert(AList, {Key, Alarm#alarm{ ack_info = AckInfo,
                                                state = new }}),
    {ok, State}.

%% Add a comment to an alarm
-spec add_comment(alarm_id(), alarm_src(), comment(), #al_state{}) ->
          {ok, #al_state{}} | {error, term()}.
add_comment(AlarmId, Src, Comment, #al_state{ alarmlist = AList } = State) ->
    Key = {AlarmId, Src},
    [{_Key, #alarm{ comments = Cs } = Alarm}] = ets:lookup(AList, Key),
    true = ets:insert(AList, {Key, Alarm#alarm{ comments = [Comment|Cs] }}),
    {ok, State}.

%% Clear an alarm
-spec clear(alarm_id(), alarm_src(), #al_state{}) ->
          {ok, #al_state{}} | {error, term()}.
clear(AlarmId, Src,
      #al_state{ alarmlist = AList, event_ids = EventIds } = State) ->
    Key = {AlarmId, Src},
    [{_, #alarm{ event_id = EventId }}] = ets:lookup(AList, Key),
    true = ets:delete(EventIds, EventId),
    true = ets:delete(AList, Key),
    {ok, State}.

-spec get_alarm(event_id(), #al_state{}) ->
          {{ok, alarm()}, #al_state{}} | {{error, not_active}, #al_state{}}.
get_alarm(EventId,
          #al_state{ alarmlist = AList, event_ids = EventIds } = State) ->
    Result = case ets:lookup(EventIds, EventId) of
                 [{EventId, Key}] ->
                     [{Key, Alarm}] = ets:lookup(AList, Key),
                     {ok, Alarm};
                 [] ->
                     {error, not_active}
             end,
    {Result, State}.

-spec get_alarm(alarm_id(), alarm_src(), #al_state{}) ->
          {{ok, alarm()}, #al_state{}} | {error, not_active, #al_state{}}.
get_alarm(AlarmId, Src,
          #al_state{ alarmlist = AList } = State) ->
    Result = case ets:lookup(AList, {AlarmId, Src}) of
                 [{_Key, Alarm}] ->
                     {ok, Alarm};
                 [] ->
                     {error, not_active}
             end,
    {Result, State}.

-spec get_alarms(#al_state{}) ->
          {{ok, [alarm()]}, #al_state{}} | {error, term(), #al_state{}}.
get_alarms(#al_state{ alarmlist = AList } = State) ->
    Alarms = [Alarm || {_Key, Alarm} <- ets:tab2list(AList)],
    {{ok, Alarms}, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% EUnit Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

al_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [{"start_up", fun just_started/0},
      {"add alarm", fun add_alarm/0}
     ]}.

just_started() ->
    {ok, State} = init([]),
    ?assertEqual({{ok, []}, State}, get_alarms(State)),
    EvtId = erlang:now(),
    ?assertEqual({{error, not_active}, State}, get_alarm(EvtId, State)),
    ?assertEqual({{error, not_active}, State}, get_alarm(full, disk_1, State)).

add_alarm() ->
    {ok, State} = init([]),
    Alarm1 = one_alarm(),
    ?assertEqual({ok, State}, new_alarm(one_alarm(), State)),
    ?assertEqual({{ok, Alarm1}, State}, get_alarm(full, disk1, State)),
    AckInfo = #ack_info{},
    AckedAlarm = Alarm1#alarm{ state = acknowledged,
                               ack_info = AckInfo},
    ?assertEqual({ok, State}, acknowledge(full, disk1, AckInfo, State)),
    AckedAlarm = Alarm1#alarm{ state = acknowledged,
                               ack_info = AckInfo},
    ?assertMatch({{ok, AckedAlarm}, State}, get_alarm(full, disk1, State)),
    Comment = #comment{user = <<"me">>,
                       time = calendar:universal_time(),
                       text = <<"test">>},
    CommentedAlarm = AckedAlarm#alarm{comments = [Comment]},
    ?assertEqual({ok, State}, add_comment(full, disk1, Comment, State)),
    ?assertEqual({{ok, CommentedAlarm}, State}, get_alarm(full, disk1, State)),

    ?assertEqual({ok, State}, clear(full, disk1, State)),
    ?assertEqual({{error, not_active}, State}, get_alarm(full, disk1, State)),

    ?assertEqual({ok, State}, new_alarm(one_alarm(), State)),
    ?assertEqual({{ok, Alarm1}, State}, get_alarm(full, disk1, State)).


setup() ->
    application:load(elarm).

teardown(_) ->
    ok.

one_alarm() ->
    #alarm{
       alarm_id = full,
       alarm_type = undefined,
       src = disk1,
       event_time = {{2013, 8, 1}, {22, 27, 30}},
       event_id = {1375, 396050, 79296},
       severity = indeterminate,
       probable_cause = <<>>,
       proposed_repair_action = <<>>,
       description = <<>>,
       additional_information = [],
       correlated_events = [],
       comments = [],
       trend = undefined,
       threshold = undefined,
       state = new,
       ack_info = undefined}.

-endif.
