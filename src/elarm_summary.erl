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
%%% Alarm status summary server.
%%%
%%% Alarm Summary gives a summary of the presence or absence of unacknowledged
%%% and acknowledged alarms of the various severities. This is useful for e.g.
%%% show the status on maps or other overview user interfaces.
%%%
%%% Each process that wants to know about changes in this summary can subscribe
%%% to getting alarm summaries by calling `elarm:subscribe_summary(Server,
%%% Filter)'. This will start an `elarm_summary' process (under an
%%% `elarm_summary_sup' supervisor), which subscribes to alarms, and notifies
%%% its subscribers about the updated summary when it receives an alarm.
%%% @end
%%%
%%% Created : 14 Aug 2013 by Anders Nygren <anders.nygren@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(elarm_summary).

-behaviour(gen_server).

%% API
-export([start_link/3,
         get_ref/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("elarm/include/elarm.hrl").

-define(SERVER, ?MODULE).

-record(summary, {
          new_critical      = 0,
          new_major         = 0,
          new_minor         = 0,
          new_warning       = 0,
          new_indeterminate = 0,
          critical          = 0,
          major             = 0,
          minor             = 0,
          warning           = 0,
          indeterminate     = 0
         }).

-record(state, {
          client,
          ref,                     % The client is subscribed to the summary
                                   % server with this reference.
          alarmlist_ref,           % The summary server is subscribed to the
                                   % elarm server with this reference.
          mref,                    % The summary server monitors the client with
                                   % this reference.
          status  = #alarm_summary{},
          alarms  = new_alarms(),  % Cache for the alarms in which the summary
                                   % server is interested.
          summary = #summary{}
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Client, Server, Filter) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Client, Server, Filter) ->
    gen_server:start_link(?MODULE, [Client, Server, Filter], [{debug,[trace]}]).

get_ref(Pid) ->
    gen_server:call(Pid, get_ref).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Client, AlarmServer, Filter]) ->
    MRef = monitor(process, Client),
    {ok, AlRef, AlarmList} = elarm_server:subscribe(AlarmServer, Filter, self()),
    {Summary, Alarms} = process_alarmlist(AlarmList),
    Ref = make_ref(),
    NewStatus = maybe_send_event(Ref, Client, Summary, undefined),
    {ok, #state{ client = Client,
                 ref = Ref,
                 mref = MRef,
                 alarmlist_ref = AlRef,
                 status = NewStatus,
                 alarms = Alarms,
                 summary = Summary }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_ref, _From, #state{ ref = Ref } = State) ->
    {reply, Ref, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({elarm, AlRef, #alarm{} = Alarm},
            #state{ ref = Ref,
                    client = Client,
                    alarmlist_ref = AlRef,
                    status = Status,
                    alarms = Alarms,
                    summary = Summary } = State) ->
    {NewSummary, NewAlarms} = new_alarm(Alarm, Summary, Alarms),
    NewStatus = maybe_send_event(Ref, Client, NewSummary, Status),
    {noreply, State#state{ status = NewStatus,
                           alarms = NewAlarms,
                           summary = NewSummary }};

handle_info({elarm, AlRef, {ack, AlarmId, AlarmSrc, _EventId, _AckInfo}},
            #state{ ref = Ref,
                    client = Client,
                    alarmlist_ref = AlRef,
                    status = Status,
                    summary = Summary,
                    alarms = Alarms} = State) ->
    Severity = get_severity(AlarmId, AlarmSrc, Alarms),
    NewSummary = update_summary_ack(Summary, Severity),
    NewStatus = maybe_send_event(Ref, Client, Summary, Status),
    {noreply, State#state{ status = NewStatus,
                           summary = NewSummary}};

handle_info({elarm, AlRef, {clear, AlarmId, AlarmSrc, _EventId}},
            #state{ ref = Ref,
                    client = Client,
                    alarmlist_ref = AlRef,
                    status = Status,
                    alarms = Alarms,
                    summary = Summary } = State) ->
    {NewSummary, NewAlarms} = clear_alarm(AlarmId, AlarmSrc, Summary, Alarms),
    NewStatus = maybe_send_event(Ref, Client, NewSummary, Status),
    {noreply, State#state{ status = NewStatus,
                           alarms = NewAlarms,
                           summary = NewSummary }};

handle_info({elarm, ref, {add_comment, _AlarmId, _AlarmSrc, _EventId, _Comment}},
            State) ->
    %% Ignore
    {noreply, State};

handle_info({'DOWN', MRef, _Type, _Pid, _Info}, #state{ mref = MRef } = State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_severity(AlarmId, AlarmSrc, Alarms) ->
    {Severity, _State} = get_alarm(AlarmId, AlarmSrc, Alarms),
    Severity.

new_alarms() ->
    dict:new().

add_alarm(Id, Src, Severity, State, Alarms) ->
    dict:store({Id,Src}, {Severity, State}, Alarms).

get_alarm(Id, Src, Alarms) ->
    dict:fetch({Id,Src},Alarms).

delete_alarm(Id, Src, Alarms) ->
    dict:erase({Id, Src}, Alarms).

process_alarmlist(AlarmList) ->
    {_Summary, _Alarms} = lists:foldr(fun new_alarm/2,
                                      {#summary{}, new_alarms()},
                                      AlarmList).

new_alarm(Alarm, {Summary, Alarms}) ->
    new_alarm(Alarm, Summary, Alarms).

new_alarm(#alarm{alarm_id = Id, src = Src, severity = Severity, state = AlState},
          Summary, Alarms) ->
    NewSummary = increment_count(Summary, Severity, AlState),
    NewAlarms = add_alarm(Id, Src, Severity, AlState, Alarms),
    {NewSummary, NewAlarms}.

increment_count(#summary{ new_critical = N }, critical, new)->
    #summary{ new_critical = N+1 };
increment_count(#summary{ new_major = N }, major, new)->
    #summary{ new_major = N+1 };
increment_count(#summary{ new_minor = N }, minor, new)->
    #summary{ new_minor = N+1 };
increment_count(#summary{ new_warning = N }, warning, new)->
    #summary{ new_warning = N+1 };
increment_count(#summary{ new_indeterminate = N }, indeterminate, new)->
    #summary{ new_indeterminate = N+1 };
increment_count(#summary{ critical = N }, critical, acknowledged)->
    #summary{ critical = N+1 };
increment_count(#summary{ major = N }, major, acknowledged)->
    #summary{ major = N+1 };
increment_count(#summary{ minor = N }, minor, acknowledged)->
    #summary{ minor = N+1 };
increment_count(#summary{ warning = N }, warning, acknowledged)->
    #summary{ warning = N+1 };
increment_count(#summary{ indeterminate = N }, indeterminate, acknowledged)->
    #summary{ indeterminate = N+1 }.

decrement_count(#summary{ new_critical = N }, critical, new)->
    #summary{ new_critical = N-1 };
decrement_count(#summary{ new_major = N }, major, new)->
    #summary{ new_major = N-1 };
decrement_count(#summary{ new_minor = N }, minor, new)->
    #summary{ new_minor = N-1 };
decrement_count(#summary{ new_warning = N }, warning, new)->
    #summary{ new_warning = N-1 };
decrement_count(#summary{ new_indeterminate = N }, indeterminate, new)->
    #summary{ new_indeterminate = N-1 };
decrement_count(#summary{ critical = N }, critical, acknowledged)->
    #summary{ critical = N-1 };
decrement_count(#summary{ major = N }, major, acknowledged)->
    #summary{ major = N-1 };
decrement_count(#summary{ minor = N }, minor, acknowledged)->
    #summary{ minor = N-1 };
decrement_count(#summary{ warning = N }, warning, acknowledged)->
    #summary{ warning = N-1 };
decrement_count(#summary{ indeterminate = N }, indeterminate, acknowledged)->
    #summary{ indeterminate = N-1 }.

create_status(#summary{
                 new_critical      = NC,
                 new_major         = NMaj,
                 new_minor         = NMin,
                 new_warning       = NW,
                 new_indeterminate = NI,
                 critical          = C,
                 major             = Maj,
                 minor             = Min,
                 warning           = W,
                 indeterminate     = I
                } = _Summary) ->
    #alarm_summary{
       new_critical      = NC > 0,
       new_major         = NMaj > 0,
       new_minor         = NMin > 0,
       new_warning       = NW > 0,
       new_indeterminate = NI > 0,
       critical          = C > 0,
       major             = Maj > 0,
       minor             = Min > 0,
       warning           = W > 0,
       indeterminate     = I > 0
      }.

maybe_send_event(Ref, Client, Summary, Status) ->
    case create_status(Summary) of
        Status ->
            Status;
        Status1 ->
            send_event(Ref, Client, Status1),
            Status1
    end.

send_event(Ref, Client, Status) ->
    Client!{elarm, Ref, Status}.

update_summary_ack(Summary, Severity) ->
    NewSummary = decrement_count(Summary, Severity, new),
    increment_count(NewSummary, Severity, acknowledged).

clear_alarm(AlarmId, AlarmSrc, Summary, Alarms) ->
    {Severity,State} = get_alarm(AlarmId, AlarmSrc, Alarms),
    NewSummary = decrement_count(Summary, Severity, State),
    NewAlarms = delete_alarm(AlarmId, AlarmSrc, Alarms),
    {NewSummary, NewAlarms}.
