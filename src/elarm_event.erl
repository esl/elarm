%%%-------------------------------------------------------------------
%%% @author Anders Nygren <anders.nygren@erlang-solutions.com>
%%% @copyright (C) 2013, Erlang Solution Ltd.
%%% @doc
%%% Event subscription plugin.
%%% @end
%%% Created : 31 Jul 2013 by Anders Nygren <anders.nygren@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(elarm_event).

%% API
-export([init/1,
         subscribe/3,
         unsubscribe/2,
         new_alarm/2,
         acknowledge/5,
         add_comment/5,
         clear/4,
         manual_clear/3,
         handle_down/2,
         filter_alarms/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("elarm/include/elarm.hrl").

-record(evt_state, {subs = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(proplists:proplist()) -> {ok, #evt_state{}} | {error, term()}.
init(_Opts) ->
    {ok, #evt_state{}}.

-spec subscribe(pid()|atom(), sub_filter(), #evt_state{}) -> {{ok,reference()},#evt_state{}}.  %% MFA filter=[all,[alarm_type], [src], summary]
subscribe(Pid, Filter, #evt_state{ subs = Subs } = State)->
    MRef = monitor(process, Pid),
    Ref = make_ref(),
    {{ok,Ref}, State#evt_state{ subs = add_subscriber(Ref, MRef, Pid, Filter, Subs)}}.

%% Cancel subscription.
-spec unsubscribe(reference(), #evt_state{}) -> {ok,#evt_state{}}.
unsubscribe(Ref, #evt_state{ subs = Subs } = State)->
    NewSubs = case find_subscriber(Ref, Subs) of
                  {ok, {Ref, MRef, _,_}} ->
                      demonitor(MRef, [flush]),
                      remove_subscriber(Ref, Subs);
                  {error, not_subscribed} ->
                      Subs
              end,
    {ok, State#evt_state{ subs = NewSubs }}.

-spec new_alarm(alarm(), #evt_state{}) -> {ok, #evt_state{}} | {error, term()}.
new_alarm(Alarm, #evt_state{ subs = Subs } = State) ->
    send_events(Alarm, Subs),
    {ok, State}.

-spec acknowledge(alarm_id(), alarm_src(), event_id(), ack_info(), #evt_state{}) -> {ok, #evt_state{}} | {error, term()}.
acknowledge(AlarmId, Src, EventId, AckInfo, #evt_state{ subs = Subs } =  State) ->
    Event = {ack, AlarmId, Src, EventId, AckInfo},
    send_events(Event, Subs),
    {ok, State}.

%% Add a comment to an alarm
-spec add_comment(alarm_id(), alarm_src(), event_id(), comment(), #evt_state{}) -> {ok, #evt_state{}} | {error, term()}.
add_comment(AlarmId, Src, EventId, Comment, #evt_state{ subs = Subs } =  State) -> 
    Event = {add_comment, AlarmId, Src, EventId, Comment},
    send_events(Event, Subs),
    {ok, State}.

%% Automatically clear an alarm

clear(AlarmId, Src, EventId, #evt_state{ subs = Subs } =  State) ->
    Event = {clear, AlarmId, Src, EventId},
    send_events(Event, Subs),
    {ok,State}.

%% Manually clear an alarm
-spec manual_clear(event_id(), user_id(), #evt_state{}) -> {ok, #evt_state{}} | {error, term()}.
manual_clear(EventId, UserId, #evt_state{ subs = Subs } =  State) ->
    {ok, State}.

%% Remove a subscriber when he has terminated
handle_down({'DOWN', _MRef, _Type, Pid, _Info},
            #evt_state{ subs = Subs } = EvtState) ->
    NewSubs = remove_subscriber(Pid, Subs),
    EvtState#evt_state{ subs = NewSubs }.

%% Filter a list of alarms.
filter_alarms(Alarms, Filter) ->
    lists:filter(fun (A) ->
                        test_filter(A, Filter) == match
                 end, Alarms).

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_subscriber(Ref, MRef, Pid, Filter, Subs) ->
    [{Ref, MRef, Pid, Filter} | Subs].

find_subscriber(Ref, Subs) ->
    case lists:keysearch(Ref, 1, Subs) of
        {value, S} ->
            {ok,S};
        false ->
            {error, not_subscribed}
    end.

remove_subscriber(Ref, Subs) when is_reference(Ref) ->
    [Sub || {Ref1, _, _, _}=Sub <- Subs, Ref/=Ref1];
remove_subscriber(Pid, Subs) when is_pid(Pid) ->
    [Sub || {_, _, Pid1, _}=Sub <- Subs, Pid/=Pid1].

send_events(Event, Subs) ->
    [maybe_send_event(Event, Sub) || Sub <- Subs].

maybe_send_event(Event, {Ref, _MRef, Pid, Filter}) ->
    case test_filter(Event, Filter) of
        match ->
            Pid!{elarm, Ref, Event};
        nomatch ->
            ok
    end.

test_filter(_Event, [all|_]) ->
    match;
test_filter(#alarm{alarm_type=Type}, [{type, Type}|_]) ->
    match;
test_filter(#alarm{src = Src}, [{src, Src}|_]) ->
    match;
test_filter({ack, _AlarmId, Src, _EventId, _AckInfo}, [{src,Src}|_]) ->
    match;
test_filter({add_comment, _AlarmId, Src, _EventId, _Comment}, [{src,Src}|_]) ->
    match;
test_filter({clear, _AlarmId, Src, _EventId}, [{src,Src}|_]) ->
    match;
test_filter(Event,[_|Filter]) ->
    test_filter(Event, Filter);
test_filter(_Event, []) ->
    nomatch.


%%%===================================================================
%%% EUnit Tests
%%%===================================================================
