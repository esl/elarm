%%%-------------------------------------------------------------------
%%% @author Anders Nygren <anders.nygren@erlang-solutions.com>
%%% @copyright (C) 2013, Erlang Solution Ltd.
%%% @doc
%%% Main Alarm Manager server.
%%% @end
%%% Created : 30 Jul 2013 by Anders Nygren <anders.nygren@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(elarm_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
         start_link/1,
         raise/4,
         clear/3,
         subscribe/2,
         unsubscribe/2,
         acknowledge/3,
         add_comment/4,
         manual_clear/3,
         get_alarms/1,
         read_log/2,
         get_configured/1,
         get_unconfigured/1,
         get_all_configuration/1,
         get_default_configuration/1,
         add_configuration/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("elarm/include/elarm.hrl").

-define(SERVER, ?MODULE). 

-record(state, {alarmlist_cb,
                alarmlist_state,
                config_cb,
                config_state,
                log_cb,
                log_state,
                event_cb,
                event_state}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link([]).

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% Raise an alarm.
-spec raise(pid(), alarm_id(), alarm_src(), additional_information()) -> ok.
raise(Pid, Id, Src, AddInfo) ->
    gen_server:call(Pid, {raise, Id, Src, AddInfo}).

%% Clear an alarm
-spec clear(pid(), alarm_id(), alarm_src()) -> ok.
clear(Pid, Id, Src) ->
    gen_server:call(Pid, {clear, Id, Src}).

%% -------------------------------------------------------------------
%% Functions used by presentation layer to access alarm status

%% Start subscription on alarm events matching Filter. all|event_type|orig
%% The subscriber will receive a message for every
%% - new alarm, {new_alarm, Ref, alarm()}
%% - ackmowledged alarm, {acknowledged, Ref, event_id()}
%% - cleared alarm, {cleared_alarm, Ref, event_id()}
%% - comment added, {comment_added, Ref, event_id, comment()}
%% that match the Filter.
-spec subscribe(pid(), sub_filter()) -> reference().  %% MFA filter=[all,[alarm_type], [src], summary]
subscribe(Pid, Filter) ->
    gen_server:call(Pid, {subscribe, self(), Filter}).

%% Cancel subscription.
-spec unsubscribe(pid(), reference()) -> ok.
unsubscribe(Pid, Ref) ->
    gen_server:call(Pid, {unsubscribe, Ref}).

%% Start a subscription of alarm status summary matching Filter
%% The subscriber will receive a message {Ref, #alarm_summary{}}
%% everytime the alarm status summary matching the Filter changes

%% Acknowledge one or more alarms.
-spec acknowledge(pid(), event_id() | [event_id()], user_id()) -> ok | {error, term()}.
acknowledge(Pid, EventId, UserId) ->
    gen_server:call(Pid, {acknowledge, EventId, UserId}).

%% Add a comment to an alarm
-spec add_comment(pid(), event_id(), binary(), user_id()) -> ok | {error, term()}.
add_comment(Pid, EventId, Text, UserId) ->
    gen_server:call(Pid, {add_comment, EventId, Text, UserId}).

%% Manually clear an alarm
-spec manual_clear(pid(), event_id(), user_id()) -> ok | {error, term()}.
manual_clear(Pid, EventId, UserId) ->
    gen_server:call(Pid, {manual_clear, EventId, UserId}).

get_alarms(Pid) ->
    gen_server:call(Pid, get_alarms).

%% -------------------------------------------------------------------
%% Functions used by presentation layer to access alarm log

-spec read_log(pid(), term()) -> [alarm()].
read_log(Pid, Filter) ->
    gen_server:call(Pid, {read_log, Filter}).

get_configured(Pid) ->
    gen_server:call(Pid, get_configured).

get_unconfigured(Pid) ->
    gen_server:call(Pid, get_unconfigured).

get_all_configuration(Pid) ->
    gen_server:call(Pid, get_all_configuration).

get_default_configuration(Pid) ->
    gen_server:call(Pid, get_default_configuration).

add_configuration(Pid, AlarmId, Config) ->
    gen_server:call(Pid, {add_configuration, AlarmId, Config}).

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
init(Opts) ->
    {AlCB, AlState} = init_alarm_list(Opts),
    {CfgCB, CfgState} = init_config(Opts),
    {LogCB, LogState} = init_log(Opts),
    {EventCB, EventState} = init_event(Opts),
    {ok, #state{alarmlist_cb = AlCB,
                alarmlist_state = AlState,
                config_cb = CfgCB,
                config_state = CfgState,
                log_cb = LogCB,
                log_state = LogState,
                event_cb = EventCB,
                event_state = EventState}}.

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
handle_call({raise, AlarmId, Src, AddInfo}, _From, State) ->
    {Reply, NewState} = handle_raise(AlarmId, Src, AddInfo, State),
    {reply, Reply, NewState};
handle_call({clear, AlarmId, Src}, _From, State) ->
    {Reply, NewState} = handle_clear(AlarmId, Src, State),
    {reply, Reply, NewState};
handle_call({subscribe, Pid, Filter}, _From, State) ->
    {Reply, NewState} = handle_subscribe(Pid, Filter, State),
    {reply, Reply, NewState};
handle_call({unsubscribe, Ref}, _From, State) ->
    {Reply, NewState} = handle_unsubscribe(Ref, State),
    {reply, Reply, NewState};
handle_call({acknowledge, EventId, UserId}, _From, State) ->
    {Reply, NewState} = handle_acknowledge(EventId, UserId, State),
    {reply, Reply, NewState};
handle_call({add_comment, EventId, Text, UserId}, _From, State) ->
    {Reply, NewState} = handle_comment(EventId, Text, UserId, State),
    {reply, Reply, NewState};
handle_call({manual_clear, EventId, UserId}, _From, State) ->
    {Reply, NewState} = handle_manual_clear(EventId, UserId, State),
    {reply, Reply, NewState};
handle_call(get_alarms, _From, State) ->
    {Reply, NewState} = handle_get_alarms(State),
    {reply, Reply, NewState};
handle_call({read_log, Filter}, _From, State) ->
    {Reply, NewState} = handle_read_log(Filter, State),
    {reply, Reply, NewState};
handle_call(get_configured, _From, State) ->
    {Reply, NewState} = handle_get_configured(State),
    {reply, Reply, NewState};
handle_call(get_unconfigured, _From, State) ->
    {Reply, NewState} = handle_get_unconfigured(State),
    {reply, Reply, NewState};
handle_call(get_all_configuration, _From, State) ->
    {Reply, NewState} = handle_get_all_configuration(State),
    {reply, Reply, NewState};
handle_call(get_default_configuration, _From, State) ->
    {Reply, NewState} = handle_get_default_configuration(State),
    {reply, Reply, NewState};
handle_call({add_configuration, AlarmId, Config}, _From, State) ->
    {Reply, NewState} = handle_add_configuration(AlarmId, Config, State),
    {reply, Reply, NewState}.


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
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_alarm_list(Opts) ->
    init_plugin(alarmlist_cb, get_opts(alarmlist, Opts)).

init_config(Opts) ->
    init_plugin(config_cb, get_opts(config, Opts)).
    
init_log(Opts) ->
    init_plugin(log_cb, get_opts(log, Opts)).

init_event(Opts) ->
    init_plugin(event_cb, get_opts(event, Opts)).

get_opts(Name, Opts) ->
    proplists:get_value(Name, Opts, []).

init_plugin(Var, Opts) ->
    {ok, CB} = application:get_env(elarm, Var),
    {ok, State} = CB:init(Opts),
    {CB, State}.

handle_raise(AlarmId, Src, AddInfo,
             #state{ alarmlist_cb = AlCB,
                     alarmlist_state = AlState,
                     config_cb = CfgCB,
                     config_state = CfgState,
                     event_cb = EvtCB,
                     event_state = EvtState,
                     log_cb = LogCB,
                     log_state = LogState } = State) ->
    case get_alarm_config(AlarmId, CfgCB, CfgState) of
        {{ok, #alarm_config{ ignore = false } = Cfg}, NewCfgState} ->
            Alarm = create_alarm(AlarmId, Src, AddInfo, Cfg),
            {ok, NewLogState} = log_alarm(Alarm, LogCB, LogState),
            {ok, NewAlState} = update_alarmlist(Alarm, AlCB, AlState),
            {ok, NewEvtState} = send_new_events(Alarm, EvtCB, EvtState),
            {ok, State#state{ alarmlist_state = NewAlState,
                              config_state = NewCfgState,
                              event_state = NewEvtState,
                              log_state = NewLogState }};
        {ok, #alarm_config{ ignore = true }, NewCfgState} -> 
            {ok, State#state{ config_state = NewCfgState }}
    end.

get_alarm_config(AlarmId, CfgCB, CfgState) ->
    CfgCB:get_mapping(AlarmId, CfgState).

create_alarm(AlarmId, Src, AddInfo, Cfg) ->
    #alarm{
       alarm_id =AlarmId,
       alarm_type = get_alarm_type(Cfg),
       src = Src,
       event_time = timestamp(),
       event_id = new_event_id(),
       severity = get_severity(Cfg),
       probable_cause = get_probable_cause(Cfg),
       proposed_repair_action = get_proposed_repair_action(Cfg),
       description = get_description(Cfg),
       additional_information = AddInfo,
       correlated_events = [],
       comments = [],
       state = new
      }.

new_event_id() ->
    %% TODO: Fix this to something better
    erlang:now().

get_alarm_type(#alarm_config{ alarm_type = Type }) ->
    Type.
get_severity(#alarm_config{ severity = Severity }) ->
    Severity.
get_probable_cause(#alarm_config{ probable_cause = ProbCause }) ->
    ProbCause.
get_proposed_repair_action(#alarm_config{ proposed_repair_action = Action }) ->
    Action.
get_description(#alarm_config{ description = Description }) ->
    Description.

log_alarm(Alarm, LogCB, LogState) ->
    LogCB:new_alarm(Alarm, LogState).

update_alarmlist(Alarm, AlCB, AlState) ->
    AlCB:new_alarm(Alarm, AlState).

send_new_events(Alarm, EventCB, EventState) ->
    EventCB:new_alarm(Alarm, EventState).
     
handle_clear(AlarmId, Src, #state{ alarmlist_cb = AlCB,
                                   alarmlist_state = AlState,
                                   event_cb = EvtCB,
                                   event_state = EvtState,
                                   log_cb = LogCB,
                                   log_state = LogState } = State) ->
    case AlCB:get_alarm(AlarmId, Src, AlState) of
        {{ok, #alarm{ event_id = EventId }}, AlState1} ->
            {ok, NewLogState} = log_clear(AlarmId, Src, EventId, LogCB, LogState),
            {ok, NewAlState} = alarmlist_clear(AlarmId, Src, AlCB, AlState1),
            {ok, NewEvtState} = send_clear_events(AlarmId, Src, EventId,
                                                  EvtCB, EvtState),
            {ok, State#state{ alarmlist_state = NewAlState,
                              event_state = NewEvtState,
                              log_state = NewLogState }};
        {Error, NewAlState} ->
            {Error, State#state{ alarmlist_state = NewAlState}}
    end.

log_clear(AlarmId, Src, EventId, LogCB, LogState) ->    
    LogCB:clear(AlarmId, Src, EventId, LogState).

alarmlist_clear(AlarmId, Src, AlCB, AlState) ->
    AlCB:clear(AlarmId, Src, AlState).

send_clear_events(AlarmId, Src, EventId, EvtCB, EvtState) ->
    EvtCB:clear(AlarmId, Src, EventId, EvtState).

handle_acknowledge(EventId, UserId,
                   #state{ alarmlist_cb = AlCB,
                           alarmlist_state = AlState,
                           event_cb = EvtCB,
                           event_state = EvtState,
                           log_cb = LogCB,
                           log_state = LogState } = State) ->
    case AlCB:get_alarm(EventId, AlState) of
        {{ok, #alarm{ alarm_id = AlarmId, src = Src, state = new }}, AlState1} ->
            AckInfo = #ack_info{user = UserId,
                                time = timestamp()},
            {ok, NewLogState} = log_acknowledge(AlarmId, Src, EventId, AckInfo,
                                                LogCB, LogState),
            {ok, NewAlState} = alarmlist_acknowledge(AlarmId, Src, AckInfo,
                                                     AlCB, AlState1),
            {ok, NewEvtState} = send_acknowlegde_events(AlarmId, Src, AckInfo,
                                                        EventId, EvtCB, EvtState),
            {ok, State#state{ alarmlist_state = NewAlState,
                              event_state = NewEvtState,
                              log_state = NewLogState }};
        {{ok, #alarm{ state = acknowledged }}, NewAlState} ->
            {{error, acknowledged}, State#state{ alarmlist_state = NewAlState }};
        {Error, NewAlState} ->
            {Error, State#state{ alarmlist_state = NewAlState}}
    end.

log_acknowledge(AlarmId, Src, EventId, AckInfo, LogCB, LogState) ->
    LogCB:acknowledge(AlarmId, Src, EventId, AckInfo,LogState).

alarmlist_acknowledge(AlarmId, Src, AckInfo,AlCB, AlState) ->
    AlCB:acknowledge(AlarmId, Src, AckInfo,AlState).

send_acknowlegde_events(AlarmId, Src, EventId, AckInfo, EvtCB, EvtState) ->
    EvtCB:acknowledge(AlarmId, Src, EventId, AckInfo, EvtState).

handle_comment(EventId, Text, UserId, 
               #state{ alarmlist_cb = AlCB,
                       alarmlist_state = AlState,
                       event_cb = EvtCB,
                       event_state = EvtState,
                       log_cb = LogCB,
                       log_state = LogState } = State) ->
    case AlCB:get_alarm(EventId, AlState) of
        {{ok, #alarm{ alarm_id = AlarmId, src = Src }}, AlState1} ->
            Comment = #comment{ user = UserId,
                                time = timestamp(),
                                text = Text},
            {ok, NewLogState} = log_comment(AlarmId, Src, EventId, Comment,
                                            LogCB, LogState),
            {ok, NewAlState} = alarmlist_comment(AlarmId, Src, Comment,
                                                 AlCB, AlState1),
            {ok, NewEvtState} = send_comment_events(AlarmId, Src, Comment,
                                                    EventId, EvtCB, EvtState),
            {ok, State#state{ alarmlist_state = NewAlState,
                              event_state = NewEvtState,
                              log_state = NewLogState }};
        {Error, NewAlState} ->
            {Error, State#state{ alarmlist_state = NewAlState}}
    end.

log_comment(AlarmId, Src, EventId, Comment, LogCB, LogState) ->
    LogCB:add_comment(AlarmId, Src, EventId, Comment, LogState).

alarmlist_comment(AlarmId, Src, Comment, AlCB, AlState) ->
    AlCB:add_comment(AlarmId, Src, Comment, AlState).

send_comment_events(AlarmId, Src, Comment, EventId, EvtCB, EvtState) ->
    EvtCB:add_comment(AlarmId, Src, Comment, EventId, EvtState).
    
handle_manual_clear(EventId, UserId, State) ->
    {ok, State}.
    
handle_get_alarms(#state{ alarmlist_cb = AlCB,
                          alarmlist_state = AlState} = State) ->
    {Result, NewAlState} = AlCB:get_alarms(AlState),
    {Result, State#state{ alarmlist_state = NewAlState}}.

handle_subscribe(Pid, Filter, 
                 #state{ event_cb = EvtCB,
                         event_state = EvtState } = State) ->
    {Result, NewEvtState} = EvtCB:subscribe(Pid, Filter, EvtState),
    {Result, State#state{event_state = NewEvtState}}.
    
handle_unsubscribe(Ref, #state{ event_cb = EvtCB,
                                event_state = EvtState } = State) ->
    {Result, NewEvtState} = EvtCB:unsubscribe(Ref, EvtState),
    {Result, State#state{event_state = NewEvtState}}.
    
handle_read_log(Filter, State) ->
    {ok, State}.

handle_get_configured(#state{ config_cb = CfgCB,
                              config_state = CfgState } = State) ->
    {Result, NewCfgState} = CfgCB:get_configured(CfgState),
    {Result, State#state{ config_state = NewCfgState }}.

handle_get_unconfigured(#state{ config_cb = CfgCB,
                                config_state = CfgState } = State) ->
    {Result, NewCfgState} = CfgCB:get_unconfigured(CfgState),
    {Result, State#state{ config_state = NewCfgState }}.

handle_get_all_configuration(#state{ config_cb = CfgCB,
                                     config_state = CfgState } = State) ->
    {Result, NewCfgState} = CfgCB:get_all_configuration(CfgState),
    {Result, State#state{ config_state = NewCfgState }}.

handle_get_default_configuration(#state{ config_cb = CfgCB,
                                     config_state = CfgState } = State) ->
    {Result, NewCfgState} = CfgCB:get_default_configuration(CfgState),
    {Result, State#state{ config_state = NewCfgState }}.

handle_add_configuration(AlarmId, Config,
                         #state{ config_cb = CfgCB,
                                 config_state = CfgState } = State) ->
    {Result, NewCfgState} = CfgCB:add_configuration(AlarmId, Config, CfgState),
    {Result, State#state{ config_state = NewCfgState }}.

timestamp() ->
    calendar:universal_time().
