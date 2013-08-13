%%%-------------------------------------------------------------------
%%% @author Anders Nygren <anders.nygren@erlang-solutions.com>
%%% @copyright (C) 2013, Erlang Solution Ltd.
%%% @doc
%%% Alarm Management Application.
%%% This module implements the public API of the elarm application.
%%% @end
%%% Created : 26 Jul 2013 by Anders Nygren <erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(elarm).

%% API
-export([start_server/1,start_server/2,
         stop_server/0,
         stop_server/1,
         which_servers/0,
         raise/3,
         raise/4,
         clear/2,
         clear/3,
         subscribe/1,
         subscribe/2,
         unsubscribe/1,
         unsubscribe/2,
         acknowledge/2,
         acknowledge/3,
         add_comment/3,
         add_comment/4,
         manual_clear/2,
         manual_clear/3,
         read_log/1,
         read_log/2,
         get_alarms/0,
         get_alarms/1,
         get_configured/0,
         get_configured/1,
         get_unconfigured/0,
         get_unconfigured/1,
         get_all_configuration/0,
         get_all_configuration/1,
         get_default_configuration/0,
         get_default_configuration/1,
         add_configuration/2,
         add_configuration/3
        ]).

-include_lib("elarm/include/elarm.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% Start an Alarm Manager server.
-spec start_server(atom()|proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_server(Name) when is_atom(Name)->
    start_server(Name, []);
start_server(Opts) when is_list(Opts) ->
    start_server(elarm_server, []).

-spec start_server(atom(), proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_server(Name, Opts) ->
    elarm_sup:start_server(Name, Opts).

%% Stop an Alarm Manager server.
-spec stop_server() -> ok | {error, term()}.
stop_server() ->
    stop_server(elarm_sup).

%% Stop an Alarm Manager server.
-spec stop_server(atom()) -> ok | {error, term()}.
stop_server(Name) ->
    elarm_sup:stop_server(Name).

%% Get a list of all servers running.
-spec which_servers() -> [{atom(), pid()}].
which_servers() ->
    elarm_sup:which_servers().

%% -------------------------------------------------------------------
%% Functions used by an application or adaptation module to raise or
%% clear alarms

%% Raise an alarm.
-spec raise(alarm_id(), alarm_src(), additional_information()) -> ok.
raise(Id, Src, AddInfo) ->
    raise(elarm_server, Id, Src, AddInfo).

-spec raise(pid()|atom(), alarm_id(), alarm_src(), additional_information()) -> ok.
raise(Ref, Id, Src, AddInfo) ->
    elarm_server:raise(Ref, Id, Src, AddInfo).

%% Clear an alarm
-spec clear(alarm_id(), alarm_src()) -> ok.
clear(Id, Src) ->
    clear(elarm_server, Id, Src).

%% Clear an alarm
-spec clear(pid()|atom(), alarm_id(), alarm_src()) -> ok.
clear(Ref, Id, Src) ->
    elarm_server:clear(Ref, Id, Src).

%% -------------------------------------------------------------------
%% Functions used by presentation layer to access alarm status

%% Start subscription on alarm events matching Filter. all|event_type|orig
%% The subscriber will receive a message for every
%% - new alarm, {elarm, Ref, alarm()}
%% - ackmowledged alarm, {elarm, Ref, {alarm_id(), alarm_src(), event_id(), ack_info()}}
%% - cleared alarm, {elarm, Ref, {alarm_id(), alarm_src(), event_id()}}
%% - comment added, {elarm, Ref, {alarm_id(), alarm_src(), event_id, comment()}}
%% that match the Filter.
-spec subscribe(sub_filter()) -> {ok, reference(), [alarm()]}.  %% MFA filter=[all,[alarm_type], [src], summary]
subscribe(Filter) when is_list(Filter) ->
    subscribe(elarm_server, Filter).

-spec subscribe(pid()|atom(), sub_filter()) -> {ok, reference(), [alarm()]}.  %% MFA filter=[all,[alarm_type], [src], summary]
subscribe(Pid, Filter) when is_list(Filter) ->
    elarm_server:subscribe(Pid, Filter).

%% Cancel subscription.
-spec unsubscribe(reference()) -> ok.
unsubscribe(Ref) when is_reference(Ref) ->
    unsubscribe(elarm_server, Ref).

%% Cancel subscription.
-spec unsubscribe(pid()|atom(), reference()) -> ok.
unsubscribe(Pid, Ref) when is_reference(Ref) ->
    elarm_server:unsubscribe(Pid, Ref).

%% Start a subscription of alarm status summary matching Filter
%% The subscriber will receive a message {Ref, #alarm_summary{}}
%% everytime the alarm status summary matching the Filter changes

%% Acknowledge one or more alarms.
-spec acknowledge(event_id() | [event_id()], user_id()) -> ok | {error, term()}.
acknowledge(EventId, UserId) ->
    acknowledge(elarm_server, EventId, UserId).

-spec acknowledge(pid() | atom(), event_id() | [event_id()], user_id()) -> ok | {error, term()}.
acknowledge(Pid, EventId, UserId) ->
    elarm_server:acknowledge(Pid, EventId, UserId).

%% Add a comment to an alarm
-spec add_comment(event_id(), text(), user_id()) -> ok | {error, term()}.
add_comment(EventId, Text, UserId) ->
    add_comment(elarm_server, EventId, Text, UserId).

%% Add a comment to an alarm
-spec add_comment(pid()|atom(), event_id(), text(), user_id()) -> ok | {error, term()}.
add_comment(Pid, EventId, Text, UserId) ->
    elarm_server:add_comment(Pid, EventId, Text, UserId).

%% Manually clear an alarm
-spec manual_clear(event_id(), user_id()) -> ok | {error, term()}.
manual_clear(EventId, UserId) ->
    manual_clear(elarm_server, EventId, UserId).

%% Manually clear an alarm
-spec manual_clear(pid()|atom(), event_id(), user_id()) -> ok | {error, term()}.
manual_clear(Pid, EventId, UserId) ->
    elarm_server:manual_clear(Pid, EventId, UserId).

%% -------------------------------------------------------------------
%% Functions used by presentation layer to access alarm log

-spec read_log(term()) -> [alarm()].
read_log(Filter) ->
    read_log(elarm_server, Filter).

-spec read_log(pid()|atom(), term()) -> [alarm()].
read_log(Pid, Filter) ->
    elarm_server:read_log(Pid, Filter).

-spec get_alarms() -> [alarm()].    
get_alarms() ->
    get_alarms(elarm_server).

-spec get_alarms(pid()|atom()) -> [alarm()].    
get_alarms(Pid) ->
    elarm_server:get_alarms(Pid).

-spec get_configured() -> {ok, [{alarm_id(),alarm_config()}]} | {error, term()}.
get_configured() ->
    get_configured(elarm_server).
    
-spec get_configured(pid()|atom()) -> {ok, [{alarm_id(),alarm_config()}]} | {error, term()}.
get_configured(Pid) ->
    elarm_server:get_configured(Pid).
    
-spec get_unconfigured() -> {ok, [alarm_id()]} | {error, term()}.
get_unconfigured() ->
    get_unconfigured(elarm_server).

-spec get_unconfigured(pid()|atom()) -> {ok, [alarm_id()]} | {error, term()}.
get_unconfigured(Pid) ->
    elarm_server:get_unconfigured(Pid).

-spec get_all_configuration() -> {ok, [{alarm_id(),alarm_config()}]} | {error, term()}.
get_all_configuration() ->
    get_all_configuration(elarm_server).

-spec get_all_configuration(pid()|atom()) -> {ok, [{alarm_id(),alarm_config()}]} | {error, term()}.
get_all_configuration(Pid) ->
    elarm_server:get_all_configuration(Pid).

-spec get_default_configuration() -> {ok, alarm_config()} | {error, term()}.
get_default_configuration() ->
    get_default_configuration(elarm_server).

-spec get_default_configuration(pid()|atom()) -> {ok, alarm_config()} | {error, term()}.
get_default_configuration(Pid) ->
    elarm_server:get_default_configuration(Pid).

-spec add_configuration(alarm_id(), alarm_config()) -> ok | {error, term()}.
add_configuration(AlarmId, Config) ->
    add_configuration(elarm_server, AlarmId, Config).

-spec add_configuration(pid()|atom(), alarm_id(), alarm_config()) -> ok | {error, term()}.
add_configuration(Pid, AlarmId, Config) ->
    elarm_server:add_configuration(Pid, AlarmId, Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
