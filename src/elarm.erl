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
-export([start_server/1,
         stop_server/1,
         which_servers/0,
         raise/4,
         clear/3,
         subscribe/2,
         unsubscribe/2,
         acknowledge/3,
         add_comment/4,
         manual_clear/3,
         read_log/2,
         get_alarms/1,
         get_configured/1,
         get_unconfigured/1,
         get_all_configuration/1,
         get_default_configuration/1,
         add_configuration/3
        ]).

-include_lib("elarm/include/elarm.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% Start an Alarm Manager server.
-spec start_server(term()) -> {ok, pid()} | {error, term()}.
start_server(Name) ->
    elarm_sup:start_server(Name).

%% Stop an Alarm Manager server.
-spec stop_server(term()) -> {ok, pid()} | {error, term()}.
stop_server(Name) ->
    elarm_sup:stop_server(Name).

%% Get a list of all servers running.
-spec which_servers() -> [{term(), pid()}].
which_servers() ->
    elarm_sup:which_servers().

%% -------------------------------------------------------------------
%% Functions used by an application or adaptation module to raise or
%% clear alarms

%% Raise an alarm.
-spec raise(pid(), alarm_id(), alarm_src(), additional_information()) -> ok.
raise(Pid, Id, Src, AddInfo) ->
    elarm_server:raise(Pid, Id, Src, AddInfo).

%% Clear an alarm
-spec clear(pid(), alarm_id(), alarm_src()) -> ok.
clear(Pid, Id, Src) ->
    elarm_server:clear(Pid, Id, Src).

%% -------------------------------------------------------------------
%% Functions used by presentation layer to access alarm status

%% Start subscription on alarm events matching Filter. all|event_type|orig
%% The subscriber will receive a message for every
%% - new alarm, {elarm, Ref, alarm()}
%% - ackmowledged alarm, {elarm, Ref, {alarm_id(), alarm_src(), event_id(), ack_info()}}
%% - cleared alarm, {elarm, Ref, {alarm_id(), alarm_src(), event_id()}}
%% - comment added, {elarm, Ref, {alarm_id(), alarm_src(), event_id, comment()}}
%% that match the Filter.
-spec subscribe(pid(), sub_filter()) -> reference().  %% MFA filter=[all,[alarm_type], [src], summary]
subscribe(Pid, Filter) when is_list(Filter)->
    elarm_server:subscribe(Pid, Filter).

%% Cancel subscription.
-spec unsubscribe(pid(), reference()) -> ok.
unsubscribe(Pid, Ref) when is_reference(Ref)->
    elarm_server:unsubscribe(Pid, Ref).

%% Start a subscription of alarm status summary matching Filter
%% The subscriber will receive a message {Ref, #alarm_summary{}}
%% everytime the alarm status summary matching the Filter changes

%% Acknowledge one or more alarms.
-spec acknowledge(pid(), event_id() | [event_id()], user_id()) -> ok | {error, term()}.
acknowledge(Pid, EventId, UserId) ->
    elarm_server:acknowledge(Pid, EventId, UserId).

%% Add a comment to an alarm
-spec add_comment(pid(), event_id(), text(), user_id()) -> ok | {error, term()}.
add_comment(Pid, EventId, Text, UserId) ->
    elarm_server:add_comment(Pid, EventId, Text, UserId).

%% Manually clear an alarm
-spec manual_clear(pid(), event_id(), user_id()) -> ok | {error, term()}.
manual_clear(Pid, EventId, UserId) ->
    elarm_server:manual_clear(Pid, EventId, UserId).
%% -------------------------------------------------------------------
%% Functions used by presentation layer to access alarm log

-spec read_log(pid(), term()) -> [alarm()].
read_log(Pid, Filter) ->
    elarm_server:read_log(Pid, Filter).

-spec get_alarms(pid()) -> [alarm()].    
get_alarms(Pid) ->
    elarm_server:get_alarms(Pid).

-spec get_configured(pid()) -> {ok, [{alarm_id(),alarm_config()}]} | {error, term()}.
get_configured(Pid) ->
    elarm_server:get_configured(Pid).
    
-spec get_unconfigured(pid()) -> {ok, [alarm_id()]} | {error, term()}.
get_unconfigured(Pid) ->
    elarm_server:get_unconfigured(Pid).

-spec get_all_configuration(pid()) -> {ok, [{alarm_id(),alarm_config()}]} | {error, term()}.
get_all_configuration(Pid) ->
    elarm_server:get_all_configuration(Pid).

-spec get_default_configuration(pid()) -> {ok, alarm_config()} | {error, term()}.
get_default_configuration(Pid) ->
    elarm_server:get_default_configuration(Pid).

-spec add_configuration(pid(), alarm_id(), alarm_config()) -> ok | {error, term()}.
add_configuration(Pid, AlarmId, Config) ->
    elarm_server:add_configuration(Pid, AlarmId, Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
