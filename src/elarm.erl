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
%%% Alarm Management Application.
%%% This module implements the public API of the elarm application.
%%% @end
%%% Created : 26 Jul 2013 by Anders Nygren <erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(elarm).

%% API
-export([start_server/1,
         start_server/2,
         stop_server/0,
         stop_server/1,
         which_servers/0,
         raise/3,
         raise/4,
         clear/2,
         clear/3,
         clear/4,
         subscribe/2,
         subscribe/3,
         subscribe_summary/1,
         subscribe_summary/2,
         unsubscribe/1,
         unsubscribe/2,
         acknowledge/3,
         acknowledge/4,
         unacknowledge/3,
         unacknowledge/4,
         add_comment/4,
         add_comment/5,
         manual_clear/3,
         manual_clear/4,
         read_log/1,
         read_log/2,
         get_alarms/0,
         get_alarms/1,
         get_alarm_by_id/1,
         get_alarm_by_id/2,
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

%%--------------------------------------------------------------------
%% @doc
%% Start an Alarm Manager server.
%% @end
%%--------------------------------------------------------------------
-spec start_server(atom()|proplists:proplist()) -> {ok, pid()} |
                                                   {error, term()}.
start_server(Name) when is_atom(Name)->
    start_server(Name, []);
start_server(Opts) when is_list(Opts) ->
    start_server(elarm_server, Opts).

%%--------------------------------------------------------------------
%% @doc
%% Start an Alarm Manager server. The server registers the Name.
%% @end
%%--------------------------------------------------------------------
-spec start_server(atom(), proplists:proplist()) -> {ok, pid()} |
                                                    {error, term()}.
start_server(Name, Opts) ->
    elarm_man_sup_sup:start_server(Name, Opts).

%%--------------------------------------------------------------------
%% @doc
%% Stop an Alarm Manager server.
%% @equiv stop_server(elarm_server)
%% @end
%%--------------------------------------------------------------------
-spec stop_server() -> ok | {error, term()}.
stop_server() ->
    stop_server(elarm_server).

%%--------------------------------------------------------------------
%% @doc
%% Stop an Alarm Manager server.
%% @end
%%--------------------------------------------------------------------
-spec stop_server(atom()) -> ok | {error, term()}.
stop_server(Name) ->
    elarm_man_sup_sup:stop_server(Name).

%%--------------------------------------------------------------------
%% @doc
%% Get a list of all servers running.
%% @end
%%--------------------------------------------------------------------
-spec which_servers() -> [{atom(), pid()}].
which_servers() ->
    elarm_registry:which_servers().

%% -------------------------------------------------------------------
%% Functions used by an application or adaptation module to raise or
%% clear alarms

%%--------------------------------------------------------------------
%% @doc
%% Raise an alarm.
%% @equiv raise(elarm_server, Id, Src, AddInfo)
%% @end
%%--------------------------------------------------------------------
-spec raise(alarm_id(), alarm_src(), additional_information()) -> ok.
raise(Id, Src, AddInfo) ->
    raise(elarm_server, Id, Src, AddInfo).

%%--------------------------------------------------------------------
%% @doc
%% Raise an alarm.
%% @end
%%--------------------------------------------------------------------
-spec raise(pid()|atom(), alarm_id(), alarm_src(), additional_information()) -> ok.
raise(Srv, Id, Src, AddInfo) ->
    elarm_server:raise(Srv, Id, Src, AddInfo).

%%--------------------------------------------------------------------
%% @doc
%% Clear an alarm
%% @equiv clear(elarm_server, Id, Src, ok)
%% @end
%%--------------------------------------------------------------------
-spec clear(alarm_id(), alarm_src()) -> ok.
clear(Id, Src) ->
    clear(elarm_server, Id, Src, ok).

%%--------------------------------------------------------------------
%% @doc
%% Clear an alarm
%% @equiv clear(Srv, Id, Src, ok)
%% @end
%%--------------------------------------------------------------------
-spec clear(pid()|atom(), alarm_id(), alarm_src()) -> ok.
clear(Srv, Id, Src) ->
    clear(Srv, Id, Src, ok).

%%--------------------------------------------------------------------
%% @doc
%% Clear an alarm
%% @end
%%--------------------------------------------------------------------
-spec clear(pid()|atom(), alarm_id(), alarm_src(), clear_reason()) -> ok.
clear(Srv, Id, Src, Reason) ->
    elarm_server:clear(Srv, Id, Src, Reason).

%% -------------------------------------------------------------------
%% Functions used by presentation layer to access alarm status

%%--------------------------------------------------------------------
%% @doc
%% Start a subscription to alarms.
%% @equiv  subscribe(elarm_server, Filter)
%% @end
%%--------------------------------------------------------------------
-spec subscribe(sub_filter(), pid()) -> {ok, reference(), [alarm()]}.
subscribe(Filter, Subsc) when is_list(Filter) ->
    subscribe(elarm_server, Filter, Subsc).

%%--------------------------------------------------------------------
%% @doc
%% Start a subscription on alarm events matching Filter.
%%
%% The subscribe function returns a reference that is part of all messages
%% received as part of the subscription. It is also used in order to identify
%% the subscription when the subscription is cancelled by unsubscribe/1,2.
%%
%% The subscriber will receive a message for every alarm related event
%% that match the Filter.
%%
%% The filter is a list of filter elements, if one filter element matches
%% then the filter matches.
%%
%% FilterElement = all | {type, alarm_type()} | {src, alarm_src()}
%% <dl>
%% <dt>all</dt><dd>matches all alarms</dd>
%% <dt>{type, Type}</dt><dd>matches alarms with alarm_type == Type</dd>
%% <dt>{src, Src}</dt><dd>matches alarms with alarm_src == Src</dd>
%% </dl>
%%
%% The type and src filter elements may appear several times, in that case
%% each one is tried and if one matches then the filter matches.
%% The messages have the following format.
%% <dl>
%% <dt>new alarm</dt>
%% <dd> {elarm, Ref, alarm()}</dd>
%% <dt>acknowledged alarm</dt>
%% <dd>{elarm, Ref, {ack, alarm_id(), alarm_src(), event_id(),
%%      ack_info()}}</dd>
%% <dt>cleared alarm</dt>
%% <dd>{elarm, Ref, {clear, alarm_id(), alarm_src(), event_id()}}</dd>
%% <dt>comment added</dt>
%% <dd>{elarm, Ref, {add_comment, alarm_id(), alarm_src(), event_id,
%%      comment()}}</dd>
%% </dl>
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe(pid()|atom(), sub_filter(), pid()) ->
                                            {ok, reference(), [alarm()]}.
subscribe(Srv, Filter, Subsc) when is_list(Filter) ->
    elarm_server:subscribe(Srv, Filter, Subsc).

%%--------------------------------------------------------------------
%% @doc
%% Cancel subscription.
%% @equiv unsubscribe(elarm_server, Ref)
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe(reference()) -> ok.
unsubscribe(Ref) when is_reference(Ref) ->
    unsubscribe(elarm_server, Ref).

%%--------------------------------------------------------------------
%% @doc
%% Cancel subscription.
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe(pid()|atom(), reference()) -> ok.
unsubscribe(Srv, Ref) when is_reference(Ref) ->
    elarm_server:unsubscribe(Srv, Ref).

%%--------------------------------------------------------------------
%% @doc
%% Start a subscription of alarm status summary matching Filter
%% The subscriber will receive a message {elarm, Ref, #alarm_summary{}}
%% everytime the alarm status summary matching the Filter changes.
%% The filter is the same as in subscribe.
%% @equiv subscribe_summary(elarm_server, Filter)
%% @end
%%--------------------------------------------------------------------
-spec subscribe_summary(sub_filter()) -> {ok, reference()}.
subscribe_summary(Filter) ->
    subscribe_summary(elarm_server, Filter).

%%--------------------------------------------------------------------
%% @doc
%% Start a subscription of alarm status summary matching Filter
%% The subscriber will receive a message {elarm, Ref, #alarm_summary{}}
%% everytime the alarm status summary matching the Filter changes.
%% The filter is the same as in subscribe.
%% @end
%%--------------------------------------------------------------------
-spec subscribe_summary(pid()|atom(), sub_filter()) -> {ok, reference()}.
subscribe_summary(Server, Filter) ->
    {ok, Pid} = elarm_summary_sup:start_child(Server, Filter),
    Ref = elarm_summary:get_ref(Pid),
    {ok, Ref}.

%%--------------------------------------------------------------------
%% @doc
%% Acknowledge one alarm.
%% @equiv acknowledge(elarm_server, AlarmId, AlarmSrc, UserId)
%% @end
%%--------------------------------------------------------------------
-spec acknowledge(alarm_id(), alarm_src(), user_id()) -> ok | {error, term()}.
acknowledge(AlarmId, AlarmSrc, UserId) ->
    acknowledge(elarm_server, AlarmId, AlarmSrc, UserId).

%%--------------------------------------------------------------------
%% @doc
%% Acknowledge one alarm.
%% @end
%%--------------------------------------------------------------------
-spec acknowledge(pid() | atom(), alarm_id(), alarm_src(), user_id()) ->
                                                        ok | {error, term()}.
acknowledge(Srv, AlarmId, AlarmSrc, UserId) ->
    elarm_server:acknowledge(Srv, AlarmId, AlarmSrc, UserId).

%%--------------------------------------------------------------------
%% @doc
%% Unacknowledge one alarm.
%% @equiv unacknowledge(elarm_server, AlarmId, AlarmSrc, UserId)
%% @end
%%--------------------------------------------------------------------
-spec unacknowledge(alarm_id(), alarm_src(), user_id()) ->
                                                        ok | {error, term()}.
unacknowledge(AlarmId, AlarmSrc, UserId) ->
    unacknowledge(elarm_server, AlarmId, AlarmSrc, UserId).

%%--------------------------------------------------------------------
%% @doc
%% Unacknowledge one alarm.
%% @end
%%--------------------------------------------------------------------
-spec unacknowledge(pid() | atom(), alarm_id(), alarm_src(), user_id()) ->
                                                        ok | {error, term()}.
unacknowledge(Srv, AlarmId, AlarmSrc, UserId) ->
    elarm_server:unacknowledge(Srv, AlarmId, AlarmSrc, UserId).

%%--------------------------------------------------------------------
%% @doc
%% Add a comment to an alarm
%% @equiv add_comment(elarm_server, AlarmId, AlarmSrc, Text, UserId)
%% @end
%%--------------------------------------------------------------------
-spec add_comment(alarm_id(), alarm_src(), text(), user_id()) ->
                                                        ok | {error, term()}.
add_comment(AlarmId, AlarmSrc, Text, UserId) ->
    add_comment(elarm_server, AlarmId, AlarmSrc, Text, UserId).

%%--------------------------------------------------------------------
%% @doc
%% Add a comment to an alarm
%% @end
%%--------------------------------------------------------------------
-spec add_comment(pid()|atom(), alarm_id(), alarm_src(), text(), user_id()) ->
                                                        ok | {error, term()}.
add_comment(Srv, AlarmId, AlarmSrc, Text, UserId) ->
    elarm_server:add_comment(Srv, AlarmId, AlarmSrc, Text, UserId).

%%--------------------------------------------------------------------
%% @doc
%% Manually clear an alarm
%% @equiv manual_clear(elarm_server, AlarmId, AlarmSrc, UserId)
%% @end
%%--------------------------------------------------------------------
-spec manual_clear(alarm_id(), alarm_src(), user_id()) -> ok | {error, term()}.
manual_clear(AlarmId, AlarmSrc, UserId) ->
    manual_clear(elarm_server, AlarmId, AlarmSrc, UserId).

%%--------------------------------------------------------------------
%% @doc
%% Manually clear an alarm
%% @end
%%--------------------------------------------------------------------
-spec manual_clear(pid()|atom(), alarm_id(), alarm_src(), user_id()) ->
                                                        ok | {error, term()}.
manual_clear(Srv, AlarmId, AlarmSrc, UserId) ->
    clear(Srv, AlarmId, AlarmSrc, {manual, UserId}).

%% -------------------------------------------------------------------
%% Functions used by presentation layer to access alarm log

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec read_log(term()) -> [alarm()].
read_log(Filter) ->
    read_log(elarm_server, Filter).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec read_log(pid()|atom(), term()) -> [alarm()].
read_log(Srv, Filter) ->
    elarm_server:read_log(Srv, Filter).

%%--------------------------------------------------------------------
%% @doc
%% Get all currently active alarms.
%% @equiv get_alarms(elarm_server)
%% @end
%%--------------------------------------------------------------------
-spec get_alarms() -> {ok, [alarm()]} | {error, term()}.
get_alarms() ->
    get_alarms(elarm_server).

%%--------------------------------------------------------------------
%% @doc
%% Get all currently active alarms.
%% @end
%%--------------------------------------------------------------------
-spec get_alarms(pid()|atom()) -> {ok, [alarm()]} | {error, term()}.
get_alarms(Srv) ->
    elarm_server:get_alarms(Srv).

%%--------------------------------------------------------------------
%% @doc
%% Get alarm by id (event id).
%% @equiv get_alarm_by_id(elarm_server, EventId)
%% @end
%%--------------------------------------------------------------------
-spec get_alarm_by_id(event_id()) -> alarm() | undefined.
get_alarm_by_id(EventId) ->
    get_alarm_by_id(elarm_server, EventId).

%%--------------------------------------------------------------------
%% @doc
%% Get alarm by id (event id).
%% @end
%%--------------------------------------------------------------------
-spec get_alarm_by_id(pid()|atom(), event_id()) -> alarm() | undefined.
get_alarm_by_id(Srv, EventId) ->
    {ok, As} = get_alarms(Srv),
    case [A || A <- As, A#alarm.event_id =:= EventId] of
        [Alarm] ->
            Alarm;
        _ ->
            undefined
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get all currently configured alarms.
%% @equiv get_configured(elarm_server)
%% @end
%%--------------------------------------------------------------------
-spec get_configured() -> {ok, [{alarm_id(),alarm_config()}]} | {error, term()}.
get_configured() ->
    get_configured(elarm_server).

%%--------------------------------------------------------------------
%% @doc
%% Get all currently configured alarms.
%% @end
%%--------------------------------------------------------------------
-spec get_configured(pid()|atom()) -> {ok, [{alarm_id(),alarm_config()}]} |
                                      {error, term()}.
get_configured(Srv) ->
    elarm_server:get_configured(Srv).

%%--------------------------------------------------------------------
%% @doc
%% Get unconfigured alarms.
%% @equiv get_unconfigured(elarm_server)
%% @end
%%--------------------------------------------------------------------
-spec get_unconfigured() -> {ok, [alarm_id()]} | {error, term()}.
get_unconfigured() ->
    get_unconfigured(elarm_server).

%%--------------------------------------------------------------------
%% @doc
%% Get unconfigured alarms.
%% Get a list of all alarms that have been raised for which no
%% configuration data exists.
%% @end
%%--------------------------------------------------------------------
-spec get_unconfigured(pid()|atom()) -> {ok, [alarm_id()]} | {error, term()}.
get_unconfigured(Srv) ->
    elarm_server:get_unconfigured(Srv).

%%--------------------------------------------------------------------
%% @doc
%% Get all alarm configuration.
%% @equiv get_all_configuration(elarm_server)
%% @end
%%--------------------------------------------------------------------
-spec get_all_configuration() -> {ok, [{alarm_id(),alarm_config()}]} |
                                 {error, term()}.
get_all_configuration() ->
    get_all_configuration(elarm_server).

%%--------------------------------------------------------------------
%% @doc
%% Get all alarm configuration.
%% For configured alarms the actual configuration is returned, for
%% unconfigured alarms the default configuration is returned.
%% @end
%%--------------------------------------------------------------------
-spec get_all_configuration(pid()|atom()) ->
          {ok, [{alarm_id(),alarm_config()}]} | {error, term()}.
get_all_configuration(Srv) ->
    elarm_server:get_all_configuration(Srv).

%%--------------------------------------------------------------------
%% @doc
%% Get default configuration.
%% @equiv get_default_configuration(elarm_server)
%% @end
%%--------------------------------------------------------------------
-spec get_default_configuration() -> {ok, alarm_config()} | {error, term()}.
get_default_configuration() ->
    get_default_configuration(elarm_server).

%%--------------------------------------------------------------------
%% @doc
%% Get default configuration.
%% @end
%%--------------------------------------------------------------------
-spec get_default_configuration(pid()|atom()) -> {ok, alarm_config()} |
                                                 {error, term()}.
get_default_configuration(Srv) ->
    elarm_server:get_default_configuration(Srv).

%%--------------------------------------------------------------------
%% @doc
%% Add configuration data for an alarm_id.
%% @equiv add_configuration(elarm_server, AlarmId, Config)
%% @end
%%--------------------------------------------------------------------
-spec add_configuration(alarm_id(), alarm_config()) -> ok | {error, term()}.
add_configuration(AlarmId, Config) ->
    add_configuration(elarm_server, AlarmId, Config).

%%--------------------------------------------------------------------
%% @doc
%% Add configuration data for an alarm_id.
%% @end
%%--------------------------------------------------------------------
-spec add_configuration(pid()|atom(), alarm_id(), alarm_config()) ->
          ok | {error, term()}.
add_configuration(Srv, AlarmId, Config) ->
    elarm_server:add_configuration(Srv, AlarmId, Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
