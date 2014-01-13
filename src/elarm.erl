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
         subscribe_summary/1,
         subscribe_summary/2,
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
    start_server(elarm_server, []).

%%--------------------------------------------------------------------
%% @doc
%% Start an Alarm Manager server. The server registers the Name.
%% @end
%%--------------------------------------------------------------------
-spec start_server(atom(), proplists:proplist()) -> {ok, pid()} |
                                                    {error, term()}.
start_server(Name, Opts) ->
    elarm_sup:start_server(Name, Opts).

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
    elarm_sup:stop_server(Name).

%%--------------------------------------------------------------------
%% @doc
%% Get a list of all servers running.
%% @end
%%--------------------------------------------------------------------
-spec which_servers() -> [{atom(), pid()}].
which_servers() ->
    elarm_sup:which_servers().

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
raise(Ref, Id, Src, AddInfo) ->
    elarm_server:raise(Ref, Id, Src, AddInfo).

%%--------------------------------------------------------------------
%% @doc
%% Clear an alarm
%% @equiv clear(elarm_server, Id, Src)
%% @end
%%--------------------------------------------------------------------
-spec clear(alarm_id(), alarm_src()) -> ok.
clear(Id, Src) ->
    clear(elarm_server, Id, Src).

%%--------------------------------------------------------------------
%% @doc
%% Clear an alarm
%% @end
%%--------------------------------------------------------------------
-spec clear(pid()|atom(), alarm_id(), alarm_src()) -> ok.
clear(Ref, Id, Src) ->
    elarm_server:clear(Ref, Id, Src).

%% -------------------------------------------------------------------
%% Functions used by presentation layer to access alarm status

%%--------------------------------------------------------------------
%% @doc
%% Start a subscription to alarms.
%% @equiv  subscribe(elarm_server, Filter)
%% @end
%%--------------------------------------------------------------------
-spec subscribe(sub_filter()) -> {ok, reference(), [alarm()]}.
subscribe(Filter) when is_list(Filter) ->
    subscribe(elarm_server, Filter).

%%--------------------------------------------------------------------
%% @doc
%% Start a subscription on alarm events matching Filter.
%%
%% The subscribe function returns a reference that is part of all messages received
%% as part of the subscription. It is also used in order to identify the subscription
%% when the subscription is cancelled by unsubscribe/1,2.
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
%% <dt>new alarm</dt><dd> {elarm, Ref, alarm()}</dd>
%% <dt>acknowledged alarm</dt><dd>{elarm, Ref, {ack, alarm_id(), alarm_src(), event_id(), ack_info()}}</dd>
%% <dt>cleared alarm</dt><dd>{elarm, Ref, {clear, alarm_id(), alarm_src(), event_id()}}</dd>
%% <dt>comment added</dt><dd>{elarm, Ref, {add_comment, alarm_id(), alarm_src(), event_id, comment()}}</dd>
%% </dl>
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe(pid()|atom(), sub_filter()) -> {ok, reference(), [alarm()]}.  %% MFA filter=[all,[alarm_type], [src], summary]
subscribe(Pid, Filter) when is_list(Filter) ->
    elarm_server:subscribe(Pid, Filter).

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
unsubscribe(Pid, Ref) when is_reference(Ref) ->
    elarm_server:unsubscribe(Pid, Ref).

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
%% Acknowledge one or more alarms.
%% @equiv acknowledge(elarm_server, EventId, UserId)
%% @end
%%--------------------------------------------------------------------
-spec acknowledge(event_id() | [event_id()], user_id()) -> ok | {error, term()}.
acknowledge(EventId, UserId) ->
    acknowledge(elarm_server, EventId, UserId).

%%--------------------------------------------------------------------
%% @doc
%% Acknowledge one or more alarms.
%% @end
%%--------------------------------------------------------------------
-spec acknowledge(pid() | atom(), event_id() | [event_id()], user_id()) ->
          ok | {error, term()}.
acknowledge(Pid, EventId, UserId) ->
    elarm_server:acknowledge(Pid, EventId, UserId).

%%--------------------------------------------------------------------
%% @doc
%% Add a comment to an alarm
%% @equiv add_comment(elarm_server, EventId, Text, UserId)
%% @end
%%--------------------------------------------------------------------
-spec add_comment(event_id(), text(), user_id()) -> ok | {error, term()}.
add_comment(EventId, Text, UserId) ->
    add_comment(elarm_server, EventId, Text, UserId).

%%--------------------------------------------------------------------
%% @doc
%% Add a comment to an alarm
%% @end
%%--------------------------------------------------------------------
-spec add_comment(pid()|atom(), event_id(), text(), user_id()) ->
          ok | {error, term()}.
add_comment(Pid, EventId, Text, UserId) ->
    elarm_server:add_comment(Pid, EventId, Text, UserId).

%%--------------------------------------------------------------------
%% @doc
%% Manually clear an alarm
%% @equiv manual_clear(elarm_server, EventId, UserId)
%% @end
%%--------------------------------------------------------------------
-spec manual_clear(event_id(), user_id()) -> ok | {error, term()}.
manual_clear(EventId, UserId) ->
    manual_clear(elarm_server, EventId, UserId).

%%--------------------------------------------------------------------
%% @doc
%% Manually clear an alarm
%% @end
%%--------------------------------------------------------------------
-spec manual_clear(pid()|atom(), event_id(), user_id()) -> ok | {error, term()}.
manual_clear(Pid, EventId, UserId) ->
    elarm_server:manual_clear(Pid, EventId, UserId).

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
read_log(Pid, Filter) ->
    elarm_server:read_log(Pid, Filter).

%%--------------------------------------------------------------------
%% @doc
%% Get all currently active alarms.
%% @equiv get_alarms(elarm_server)
%% @end
%%--------------------------------------------------------------------
-spec get_alarms() -> [alarm()].
get_alarms() ->
    get_alarms(elarm_server).

%%--------------------------------------------------------------------
%% @doc
%% Get all currently active alarms.
%% @end
%%--------------------------------------------------------------------
-spec get_alarms(pid()|atom()) -> [alarm()].
get_alarms(Pid) ->
    elarm_server:get_alarms(Pid).

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
get_configured(Pid) ->
    elarm_server:get_configured(Pid).

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
get_unconfigured(Pid) ->
    elarm_server:get_unconfigured(Pid).

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
get_all_configuration(Pid) ->
    elarm_server:get_all_configuration(Pid).

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
get_default_configuration(Pid) ->
    elarm_server:get_default_configuration(Pid).

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
add_configuration(Pid, AlarmId, Config) ->
    elarm_server:add_configuration(Pid, AlarmId, Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================
