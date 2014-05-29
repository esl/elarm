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
%%% Configuration plugin.
%%% @end
%%% Created : 30 Jul 2013 by Anders Nygren <anders.nygren@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(elarm_config).

%% API
-export([init/1,
         get_mapping/2,
         get_configured/1,
         get_unconfigured/1,
         get_all_configuration/1,
         get_default_configuration/1,
         add_configuration/3]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("elarm/include/elarm.hrl").

-record(config_state, { default_map,
                        config_tab,
                        missing_tab }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(proplists:proplist()) -> {ok, #config_state{}} | {error, term()}.
init(_Opts) ->
    CfgTab = ets:new(configured_alarms, [set]),
    MissingTab = ets:new(missing_alarms, [set]),
    {ok, DefaultMap} = application:get_env(elarm, def_alarm_mapping),
    DefConfig = create_record(DefaultMap),
    {ok, #config_state{ default_map = DefConfig,
                        config_tab = CfgTab,
                        missing_tab = MissingTab }}.

%% Get the alarm parameter mapping for AlarmId.
-spec get_mapping(alarm_id(), #config_state{}) ->
      {{ok, alarm_config()}, #config_state{}} | {error, term()}.
get_mapping(AlarmId, #config_state{ default_map = DefaultConfig,
                                    config_tab = CfgTab,
                                    missing_tab = MissingTab } = State) ->
    Result = case ets:lookup(CfgTab, AlarmId) of
                 [{AlarmId, Config}] ->
                     Config;
                 [] ->
                     ets:insert(MissingTab, {AlarmId}),
                     DefaultConfig
             end,
    {{ok, Result}, State}.

%% Functions used manage the configuration of alarm mappings

%% Get a list of all alarm_ids that have been received that do not
%% have mapping configuration.
-spec get_unconfigured(#config_state{}) ->
      {{ok, [alarm_id()]}, #config_state{}} | {error, term()}.
get_unconfigured(#config_state{ missing_tab = MissingTab } = State) ->
    Result = [AlarmId || {AlarmId} <- ets:tab2list(MissingTab)],
    {{ok, Result}, State}.

%% Get a list of all alarm_ids that have mapping configuration.
-spec get_configured(#config_state{}) ->
      {{ok, [{alarm_id(),alarm_config()}]}, #config_state{}} | {error, term()}.
get_configured(#config_state{ config_tab = CfgTab} = State) ->
    Result = ets:tab2list(CfgTab),
    {{ok, Result}, State}.

%% Get a list of all configuration data. Alarms that do not have
%% configuration will have a default configuration.
-spec get_all_configuration(#config_state{}) ->
      {{ok, [{alarm_id(),alarm_config()}]}, #config_state{}} | {error, term()}.
get_all_configuration(#config_state{ default_map = Default,
                                     config_tab = CfgTab,
                                     missing_tab = MissingTab } = State) ->
    Configured = ets:tab2list(CfgTab),
    UnConfigured = [{AlarmId,Default} || {AlarmId} <- ets:tab2list(MissingTab)],
    Result = Configured ++ UnConfigured,
    {{ok, Result}, State}.

get_default_configuration(#config_state{ default_map = Default } = State) ->
    {{ok,Default}, State}.

%% Configure alarm mapping
-spec add_configuration(alarm_id(), alarm_config(), #config_state{}) ->
      {ok, #config_state{}} | {error, term()}.
add_configuration(AlarmId, Config,
                  #config_state{ config_tab = CfgTab,
                                 missing_tab = MissingTab } = State) ->
    true = ets:insert(CfgTab, {AlarmId, Config}),
    true = ets:delete(MissingTab, AlarmId),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_record(DefaultMap) ->
    #alarm_config{
       alarm_type = proplists:get_value(alarm_type, DefaultMap),
       severity = proplists:get_value(severity, DefaultMap),
       probable_cause = proplists:get_value(probable_cause, DefaultMap),
       proposed_repair_action = proplists:get_value(proposed_repair_action, DefaultMap),
       description = proplists:get_value(description, DefaultMap),
       additional_information = proplists:get_value(additional_information, DefaultMap),
       trend = proplists:get_value(trend, DefaultMap),
       threshold = proplists:get_value(threshold, DefaultMap),
       manual_clear_allowed = proplists:get_value(manual_clear_allowed, DefaultMap),
       ack_required = proplists:get_value(ack_required, DefaultMap),
       log = proplists:get_value(log, DefaultMap),
       ignore = proplists:get_value(ignore, DefaultMap)
      }.

%%%===================================================================
%%% EUnit Tests
%%%===================================================================

config_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [{"start_up", fun just_started/0},
      {"unmapped alarm", fun unmapped_alarm/0},
      {"mapped alarm", fun mapped_alarm/0}
     ]}.

just_started() ->
    {ok,State} = init([]),
    ?assertEqual({{ok,[]}, State}, get_unconfigured(State)),
    ?assertEqual({{ok,[]}, State}, get_configured(State)),
    ?assertEqual({{ok,[]}, State}, get_all_configuration(State)),
    ?assertMatch({{ok,M}, #config_state{default_map=M}},
                 get_default_configuration(State)).

unmapped_alarm() ->
    {ok,State} = init([]),
    ?assertMatch({{ok,M}, #config_state{default_map=M}}, get_mapping(test, State)),
    ?assertEqual({{ok,[test]}, State}, get_unconfigured(State)),
    ?assertEqual({{ok,[]}, State}, get_configured(State)),
    {{ok, Default}, State} = get_default_configuration(State),
    ?assertEqual({{ok,[{test,Default}]}, State}, get_all_configuration(State)).

mapped_alarm() ->
    {ok,State} = init([]),
    {{ok,Default},State} = get_default_configuration(State),
    Map = Default#alarm_config{ severity = critical},
    ?assertEqual({ok,State}, add_configuration(alarm1, Map, State)),
    ?assertMatch({{ok,Map}, #config_state{}}, get_mapping(alarm1, State)),
    ?assertEqual({{ok,[]}, State}, get_unconfigured(State)),
    ?assertEqual({{ok,[{alarm1,Map}]}, State}, get_configured(State)),
    ?assertMatch({{ok,[{alarm1,Map}]}, #config_state{}}, get_all_configuration(State)),
    ?assertMatch({{ok,M}, #config_state{default_map=M}}, get_mapping(test, State)).

setup() ->
    application:load(elarm).

teardown(_) ->
    ok.
