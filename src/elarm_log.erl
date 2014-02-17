%%%-------------------------------------------------------------------
%%% @author Anders Nygren <anders.nygren@erlang-solutions.com>
%%% @copyright (C) 2013, Erlang Solution Ltd.
%%% @doc
%%% Alarm log plugin.
%%%
%%% @end
%%% FIXME: Implement the functions of this module.
%%% Created : 30 Jul 2013 by Anders Nygren <anders.nygren@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(elarm_log).

%% API
-export([init/1,
         new_alarm/2,
         acknowledge/3,
         unacknowledge/3,
         add_comment/3,
         clear/2, clear/3,
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
-spec clear(alarm(), #log_state{}) -> {ok, #log_state{}} | {error, term()}.
clear(_Alarm, State) ->
    {ok, State}.

%% Manually clear an alarm
-spec clear(alarm(), user_id(), #log_state{}) ->
          {ok, #log_state{}} | {error, term()}.
clear(_Alarm, _UserId, State) ->
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
