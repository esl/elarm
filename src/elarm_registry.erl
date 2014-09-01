%%%-------------------------------------------------------------------
%%% Copyright (C) 2014, Erlang Solutions Ltd.
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
%%% @author Richard Jonas <richard.jonas@erlang-solutions.com>
%%% @doc
%%% Registry for elarm servers
%%% @end
%%%-------------------------------------------------------------------
-module(elarm_registry).

-behaviour(gen_server).

-export([start_link/0,
         subscribe/1,
         unsubscribe/1,
         which_servers/0,
         server_started/1,
         server_stopped/1]).

-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SERVER, ?MODULE).

-record(state, {
          servers           :: [{atom(), pid()}],   %% ServerName & Pid
          subscribers = []  :: [{pid(), reference()}]
         }).

%%%===================================================================
%%% API functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% Start elarm registry server with {local, elarm_registry} name.
%%% @end
%%%-------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%-------------------------------------------------------------------
%%% @doc
%%% Subscribe to listening elarm server events, e.g. starting
%%% new elarm server, stop elarm server, elarm server crashed.
%%% The subscriber process - identified by self() - will get messages
%%% when an elarm event happens.
%%% <dl>
%%%   <dt>{elarm_started, Name}</dt>
%%%   <dd>when an elarm server is started</dd>
%%%   <dt>{elarm_down, Name}</dt>
%%%   <dd>when an elarm server is stopped or crashed</dd>
%%% </dl>
%%% @end
%%%-------------------------------------------------------------------
-spec subscribe(pid()) -> {ok, [{atom(), pid()}]}.
subscribe(Subscriber) ->
    gen_server:call(?SERVER, {subscribe, Subscriber}).

%%%-------------------------------------------------------------------
%%% @doc
%%% Unsubscribe from elarm events
%%% @end
%%%-------------------------------------------------------------------
-spec unsubscribe(pid()) -> ok.
unsubscribe(Subscriber) ->
    gen_server:call(?SERVER, {unsubscribe, Subscriber}).

%%%-------------------------------------------------------------------
%%% @doc
%%% Get all registered elarm server processes
%%% @end
%%%-------------------------------------------------------------------
-spec which_servers() -> [{atom(), pid()}].
which_servers() ->
    gen_server:call(?SERVER, which_servers).

%%%-------------------------------------------------------------------
%%% @doc
%%% An elarm server has to call this when it is started in order that
%%% registry listeners knows about that elarm server.
%%% @end
%%%-------------------------------------------------------------------
-spec server_started(atom()) -> term().
server_started(Name) ->
    gen_server:cast(?SERVER, {elarm_started, Name, self()}).

%%%-------------------------------------------------------------------
%%% @doc
%%% Elarm server may call this when it terminates. Registry will send
%%% a message to its subscribers about that event. Elarm server is
%%% monitored by the registry, so calling this function is optional.
%%% @end
%%%-------------------------------------------------------------------
-spec server_stopped(atom()) -> term().
server_stopped(Name) ->
    gen_server:cast(?SERVER, {elarm_stopped, Name, self()}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_) ->
    {ok, #state{servers = elarm_man_sup_sup:which_servers()}}.

handle_call({subscribe, Pid}, _From, State) ->
    {reply, {ok, State#state.servers}, handle_subscribe(Pid, State)};
handle_call({unsubscribe, Pid}, _From, State) ->
    {reply, ok, handle_unsubscribe(Pid, State)};
handle_call(which_servers, _From, State) ->
    {reply, State#state.servers, State};
handle_call(Req, _From, State) ->
    %% lager:debug("Unsupported call: ~p", [Req]),
    {reply, {error, {unsupported, Req}}, State}.

handle_cast({elarm_started, Name, Pid}, State) ->
    {noreply, handle_server_started(Name, Pid, State)};
handle_cast({elarm_stopped, Name, Pid}, State) ->
    {noreply, handle_server_down(Name, Pid, State)};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({'DOWN', _MRef, _Type, {Name, Node}, _Info}, State)
  when Node =:= node() ->
    %% An elarm server went down
    case lists:keyfind(Name, 1, State#state.servers) of
        {Name, Pid} ->
            {noreply, handle_server_down(Name, Pid, State)};
        false ->
            {noreply, State}
    end;
handle_info({'DOWN', _MRef, _Type, Pid, _Info}, State) ->
    %% An external subscriber went down
    {noreply, handle_unsubscribe(Pid, State)};
handle_info(_Info, State) ->
    %% lager:debug("Unknown message ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_subscribe(Pid, #state{subscribers = Subs} = State) ->
    case lists:keyfind(Pid, 1, Subs) of
        false ->
            Mon = erlang:monitor(process, Pid),
            State#state{subscribers = [{Pid, Mon} | Subs]};
        _ ->
            %% already subscribed
            State
    end.

handle_unsubscribe(Pid, #state{subscribers = Subs} = State) ->
    case lists:keyfind(Pid, 1, Subs) of
        false ->
            %% already unsubscribed somehow
            State;
        {_, Mon} ->
            erlang:demonitor(Mon),
            State#state{subscribers = lists:keydelete(Pid, 1, Subs)}
    end.

handle_server_down(Name, Pid, #state{servers = Servers} = State) ->
    [Sub ! {elarm_down, Name, Pid}
        || {Sub, _Mon} <- State#state.subscribers],
    State#state{servers = lists:keydelete(Name, 1, Servers)}.

handle_server_started(Name, Pid, #state{servers = Servers} = State) ->
    erlang:monitor(process, Name),
    [Sub ! {elarm_started, Name, Pid}
        || {Sub, _Mon} <- State#state.subscribers],
    case lists:keyfind(Name, 1, Servers) of
        false ->
            State#state{servers = [{Name, Pid} | Servers]};
        _ ->
            State
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-define(GET_MSG(PATTERN),
        receive
            ?PATTERN = Msg ->
                Msg
        after
            100 ->
                {error, no_message}
        end).

subscribe_test_() ->
    {setup,
     local,
     fun() ->
         application:set_env(elarm, alarmlist_cb, elarm_alarmlist),
         application:set_env(elarm, config_cb, elarm_config),
         application:set_env(elarm, log_cb, elarm_log),
         application:set_env(elarm, event_cb, elarm_event),
         application:set_env(elarm, def_alarm_mapping,
                                      [{severity, indeterminate},
                                       {probable_cause, <<>>},
                                       {proposed_repair_action, <<>>},
                                       {description, <<>>},
                                       {additional_information, undefined},
                                       {correlated_events, []},
                                       {comments, []},
                                       {trend, undefined},
                                       {threshold, undefined},
                                       {manual_clear_allowed, true},
                                       {no_ack_required, false},
                                       {log, true},
                                       {ignore, false}]),
         {ok, P} = elarm_registry:start_link(),
         erlang:unlink(P),
         elarm_registry:subscribe(self())
     end,
     fun(_) ->
         exit(whereis(?MODULE), kill)
     end,
     [fun() ->
              {ok, P} = elarm_server:start_link(test, []),
              erlang:unlink(P),
              receive
                  {elarm_started, test, _} ->
                      ok
              end
      end,
      fun() ->
              exit(whereis(test), shutdown),
              receive
                  {elarm_down, test, _} ->
                      ok
              end
      end,
      fun() ->
              elarm_registry:unsubscribe(self()),
              {ok, P} = elarm_server:start_link(test2, []),
              erlang:unlink(P),
              receive
                  {elarm_started, test2, _} ->
                      ?assert(false)
              after
                  100 ->
                      ok
              end
      end
     ]
     }.

-endif.
