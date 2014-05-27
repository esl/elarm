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
%%% Supervisor for one alarm manager and its summary processes.
%%% @end
%%% Created : 20 Aug 2013 by Anders Nygren <anders.nygren@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(elarm_man_sup).

-behaviour(supervisor).

%% API
-export([start_link/2,
         which_summary_sup/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link(Name, Opts) ->
    supervisor:start_link(?MODULE, [Name, Opts]).

%% Get the pid of the alarm summary supervisor for the alarm server
which_summary_sup(ManagerSupPid) ->
    Children = supervisor:which_children(ManagerSupPid),
    [Pid] = [Pid || {_, Pid, _, [elarm_summary_sup]} <- Children],
    {ok, Pid}.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([Name, Opts]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [alarm_server_spec(Name, Opts), summary_sup_spec()]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
alarm_server_spec(Name, Opts) ->
    {Name, {elarm_server, start_link, [Name, Opts]},
     permanent, 2000, worker, [elarm_server]}.

summary_sup_spec() ->
    {summary_sup, {elarm_summary_sup, start_link, []},
     permanent, 2000, supervisor, [elarm_summary_sup]}.
