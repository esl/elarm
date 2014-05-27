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
%%% Supervisor for the summary processes of one alarm server.
%%% @end
%%% Created : 16 Aug 2013 by Anders Nygren <anders.nygren@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(elarm_summary_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/2]).

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
start_link() ->
    supervisor:start_link(?MODULE, []).

start_child(AlarmServer, Filter) ->
    Servers = elarm_man_sup_sup:which_servers(),
    {AlarmServer, ManagerSupPid} = lists:keyfind(AlarmServer, 1, Servers),
    {ok, ServerSummarySupPid} = elarm_man_sup:which_summary_sup(ManagerSupPid),
    supervisor:start_child(ServerSummarySupPid, [self(), AlarmServer, Filter]).

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
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 2000,
    Type = worker,
    Child = {summary, {elarm_summary, start_link, []},
             Restart, Shutdown, Type, [elarm_summary]},
    {ok, {SupFlags, [Child]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
