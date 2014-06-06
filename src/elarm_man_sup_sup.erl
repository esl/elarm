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
%%% @author Luca Favatella <luca.favatella@erlang-solutions.com>
%%% @doc
%%% Supervisor for the supervisors for all the alarm managers.
%%% @end
%%% Created : 06 Jun 2014 by Luca Favatella <luca.favatella@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(elarm_man_sup_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_server/2,
         stop_server/1,
         which_servers/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Start a new alarm manager.
start_server(Name, Opts) ->
    Spec = alarm_manager_spec(Name, Opts),
    supervisor:start_child(?SERVER, Spec).

%% Stop an alarm manager
stop_server(Name) ->
    ok = supervisor:terminate_child(?SERVER, Name),
    ok = supervisor:delete_child(?SERVER, Name).

%% Get a list of all servers running
which_servers() ->
    Children =
        case whereis(?SERVER) of
            undefined ->             [];
            SupPid when is_pid(SupPid) ->
                supervisor:which_children(?SERVER)
        end,
    [{Name, Pid}
     || {Name, Pid, _, [elarm_man_sup]} <- Children].

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
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Servers = mk_servers_specs(),
    {ok, {SupFlags, Servers}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

mk_servers_specs() ->
    {ok,Servers} = application:get_env(elarm, servers),
    [alarm_manager_spec(Name, Opts) || {Name, Opts} <- Servers].

alarm_manager_spec(Name, Opts) ->
    {Name, {elarm_man_sup, start_link, [Name, Opts]},
     permanent, 2000, supervisor, [elarm_man_sup]}.
