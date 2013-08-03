%%%-------------------------------------------------------------------
%%% @author Anders Nygren <anders.nygren@erlang-solutions.com>
%%% @copyright (C) 2013, Erlang Solution Ltd.
%%% @doc
%%% Top level supervisor for elarm.
%%% @end
%%% Created : 30 Jul 2013 by Anders Nygren <anders.nygren@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(elarm_sup).

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
    [{Name, Pid} || {Name, Pid, _, _} <- supervisor:which_children(?SERVER)].
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
    {Name, {elarm_server, start_link, [Name, Opts]},
     permanent, 2000, worker, [elarm_server]}.
