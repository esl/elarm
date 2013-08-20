%%%-------------------------------------------------------------------
%%% @author Anders Nygren <>
%%% @copyright (C) 2013, Anders Nygren
%%% @doc
%%%
%%% @end
%%% Created : 16 Aug 2013 by Anders Nygren <>
%%%-------------------------------------------------------------------
-module(elarm_summary_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,
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
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name) ->
    supervisor:start_link(?MODULE, [Name]).

start_child(AlarmServer, Filter) ->
    Super = gproc:lookup_local_name({elarm_summary_sup, AlarmServer}),
    supervisor:start_child(Super, [self(), AlarmServer, Filter]).

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
init([Name]) ->
    gproc:add_local_name({elarm_summary_sup, Name}),
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
