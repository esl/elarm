%%%-------------------------------------------------------------------
%%% @author Anders Nygren <>
%%% @copyright (C) 2013, Anders Nygren
%%% @doc
%%%
%%% @end
%%% Created : 20 Aug 2013 by Anders Nygren <>
%%%-------------------------------------------------------------------
-module(elarm_man_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

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
start_link(Name, Opts) ->
    supervisor:start_link(?MODULE, [Name, Opts]).

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
init([Name, Opts]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [alarm_manager_spec(Name, Opts), summary_sup_spec(Name)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
alarm_manager_spec(Name, Opts) ->
    {Name, {elarm_server, start_link, [Name, Opts]},
     permanent, 2000, worker, [elarm_server]}.

summary_sup_spec(Name) ->
    {summary_sup, {elarm_summary_sup, start_link, [Name]},
     permanent, 2000, supervisor, [elarm_summary_sup]}.
