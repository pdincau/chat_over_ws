%%%-------------------------------------------------------------------
%%% @author Paolo <paolo@ubuntu.ubuntu-domain>
%%% @copyright (C) 2012, Paolo
%%% @doc
%%%
%%% @end
%%% Created : 15 Jul 2012 by Paolo <paolo@ubuntu.ubuntu-domain>
%%%-------------------------------------------------------------------
-module(chat_over_ws_supersup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    English = {english, {room, start_link, [english]},
	       Restart, Shutdown, Type, [room]},
    
    French = {french, {room, start_link, [french]},
	      Restart, Shutdown, Type, [room]},
    
    Italian = {italian, {room, start_link, [italian]},
	      Restart, Shutdown, Type, [room]},

    {ok, {SupFlags, [English, French, Italian]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
