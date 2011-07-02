-module(regions_sup).
-behaviour(supervisor).
-include("../include/game.hrl").

%% API
-export([start_link/0, lookup/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

lookup([X, Y]) when is_integer(X), is_integer(Y) ->
    gproc:lookup_local_name({region, [X, Y]}).

start_link() ->
    Pid = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    run_region_supervisors(),
    Pid.

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {region_server, {region_server, start_link, []},
	      Restart, Shutdown, Type, [region_server]},

    {ok, {SupFlags, [AChild]}}.

%% Private

read_regions() ->
    {ok, Terms} = file:consult(filename:join(?PrivatePath, "regions.conf")),
    Terms.

run_region_supervisors() ->
    Regions = read_regions(),
    run_next_supervisor(Regions).

run_next_supervisor([#region{}=Region|T]) ->
    {ok, Pid} = supervisor:start_child(?SERVER, [Region]),
    ?Log("Started new region server ~p", [Pid]),
    run_next_supervisor(T);

run_next_supervisor([]) ->
    [].
