%% Запускает корневой игровой супервизор, который запускает веб-сервер и прочие необходимые сервера
-module(game_sup).
-behaviour(supervisor).
-include("../include/game.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Options).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init(Options) ->
    ?Log("Starting the game supervisor..."),
    UsersDataPath = filename:join([?DataPath, "users.dets"]),
    UsersServerOptions = #user_server_options{dataPath=UsersDataPath},
    {ok, { {one_for_one, 5, 10}, [
				  ?CHILD(web_server, worker),
				  {user_server, {user_server, start_link, [UsersServerOptions]}, transient, 5000, worker, [user_server]},
				  {regions_sup, {regions_sup, start_link, []}, permanent, infinity, supervisor, [regions_sup]}
				 ]}
    }.
