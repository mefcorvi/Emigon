%% Запускает корневой игровой супервизор, который запускает веб-сервер и прочие необходимые сервера
-module(game_sup).
-behaviour(supervisor).
-include("../include/game.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    {ok, { {one_for_one, 5, 10}, [
				  ?CHILD(web_server, worker),
				  ?CHILD(user_server, worker),
				  {region1_server, {region_server, start_link, [1]}, permanent, 5000, worker, [region_server]}
				 ]}
    }.
