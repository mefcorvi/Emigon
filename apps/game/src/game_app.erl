-module(game_app).
-behaviour(application).
-include("../include/game.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    log4erl:conf(filename:join(?PrivatePath, "log4erl.conf")),
    game_sup:start_link().

stop(_State) ->
    ok.
