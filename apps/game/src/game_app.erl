-module(game_app).
-behaviour(application).
-include("../include/game.hrl").

%% Application callbacks
-export([start/0, start/1, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    start(normal, []).

start(Options) ->
    start(normal, Options).

start(_StartType, Options) ->
    error_logger:delete_report_handler(error_logger),
    error_logger:tty(false),
    log4erl:conf(filename:join(?PrivatePath, "log4erl.conf")),
    log4erl:error_logger_handler(),
    ?Log("Game application was started. Options: ~p", [Options]),
    game_sup:start_link(Options).

stop(_State) ->
    ok.
