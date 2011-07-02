-module(game_app).
-behaviour(application).
-include("../include/game.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    error_logger:delete_report_handler(error_logger),
    error_logger:tty(false),
    log4erl:conf(filename:join(?PrivatePath, "log4erl.conf")),
    log4erl:error_logger_handler(),
    game_sup:start_link().

stop(_State) ->
    ok.
