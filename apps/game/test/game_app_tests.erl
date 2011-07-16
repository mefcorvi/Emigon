-module(game_app_tests).
-compile(export_all).
-include("../include/game.hrl").
-include_lib("eunit/include/eunit.hrl").

start_without_args_test() ->
    code:unstick_mod(error_logger),    
    meck:new(error_logger),
    meck:expect(error_logger, delete_report_handler, fun(error_logger) -> ok end),
    meck:expect(error_logger, tty, fun(false) -> ok end),
    meck:new(log4erl),
    meck:expect(log4erl, conf, fun(_) -> ok end),
    meck:expect(log4erl, error_logger_handler, fun() -> ok end),
    meck:expect(log4erl, log, fun(info, _, [[]]) -> ok end),
    meck:new(game_sup),
    meck:expect(game_sup, start_link, fun([]) -> ok end),
    game_app:start(),
    meck:unload([game_sup, error_logger, log4erl]).

stop_test() ->
    ?assertEqual(ok, game_app:stop(void)).
