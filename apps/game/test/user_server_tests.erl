-module(user_server_tests).
-compile(export_all).
-include("../include/game.hrl").
-include_lib("eunit/include/eunit.hrl").

user_server_test_() ->
    {setup, fun global_setup/0, fun global_teardown/1,
     [{foreach, fun setup/0, fun teardown/1,
       [{with, [T]} || T <- [
			     fun create_new/1,
			     fun create_new_and_login/1,
			     fun login_unregistered/1,
			     fun create_new_and_clear/1,
			     fun call_unknown_command/1,
			     fun cast_unknown_command/1,
			     fun send_info_message/1
			    ]]}
     ]}.

setup() ->
    {ok, Pid} = user_server:start_link(#user_server_options{dataPath=os:cmd("mktemp")}),
    {user_server, Pid}.

teardown(_) ->
    user_server:stop().

global_setup() ->
    ok.

global_teardown(_) ->
    ok.

create_new(_) ->
    ?assertEqual({ok, registered}, user_server:create_new("Login", "Pwd")),
    ?assertEqual({error, already_registered}, user_server:create_new("Login", "")).

create_new_and_login(_)->
    ?assertEqual({ok, registered}, user_server:create_new("Login", "Pwd")),
    ?assertEqual({ok, #user{password="Pwd",login="Login"}}, user_server:login("Login", "Pwd")).

login_unregistered(_) ->
    ?assertEqual({error, not_registered}, user_server:login("Login", "Pwd")).

create_new_and_clear(_) ->
    ?assertEqual({ok, registered}, user_server:create_new("Login", "Pwd")),
    ?assertEqual(ok, user_server:clear_all()),
    ?assertEqual({error, not_registered}, user_server:login("Login", "Pwd")).

call_unknown_command({user_server, Pid}) ->
    ?assertEqual({unknown_command, {some_unknown_command}}, gen_server:call(Pid, {some_unknown_command})).

cast_unknown_command({user_server, Pid}) ->
    gen_server:cast(Pid, {some_unknown_command}).

send_info_message({user_server, Pid}) ->
    Pid ! message.

exception_when_dets_not_loaded_test() ->
    code:unstick_mod(dets),
    meck:new(dets),
    meck:expect(dets, open_file, fun(_, _) -> {error, something_wrong} end),
    Error = {error, {bad_return_value, {error, something_wrong}}},
    ?assertEqual(Error, user_server:start_link(#user_server_options{dataPath=os:cmd("mktemp")})),
    meck:unload(dets),
    ok.
