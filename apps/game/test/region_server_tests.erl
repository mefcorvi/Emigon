-module(region_server_tests).
-compile(export_all).
-include("../include/game.hrl").
-include_lib("eunit/include/eunit.hrl").

region_server_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{with, [T]} || T <- [
			   fun save_and_lookup/1,
			   fun save_remove_and_lookup/1,
			   fun lookup_undefined/1,
			   fun call_unknown_command/1,
			   fun cast_unknown_command/1,
			   fun send_info_message/1
			  ]]}.

setup() ->
    ?debugMsg("Setup"),
    meck:new(regions_sup),
    meck:expect(regions_sup, register_as, fun([X, Y]) when X =:= 0, Y =:= 0 -> ok end),
    code:unstick_mod(dets),
    meck:new(dets, [passthrough]),
    meck:expect(dets, open_file, fun(Name, _) ->
					 NewArgs = [Name, [
							   {file, os:cmd("mktemp")},
							   {keypos, 2}
							  ]],
					 meck:passthrough(NewArgs)
				 end),

    {ok, Pid} = region_server:start_link(#region{name="region_test",x=0,y=0}),
    {region_server, Pid}.
    
teardown(Region) ->
    Region:stop(),
    meck:unload([dets, regions_sup]),
    ?debugMsg("Teardown"),
    ok.

save_and_lookup(Region) ->
    ItemToAdd = #region_item{type=permanent,id=1,x=0,y=0},
    Region:add_item(ItemToAdd),
    RegionItem = Region:lookup(1),
    ?assertEqual(ItemToAdd, RegionItem).

save_remove_and_lookup(Region) ->
    ItemToAdd = #region_item{type=permanent,id=1,x=0,y=0},
    Region:add_item(ItemToAdd),
    ?assertEqual(ok, Region:remove_item(1)),
    ?assertEqual(undefined, Region:lookup(1)).

lookup_undefined(Region) ->
    ?assertEqual(undefined, Region:lookup(123)).

call_unknown_command({region_server, Pid}) ->
    ?assertEqual(unknown_command, gen_server:call(Pid, {some_unknown_command})).

cast_unknown_command({region_server, Pid}) ->
    gen_server:cast(Pid, {some_unknown_command}).

send_info_message({region_server, Pid}) ->
    Pid ! message.

reload_module(_) ->
    code:purge(region_server),
    code:load_file(region_server).
