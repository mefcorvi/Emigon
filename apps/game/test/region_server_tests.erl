-module(region_server_tests).
-compile(export_all).
-include("../include/game.hrl").
-include_lib("eunit/include/eunit.hrl").

inc_0_test() ->
    gproc_app:start(),
    game_app:start(),
    Region = regions_sup:lookup([0,0]),
    Region:add_item(#region_item{type=permanent,id=1,x=0,y=0}),
    RegionItem = Region:lookup(1),
    ?assertEqual(#region_item{type=permanent,id=1,x=0,y=0}, RegionItem).
