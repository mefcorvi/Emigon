-module(game_util_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

parse_term_test() ->
    ?assertEqual({ok, {simple}}, game_util:parse_term("{  simple  }.")),
    ?assertEqual({ok, {simple, [1,2,3]}}, game_util:parse_term("{simple, [1,2,3]}.")),
    ?assertEqual({ok, {simple, [{some}, {some2, [1,2,3]}]}}, game_util:parse_term("{simple, [{some}, {some2, [1,2,3]}]}.")),
    ?assertEqual({wrong_term, "{ simple ]"}, game_util:parse_term("{ simple ]")),
    ?assertEqual({wrong_term, [15678]}, game_util:parse_term([15678])).

term_to_string_test() ->
    ?assertEqual("{simple}", game_util:term_to_string({simple})),
    ?assertEqual("{simple,[1,2,3]}", game_util:term_to_string({simple, [1,2,3]})),
    ?assertEqual("{simple,[some,[1,2,3]]}", game_util:term_to_string({simple, [some, [1,2,3]]})).
