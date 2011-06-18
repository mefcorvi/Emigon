-module(socket_wrap).
-export([send/2, send_term/2, wrap/1]).

wrap(Ws) ->
    {socket_wrap, Ws}.

send(Data, {socket_wrap, Ws}) ->
    Ws:send(Data).

send_term(Data, {socket_wrap, Ws}) ->
    Ws:send(game_util:term_to_string(Data)).
