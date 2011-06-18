-module(session).
-export([start/1]).
-include("../include/game.hrl").

start(Ws) ->
    spawn_link(fun() -> init(Ws) end).

init(Ws) ->
    LoginInfo = wait_for_login(Ws),
    Ws:send_term(["Finding region server", LoginInfo]),
    loop(Ws).

wait_for_login(Ws) ->
    receive
	{login, Login, Pwd} ->
	    LoginInfo = user_server:login(Login, Pwd),
	    case LoginInfo of
		{ok, User} ->
		    Ws:send_term({ok, user_authorized}),
		    LoginInfo;
		{error, Reason} -> Ws:send_term({error, Reason}),
				   wait_for_login(Ws)
	    end;
	_ ->
	    Ws:send_term({error, user_not_authorized}),
	    wait_for_login(Ws)
    end.  

loop(Ws) ->
    receive
	{move, X, Y} ->
	    Ws:send_term({moved, X, Y}),
	    loop(Ws);
	{die} ->
	    ok;
	{msg, Data} ->
	    Ws:send_term(Data),
	    loop(Ws);
	{create_new, Login, Pwd} ->
	    Ws:send_term(user_server:create_new(Login, Pwd)),
	    loop(Ws);
	_Ignore ->
	    loop(Ws)
    end.
