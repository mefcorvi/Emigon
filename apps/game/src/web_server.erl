-module(web_server).
-export([start_link/0]).
-include("../include/game.hrl").

%% start misultin http server
start_link() ->
    Port = 1234,
    misultin:start_link([
			 {port, Port},
			 {compress, true},
			 {loop, fun(Req) -> handle_http(Req) end},
			 {ws_loop, fun(Ws) -> process_socket(socket_wrap:wrap(Ws)) end}
			]).

%% callback on request received
handle_http(Req) ->	
    %% output
    {abs_path, Uri} = Req:get(uri),
    handle(Req, Uri).

handle(Req, "/") ->
    handle_file(Req, "/index.html");

handle(Req, Uri) ->
    handle_file(Req, Uri).

handle_file(Req, FileName) ->
    ContentType = case filename:extension(FileName) of
		      ".js" -> "text/javascript";
		      _ -> "text/html"
		  end,
    FilePath = string:concat(?StaticPath, FileName),
    case file:read_file(FilePath) of
	{ok, Content} -> Req:ok([{"Content-Type", ContentType}], Content);
	_ -> Req:respond(404, [{"Content-Type", "text/html"}], "Page not found")
    end.
    
%%--------------------------------------------------------------------
%% @doc Запускает цикл обработки нового веб-сокета.
%% @end
%%--------------------------------------------------------------------
-spec process_socket(term()) -> ok.
process_socket(Ws) ->
    process_flag(trap_exit, true),
    Sid = start_session(Ws),
    handle_websocket(Ws, Sid).

%%--------------------------------------------------------------------
%% @doc Запускает новую сессию
%% @end
%%--------------------------------------------------------------------
-spec start_session(term()) -> node().
start_session(Ws) ->
    session:start(Ws).

%%--------------------------------------------------------------------
%% @doc Цикл обработки сообщений веб-сокета
%% @spec handle_websocket(term(), node()) -> no_return()
%% @end
%%--------------------------------------------------------------------
handle_websocket(Ws, Sid) ->
    receive
	{browser, Data} ->
	    TermString = string:concat(Data, "."),
	    case game_util:parse_term(TermString) of
		{ok, Term} ->
		    Sid ! Term;
		Error ->
		    Ws:send_term({err, Error})
	    end,
	    handle_websocket(Ws, Sid);
	{'EXIT', Sid, _Reason} -> % сессия погибла, перезапустим её
	    Ws:send_term({session_died}),
	    ?Error("Session ~p died because ~p", [Sid, _Reason]),
            handle_websocket(Ws, start_session(Ws));
	_Ignore ->
	    handle_websocket(Ws, Sid)
    end.
