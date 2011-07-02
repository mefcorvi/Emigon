%%% Сервер регионов, который хранит в себе информацию обо всех объектах в регионе,
%%% а также предосталяет базовый функционал по перемещению этих объектов, по
%%% получению объектов и управлению ими в некоторой зоне видимости.
-module(region_server).
-behaviour(gen_server).
-include("../include/game.hrl").

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

-spec start_link(integer(), string(), list()) -> no_return().
start_link(RegionId, Name, Coords) when is_integer(RegionId) ->
    gen_server:start_link(?MODULE, {region, RegionId, Name, Coords}, []).

-spec init({region, integer(), string()}) -> {ok, #state{}}.
init({region, RegionId, Name, Coords}) ->
    {ok, loaded} = open_dets(Name),
    {ok, loaded} = load_data(Name),
    gproc:add_local_name({region, Coords}),
    ?Log("Region server started and registered as ~p", [{region, RegionId}]),
    {ok, #state{}}.

open_dets(Name) ->
    FilePath = filename:join([?DataPath, "regions", string:concat(Name, ".dets")]),
    Opts = [
	    {file, FilePath},
	    {keypos, 2}
	   ],
    case dets:open_file(Name, Opts) of
	{ok, Name} -> ?Log("Loading data for ~p from ~p", [Name, FilePath]),
			 {ok, loaded};
	{error, _Reason} ->
	    ?Log("Region ~p cannot be loaded: ~p", [Name, _Reason]),
	    {error, _Reason}
    end.

load_data(Name) ->
    dets:traverse(Name, fun(X) -> load_item(X) end),
    ?Log("Data for ~p loaded", [Name]),
    {ok, loaded}.

load_item(X) ->
    ?Log("Item").

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
