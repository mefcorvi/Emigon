%%% Сервер регионов, который хранит в себе информацию обо всех объектах в регионе,
%%% а также предосталяет базовый функционал по перемещению этих объектов, по
%%% получению объектов и управлению ими в некоторой зоне видимости.
-module(region_server).
-behaviour(gen_server).
-include("../include/game.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TableName, ?MODULE).
-record(state, {}).

-spec start_link(integer()) -> no_return().
start_link(RegionId) when is_integer(RegionId) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, RegionId, []).

-spec init(integer()) -> {ok, #state{}}.
init(RegionId) ->
    {ok, loaded} = open_dets(RegionId),
    {ok, loaded} = load_data(),
    {ok, #state{}}.

open_dets(RegionId) ->
    Opts = [
	    {file, filename:join([?DataPath, "regions", integer_to_list(RegionId), "region.dets"])},
	    {keypos, 2}
	   ],
    ?Log(Opts),
    case dets:open_file(?TableName, Opts) of
	{ok, ?TableName} -> ?Log("Region ~p loaded", [RegionId]),
			 {ok, loaded};
	{error, _Reason} ->
	    ?Log("Region ~p cannot be loaded: ~p", [RegionId, _Reason]),
	    {error, _Reason}
    end.

load_data() ->
    dets:traverse(?TableName, fun(X) -> load_item(X) end),
    {ok, loaded}.

load_item(X) ->
    ?Log(X).

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
