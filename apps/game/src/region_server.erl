%%% Сервер регионов, который хранит в себе информацию обо всех объектах в регионе,
%%% а также предосталяет базовый функционал по перемещению этих объектов, по
%%% получению объектов и управлению ими в некоторой зоне видимости.
-module(region_server).
-behaviour(gen_server).
-include("../include/game.hrl").

% API
-export([start_link/1, add_item/2, remove_item/2, lookup/2, stop/1]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  objectsTable
	 }).

-spec start_link(#region{}) -> tuple().
start_link(Region) when is_record(Region, region) ->
    gen_server:start_link(?MODULE, Region, []).

stop({region_server, Pid}) ->
    gen_server:call(Pid, stop_server).

add_item(RegionItem, #state{objectsTable=ObjectsTable}) 
  when is_record(RegionItem, region_item) ->
    dets:insert(ObjectsTable, RegionItem);

add_item(RegionItem, {region_server, Pid}) when is_record(RegionItem, region_item) ->
    gen_server:call(Pid, {add_item, RegionItem}).

remove_item(ItemId, #state{objectsTable=ObjectsTable}) -> 
    dets:delete(ObjectsTable, ItemId);

remove_item(ItemId, {region_server, Pid}) ->
    gen_server:call(Pid, {remove_item, ItemId}).

lookup(ItemId, #state{objectsTable=ObjectsTable}) ->
    Result = dets:lookup(ObjectsTable, ItemId),
    case Result of
	[#region_item{}=RegionItem] ->
	    RegionItem;
	_Other ->
	    undefined
    end;

lookup(ItemId, {region_server, Pid}) ->
    gen_server:call(Pid, {lookup, ItemId}).

-spec init(#region{}) -> {ok, #state{}}.
init(#region{name=RegionName,x=X,y=Y}) ->
    process_flag(trap_exit, true),
    ObjectsTable = load_from_file(RegionName),
    regions_sup:register_as([X, Y]),
    {ok, #state{objectsTable=ObjectsTable}}.

handle_call({add_item,RegionItem}, _From, State) ->
    add_item(RegionItem, State),
    {reply, ok, State};

handle_call({remove_item, ItemId}, _From, State) ->
    remove_item(ItemId, State),
    {reply, ok, State};

handle_call({lookup, ItemId}, _From, State) ->
    Reply = lookup(ItemId, State),
    {reply, Reply, State};

handle_call(stop_server, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_command, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{objectsTable=ObjectsTable}) ->
    dets:close(ObjectsTable).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
load_from_file(RegionName) ->
    {ok, loaded} = open_dets(RegionName),
    RegionName.

open_dets(Name) ->
    FilePath = filename:join([?DataPath, "regions", string:concat(Name, ".dets")]),
    Opts = [
	    {file, FilePath},
	    {keypos, 2},
	    {auto_save, 10000}
	   ],
    case dets:open_file(Name, Opts) of
	{ok, Name} -> ?Log("Loading data for ~p from ~p", [Name, FilePath]),
			 {ok, loaded};
	{error, Reason} ->
	    ?Log("Region ~p cannot be loaded: ~p", [Name, Reason]),
	    {error, Reason}
    end.
