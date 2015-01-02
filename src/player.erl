% Author: Peter
% %% Created: Nov 11, 2014
% %% Description: Player module
-module(player).

-include("schema.hrl").
-include("common.hrl").

-export([init_perception/1, 
         get_info/1,
         move_obj/2]).

init_perception(PlayerId) ->

    %Get armies
    Armies = get_armies(PlayerId),

    %Get explored tile list
    ExploredTiles = map:get_explored(PlayerId),
    lager:info("ExploredTiles: ~p", [ExploredTiles]),

    %Get visible map (should be in explored unless first login)
    VisibleTiles = get_visible_map(Armies, []),
    lager:info("VisibleTiles: ~p", [VisibleTiles]),

    %Get visible objs
    Objs = get_visible_objs(Armies, []),

    AllExplored = util:unique_list(ExploredTiles ++ VisibleTiles),

    lager:info("AllExplored: ~p", [AllExplored]),
    lager:info("Objs: ~p", [Objs]),

    [{<<"player">>, PlayerId}, {<<"explored">>, AllExplored}, {<<"objs">>, Objs}].

get_info(Id) ->
    %Must have { } tuple around Id, mongo convention
    Cursor = mongo:find(mdb:get_conn(), <<"army">>, {'_id', {Id}}),
    [Obj] = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Obj.

move_obj(Id, Pos1D) ->

    Player = get(player_id),
    NumTicks = 8,
    Pos = map:convert_coords(Pos1D), 
    Result = map:is_valid_pos(Pos),
    add_move(Result, {Player, Id, Pos}, NumTicks).

add_move(false, _EventData, _Ticks) ->
    lager:info("Invalid position"),
    none;
add_move(true, EventData, NumTicks) ->
    game:add_event(self(), move_obj, EventData, NumTicks).

get_armies(PlayerId) ->

    Cursor = mongo:find(mdb:get_conn(), <<"army">>, {player, PlayerId}),
    Armies = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),

    Armies.

get_visible_map([], VisibleMap) ->
    VisibleMap;

get_visible_map([Obj | Rest], VisibleMap) ->
    lager:info("Obj: ~p", [Obj]),

    {X} = bson:lookup(x, Obj),
    {Y} = bson:lookup(y, Obj),

    [CurrentTile] = map:get_tile(X, Y),
    Type = CurrentTile#tile.type,
    Pos1D = map:convert_coords({X, Y}),

    NeighbourTileIds = map:get_neighbours(X, Y),
    Neighbours = map:get_tiles(NeighbourTileIds),

    NewVisibleMap = Neighbours ++ VisibleMap ++ [{Pos1D, Type}],

    get_visible_map(Rest, NewVisibleMap).

get_visible_objs([], Objs) ->
    Objs;

get_visible_objs([Army | Rest], Objs) ->

    {X} = bson:lookup(x, Army),
    {Y} = bson:lookup(y, Army),

    NearbyObjs = map:get_nearby_objs(X, Y),
    NewObjs = NearbyObjs ++ Objs,

    get_visible_objs(Rest, NewObjs).
