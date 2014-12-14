% Author: Peter
% %% Created: Nov 11, 2014
% %% Description: Player module
-module(player).

-export([get_perception/1, 
         get_info/1,
         move_obj/2]).

get_perception(PlayerId) ->

    %Get armies
    Armies = get_armies(PlayerId),

    %Get explored tiles
    TileIds = get_explored_tiles(Armies, []),
    lager:info("TileIds: ~p", [TileIds]),
    Tiles = map:get_explored_map(TileIds),

    %Get visible objs
    Objs = get_visible_objs(Armies, []),
    lager:info("Objs: ~p", [Objs]),

    [{<<"tiles">>, Tiles}, {<<"objs">>, Objs}].

get_info(Id) ->
    %Must have { } tuple around Id, mongo convention
    Cursor = mongo:find(mdb:get_conn(), <<"army">>, {'_id', {Id}}),
    [Obj] = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Obj.

move_obj(Id, Pos1D) ->

    %TODO: Add validation

    NumTicks = 8,
    Pos = map:convert_coords(Pos1D), 
    game:add_event(self(), move_obj, {Id, Pos}, NumTicks).

get_armies(PlayerId) ->

    Cursor = mongo:find(mdb:get_conn(), <<"army">>, {player, PlayerId}),
    Armies = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),

    Armies.

get_explored_tiles([], ExploredTiles) ->
    ExploredTiles;

get_explored_tiles([Army | Rest], ExploredTiles) ->

    lager:info("Army: ~p", [Army]),

    {X} = bson:lookup(x, Army),
    {Y} = bson:lookup(y, Army),

    Neighbours = map:get_neighbours(X, Y),

    NewExploredTiles = Neighbours ++ ExploredTiles,

    get_explored_tiles(Rest, NewExploredTiles).

get_visible_objs([], Objs) ->
    Objs;

get_visible_objs([Army | Rest], Objs) ->

    {X} = bson:lookup(x, Army),
    {Y} = bson:lookup(y, Army),

    NearbyObjs = map:get_nearby_objs(X, Y),
    NewObjs = NearbyObjs ++ Objs,

    get_visible_objs(Rest, NewObjs).

    

