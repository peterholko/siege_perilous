% Author: Peter
% %% Created: Nov 11, 2014
% %% Description: Player module
-module(player).

-export([get_perception/1]).

get_perception(PlayerId) ->

    %Get armies
    Armies = get_armies(PlayerId),

    %Get explored tiles
    TileIds = get_explored_tiles(Armies, []),
    Tiles = map:get_explored_map(TileIds),

    %Get visible objs
    Objs = get_visible_objs(Armies, []),

    [{<<"tiles">>, Tiles}, {<<"objs">>, Objs}].

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

    

