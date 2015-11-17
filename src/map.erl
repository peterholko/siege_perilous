%% -------------------------------------------------------------------
%% Author  : Peter
%%% Description : Map module to access tile data
%%%
%%% Created : Dec 2, 2014
%%% -------------------------------------------------------------------
-module(map).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([load_map/0, get_tile/1, get_tile/2, get_explored/2, get_nearby_objs/3]).
-export([get_nearby_objs/4, tileset/0]).
-export([add_explored/2, is_valid_pos/1]).
-export([neighbours/4, distance/2, cube_to_odd_q/1, odd_q_to_cube/1, is_adjacent/2]).
-export([movement_cost/1]).

-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, map}, map, [], []).

get_tile({X, Y}) ->
    get_tile(X, Y).

get_tile(X, Y) ->
    gen_server:call({global, map}, {get_tile, {X,Y}}).

get_explored(PlayerId, All) ->
    gen_server:call({global, map}, {get_explored, PlayerId, All}).

get_nearby_objs({X, Y}, MapType, Dist) ->
    get_nearby_objs(X, Y, MapType, Dist).
get_nearby_objs(X, Y, MapType, Dist) ->
    gen_server:call({global, map}, {get_nearby_objs, {X,Y}, MapType, Dist}).    

add_explored(Player, {X, Y}) ->
    gen_server:cast({global, map}, {add_explored, Player, {X, Y}}).

is_valid_pos({X, Y}) ->
    is_valid_coord({X, Y}, {?MAP_WIDTH, ?MAP_HEIGHT}).

is_adjacent(SourcePos, TargetPos) ->
    {SX, SY} = SourcePos,
    Neighbours = map:neighbours(SX, SY, 10, 10),
    lists:member(TargetPos, Neighbours).

movement_cost(Pos) ->
    [Tile] = db:dirty_read(map, {1, Pos}),
    TileType = Tile#map.tile - 1,
    MoveCost = case TileType of
                   ?PLAINS -> ?PLAINS_MC;
                   ?MOUNTAINS -> ?MOUNTAINS_MC;
                   ?HILLS -> ?HILLS_MC;
                   ?FOREST -> ?FOREST_MC
               end,
    MoveCost.   

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data{},
    {ok, Data}.

handle_cast(none, Data) ->  
    {noreply, Data};

handle_cast({add_explored, Player, {X, Y}}, Data) ->
    lager:debug("add_explored: ~p ~p", [X, Y]),
    ExploredMap = db:read(explored_map, Player),
    
    ExploredTiles = get_explored_tiles(ExploredMap),
    Neighbours = neighbours(X, Y, ?MAP_WIDTH, ?MAP_HEIGHT),
    NeighboursTwo = neighbours_two(Neighbours, []),
    lager:debug("NeighboursTwo: ~p", [NeighboursTwo]),
    LatestTiles = NeighboursTwo ++ [{X, Y}],

    %Convert lists to sets for intersect and unique list processing
    SetExploredTiles = sets:from_list(ExploredTiles),
    SetLatestTiles = sets:from_list(LatestTiles),

    SetNewTiles = sets:subtract(SetLatestTiles, SetExploredTiles),
    SetNewExploredTiles = sets:union(SetExploredTiles, SetLatestTiles),

    NewExploredMap = #explored_map {player = Player, 
                                    tiles = sets:to_list(SetNewExploredTiles),
                                    new_tiles = sets:to_list(SetNewTiles)},
    db:write(NewExploredMap),

    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({get_explored, PlayerId, All}, _From, Data) ->
    ExploredMap = db:read(explored_map, PlayerId),
    Exploredtiles = explored_map(ExploredMap, All),

    {reply, Exploredtiles, Data};

handle_call({get_tile, TileIndex}, _From, Data) ->
    Tile = db:dirty_read(global_map, TileIndex),
    {reply, Tile, Data};

handle_call({get_nearby_objs, {X,Y}, MapType, Dist}, _From, Data) ->
    Objects = nearby_objs({X,Y}, MapType, Dist),
    {reply, Objects, Data};

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Event}, 
                              {from, From}
                             ]),
    {noreply, Data}.

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

terminate(_Reason, _) ->
    ok.

%% --------------------------------------------------------------------
%%% Internal functions
explored_map([], _) ->
    [];
explored_map([ExploredMap], all) ->
    TileIds = ExploredMap#explored_map.tiles,
    Tiles = tiles_msg_format(TileIds, []),
    Tiles;
explored_map([ExploredMap], new) ->
    TileIds = ExploredMap#explored_map.new_tiles,
    Tiles = tiles_msg_format(TileIds, []),
    Tiles.

get_explored_tiles([]) ->
    [];
get_explored_tiles([ExploredMap]) ->
    ExploredMap#explored_map.tiles.

tiles_msg_format([], Tiles) ->
    Tiles;
tiles_msg_format([TileId | Rest], Tiles) ->
    [Map] = db:dirty_read(map, TileId),
    {X, Y} = TileId,
    NewTiles = [#{<<"x">> => X,
                  <<"y">> => Y,
                  <<"t">> => Map#map.layers} | Tiles],

    tiles_msg_format(Rest, NewTiles).

neighbours_two([], List) ->
    List;
neighbours_two([{X, Y} | Rest], List) ->
    NewList = neighbours(X, Y, ?MAP_WIDTH, ?MAP_HEIGHT) ++ List,
    neighbours_two(Rest, NewList).

%From Amit's article on hex grid: http://www.redblobgames.com/grids/hexagons/#neighbors
neighbours(Q, R, W, H) ->

    CubeCoords = odd_q_to_cube({Q,R}),
    ConversionTable = conversion_table(),

    find_neighbours(ConversionTable, CubeCoords, [], {W, H}).

conversion_table() ->
    [{1,-1,0}, {1,0,-1}, {0,1,-1}, {-1,1,0}, {-1,0,1}, {0,-1,1}].

find_neighbours([], _CubeCoords, Neighbours, _Dimensions) ->
    Neighbours;

find_neighbours([Conversion | Rest], CubeCoords, Neighbours, Dimensions) ->

    %Use Cube coords as it is easier to find neighbours
    {X, Y, Z} = CubeCoords,
    {OffsetX, OffsetY, OffsetZ} = Conversion,
    
    %Add offsets to find neighbours and convert to back to OddQ
    NeighbourCube = {X + OffsetX, Y + OffsetY, Z + OffsetZ},
    NeighbourOddQ = cube_to_odd_q(NeighbourCube),
   
    %Check if neighbour is within map
    ValidCoord = is_valid_coord(NeighbourOddQ, Dimensions),
    NewNeighbours = add_neighbour(ValidCoord, NeighbourOddQ, Neighbours),

    find_neighbours(Rest, CubeCoords, NewNeighbours, Dimensions).

add_neighbour(true, NeighbourOddQ, Neighbours) ->
    [NeighbourOddQ | Neighbours];
add_neighbour(false, _NeighbourOddQ, Neighbours) ->
    Neighbours.

nearby_objs(SourcePos, map, LOSDist) ->
    T = fun() ->
            AllObjs = ets:tab2list(map),

            F = fun(MapObj, NearbyObjs) ->
                    build_nearby_list(SourcePos, MapObj, NearbyObjs, LOSDist)
                end,

            lists:foldl(F, [], AllObjs)
        end,
    {atomic, Result} = mnesia:transaction(T),
    Result.

build_nearby_list(SourcePos, MapObj, Objs, LOSDist) ->
    check_distance(distance(SourcePos, MapObj#obj.pos), LOSDist, MapObj, Objs).

check_distance(Distance, Range, MapObj, Objs) when Distance =< Range ->
    build_message(MapObj, Objs);
check_distance(Distance, Range, _MapObj, Objs) when Distance > Range ->
    Objs.

build_message(MapObj, Objs) ->
    {X, Y} = MapObj#obj.pos,
    [ #{<<"id">> => MapObj#obj.id, 
        <<"player">> => MapObj#obj.player, 
        <<"x">> => X,
        <<"y">> => Y,
        <<"class">> => MapObj#obj.class,
        <<"type">> => MapObj#obj.name, %TODO fix client to accept name instead of type
        <<"state">> => MapObj#obj.state,
        <<"effect">> => MapObj#obj.effect} | Objs].

distance(SourcePos, TargetPos) ->
    SourceCube = odd_q_to_cube(SourcePos),
    TargetCube = odd_q_to_cube(TargetPos),
    
    {SX, SY, SZ} = SourceCube,
    {TX, TY, TZ} = TargetCube,

    (abs(SX - TX) + abs(SY - TY) + abs(SZ - TZ)) div 2.

is_valid_coord({X, Y}, {Width, Height}) ->
    GuardX = (X >= 0) and (X < Width),
    GuardY = (Y >= 0) and (Y < Height),
    
    Result = if
                (GuardX and GuardY) ->
                    true;
                true ->
                    false
                end,
    
    Result.

cube_to_odd_q({X, _Y, Z}) ->
    Q = X,
    R = trunc(Z + (X - (X band 1)) / 2),
    {Q, R}.

odd_q_to_cube({Q, R}) ->
    X = Q,
    Z = trunc(R - (Q - (Q band 1)) / 2),
    Y = -X-Z,
    {X, Y, Z}.

load_map() ->
    lager:info("Parsing map"),
    {ok, Bin} = file:read_file("lib/sp-1/priv/test2.tmx"),
    {_T, _A, C} = parsexml:parse(Bin),
    lager:info("Processing layers"),
    process_layers(C).

tileset() ->
    lager:info("Parsing tileset"),
    {ok, Bin} = file:read_file("lib/sp-1/priv/test2.tmx"),
    {_T, _A, C} = parsexml:parse(Bin),
    TilesetList = process_tileset(C, []),
    JSON = jsx:encode(TilesetList),
    {ok, F} = file:open("tileset.json", write),
    file:write(F, JSON).

process_tileset([], TilesetList) ->
    lager:info("Done processing tileset"),
    TilesetList;
process_tileset([{<<"tileset">>, TilesetInfo, TilesetData} | Rest], TilesetList) ->
    [FirstGidInfo, NameInfo, _H, _W] = TilesetInfo,
    {_, BinFirstGid} = FirstGidInfo,
    {_, TilesetName} = NameInfo,
    FirstGid = list_to_integer(binary_to_list(BinFirstGid)),
    lager:info("FirstGid: ~p ~p", [FirstGid, TilesetName]),
    lager:info("Tileset Data: ~p", [TilesetData]),
    NewTilesetList = process_tileset_data(TilesetData, {0,0}, {FirstGid, TilesetName}, TilesetList),

    process_tileset(Rest, NewTilesetList);
process_tileset(_, Tileset) ->
    Tileset.

process_tileset_data([], _, _, NewTileset) ->
    lager:info("Done procssing tileset"),
    NewTileset;
process_tileset_data([{<<"tileoffset">>, OffsetInfo, _OffSetData} | Rest], _Offset, TilesetInfo, Tileset) ->
    lager:info("tileoffset: ~p ", [OffsetInfo]),
    [{<<"x">>, X}, {<<"y">>, Y}] = OffsetInfo,
    TileOffset = {X, Y},
    process_tileset_data(Rest, TileOffset, TilesetInfo, Tileset);
process_tileset_data([{<<"tile">>, IdInfo, TileData} | Rest], TileOffset, TilesetInfo, Tileset) ->
    lager:info("TileData ~p", [TileData]),  
    {FirstGid, TilesetName} = TilesetInfo,
    [{_, BinTileId}] = IdInfo,
    LocalTileId = binary_to_integer(BinTileId),
    TileId = FirstGid + LocalTileId,
    {X, Y} = TileOffset,
    
    Image = process_tile_data(TilesetName, TileId, TileData, none),

    NewTileset= [#{<<"tile">> => TileId,
                   <<"image">> => Image, 
                   <<"offsetx">> => X,
                   <<"offsety">> => Y} | Tileset],
    
    process_tileset_data(Rest, TileOffset, TilesetInfo, NewTileset).

process_tile_data(_TilesetName, _TileId, [], Image) ->
    Image;
process_tile_data(TilesetName, TileId, [{<<"image">>, [_Width, _Height, Source], _Empty} | Rest], Image) ->
    {_, BinFilePath} = Source,
    NewImage = BinFilePath,
    lager:info("Image: ~p", [Image]),

    process_tile_data(TilesetName, TileId, Rest, NewImage);

process_tile_data(<<"resources">>, TileId, [{<<"properties">>, _, PropertiesData} | Rest], Image) ->
    lager:info("PropertyData: ~p", [PropertiesData]), 
    Properties = process_property(PropertiesData, maps:new()),
    store_resource_def(TileId, Properties),

    process_tile_data(<<"resources">>, TileId, Rest, Image);

process_tile_data(<<"unit">>, TileId, [{<<"properties">>, _, PropertiesData} | Rest], Image) ->
    lager:info("PropertyData: ~p", [PropertiesData]), 
    _Properties = process_property(PropertiesData, maps:new()),

    process_tile_data(<<"unit">>, TileId, Rest, Image);

process_tile_data(TileLayer, TileId, [{<<"properties">>, _, PropertiesData} | Rest], Image) ->
    lager:info("PropertyData: ~p", [PropertiesData]), 
    Properties = process_property(PropertiesData, maps:new()),
    store_poi_def(TileId, Properties),

    process_tile_data(TileLayer, TileId, Rest, Image).

process_property([], Properties) ->
    lager:info("Done properties processing: ~p", [Properties]),
    Properties;

process_property([{<<"property">>, PropertyData, _} | Rest], Properties) ->
    [{<<"name">>, Name},{<<"value">>, Value}] = PropertyData,
    
    NewProperties = maps:put(Name, Value, Properties),
    process_property(Rest, NewProperties).

store_resource_def(TileId, Properties) ->
    ResourceName = maps:get(<<"name">>, Properties),
    ResourceQuantity = maps:get(<<"quantity">>, Properties),
    
    ResourceDef = #resource_def {tile = TileId,
                                 name = ResourceName,
                                 quantity = ResourceQuantity},
    db:write(ResourceDef).

store_poi_def(TileId, Properties) ->
    Name = maps:get(<<"name">>, Properties),
    
    PoiDef = #poi_def {tile = TileId,
                       name = Name},
    db:write(PoiDef).

process_layers([]) ->
    lager:info("Done processing layers");
process_layers([{<<"layer">>, [{<<"name">>,<<"base1">>} | _], LayerData} | Rest]) -> 
    process_layer_data(base,LayerData),
    process_layers(Rest);
process_layers([{<<"layer">>, [{<<"name">>,<<"base2">>} | _], LayerData} | Rest]) ->
    process_layer_data(base,LayerData),
    process_layers(Rest);
process_layers([{<<"layer">>, [{<<"name">>,<<"resource1">>} | _], LayerData} | Rest]) -> 
    process_layer_data(resource,LayerData),
    process_layers(Rest);
process_layers([{<<"layer">>, [{<<"name">>,<<"poi">>} | _], LayerData} | Rest]) -> 
    process_layer_data(poi,LayerData),
    process_layers(Rest);
process_layers([{<<"layer">>, [{<<"name">>,<<"unit">>} | _], LayerData} | Rest]) ->
    process_layer_data(unit,LayerData),
    process_layers(Rest);
process_layers([{<<"layer">>, LayerProp, LayerData} | Rest]) ->
    lager:info("Processing layer ~p", [LayerProp]),
    process_layer_data(none, LayerData),
    process_layers(Rest);
process_layers([_MapData | Rest]) ->
    process_layers(Rest).

process_layer_data(LayerType, [{<<"data">>, _Encoding, Data}]) ->
    [BinData] = Data,
    ListData = binary_to_list(BinData),
    ListSplit = string:tokens(ListData, "\n"),
    process_row(LayerType, ListSplit, 0);
process_layer_data(_, LayerData) ->
    lager:info("~p", [LayerData]).

process_row(_, [], _NumRow) ->
    lager:info("Done storing layer");
process_row(LayerType, [Row | Rest], NumRow) ->
    NewRow = string:strip(Row, right, $\r),
    ListTiles = string:tokens(NewRow, ","),
    store_tile_list(LayerType, ListTiles, NumRow, 0),
    process_row(LayerType, Rest, NumRow + 1).

store_tile_list(_, [], _NumRow, _NumCol) ->
    lager:info("Done storing tile row");
store_tile_list(LayerType, ["0" | Rest], NumRow, NumCol) ->
    do_nothing,
    store_tile_list(LayerType, Rest, NumRow, NumCol + 1);
store_tile_list(LayerType, [Tile | Rest], NumRow, NumCol) ->
    lager:info("Storing tile ~p ~p ~p ~p", [LayerType, Tile, NumRow, NumCol]),
    Pos = {NumCol, NumRow},
    store_tile(LayerType, Tile, Pos),    

    store_tile_list(LayerType, Rest, NumRow, NumCol + 1).

store_tile(base, Tile, Pos) ->
    case db:dirty_read(map, Pos) of
        [] ->
            NewTile = #map {index = Pos,
                            tile = list_to_integer(Tile),
                            layers = [list_to_integer(Tile)]},
            db:dirty_write(NewTile);
        [LocalTile] ->
            NewLayers = [list_to_integer(Tile) | LocalTile#map.layers],
            NewTile = LocalTile#map { tile = list_to_integer(Tile),
                                      layers = NewLayers},
            db:dirty_write(NewTile)
    end;
store_tile(resource, Tile, Pos) ->
    lager:info("Tile: ~p", [Tile]),
    [ResourceDef] = db:dirty_read(resource_def, list_to_integer(Tile)),
    Quantity = resource:quantity(ResourceDef#resource_def.quantity),

    Resource = #resource {index = Pos,
                          name = ResourceDef#resource_def.name,
                          max = Quantity,
                          quantity = Quantity},

    db:dirty_write(Resource);
store_tile(poi, Tile, Pos) ->
    [PoiDef] = db:dirty_read(poi_def, list_to_integer(Tile)),
    local:create(Pos, -1, poi, <<"poi">>, PoiDef#poi_def.name, none);
store_tile(none, Tile, Pos) ->
    case db:dirty_read(map, Pos) of
        [] ->
            NewTile = #map {index = Pos,
                            layers = [list_to_integer(Tile)]},
            db:dirty_write(NewTile);
        [LocalTile] ->
            NewLayers = [list_to_integer(Tile) | LocalTile#map.layers],
            NewTile = LocalTile#map { layers = NewLayers},
            db:dirty_write(NewTile)
    end.

