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
-export([get_tile/1, get_tile/2, get_explored/2, get_nearby_objs/2]).
-export([get_nearby_objs/3, get_ford_pos/2]).
-export([add_explored/3]).
-export([neighbours/1, neighbours/2, get_random_neighbour/1]).
-export([cube_to_odd_q/1, odd_q_to_cube/1, is_adjacent/2]).
-export([movement_cost/1, is_passable/1, is_not_blocked/2, is_river/1, random_location/0, random_location_from/3]).
-export([check_distance/4, distance/2]).
-export([range/2, ring/2, filter_pos/1]).
-export([spawn_resources/0, tile_name/1, defense_bonus/1]).
-export([num_tiles/1]).

-record(data, {}).
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

get_nearby_objs({X, Y}, Dist) ->
    get_nearby_objs(X, Y, Dist).
get_nearby_objs(X, Y, Dist) ->
    gen_server:call({global, map}, {get_nearby_objs, {X,Y}, Dist}).    

add_explored(Player, Pos, Range) ->
    gen_server:cast({global, map}, {add_explored, Player, Pos, Range}).

is_adjacent(SourceObj, TargetObj) when (SourceObj =:= invalid) or (TargetObj =:= invalid) ->
    false;
is_adjacent(SourceObj, TargetObj) when is_record(SourceObj, obj) and 
                                       is_record(TargetObj, obj) ->
    is_adjacent(SourceObj#obj.pos, TargetObj#obj.pos);
is_adjacent(SourcePos, TargetPos) ->
    {SX, SY} = SourcePos,
    Neighbours = map:neighbours(SX, SY),
    lists:member(TargetPos, Neighbours).

is_river(Pos) ->
    [Tile] = db:dirty_read(map, Pos),
    TileName = tile_name(Tile#map.tile),
    TileName =:= ?RIVER.

movement_cost(Pos) when is_tuple(Pos) ->
    [Tile] = db:dirty_read(map, Pos),
    TileType = Tile#map.tile,
    TileName = tile_name(TileType),
    MoveCost = mc(TileName),
    MoveCost;
movement_cost(TileName) when is_binary(TileName) ->
    MoveCost = mc(TileName),
    MoveCost.

is_passable(Pos) when is_tuple(Pos) ->
    [Tile] = db:dirty_read(map, Pos),
    TileType = Tile#map.tile,
    TileName = tile_name(TileType),
    passable_tile(TileName);
is_passable(TileName) when is_binary(TileName) ->
    passable_tile(TileName).

is_not_blocked(SourcePlayer, Pos) ->
    Objs = obj:get_by_pos(Pos),

    F = fun(Obj) -> obj:is_blocking(SourcePlayer, Obj) end,

    lists:filter(F, Objs) =:= [].

defense_bonus(Pos) when is_tuple(Pos) ->
    [Tile] = db:dirty_read(map, Pos),
    TileType = Tile#map.tile,
    TileName = tile_name(TileType),
    def_bonus(TileName);
defense_bonus(TileName) when is_binary(TileName) -> 
    def_bonus(TileName).

random_location() ->
    random_location(false, {0, 0}).

random_location(true, Pos) ->
    Pos;
random_location(false, _Pos) ->
    X = util:rand(?MAP_WIDTH - 1),
    Y = util:rand(?MAP_HEIGHT - 1),
    Pos = {X, Y},

    Result = is_passable(Pos) andalso obj:is_empty(Pos),

    random_location(Result, Pos).

random_location_from(Player, Center, Radius) ->
    ListOfPos = ring(Center, Radius),

    F = fun(Pos) -> 
            is_valid_coord(Pos) andalso 
            is_passable(Pos) andalso 
            is_not_blocked(Player, Pos)
        end,

    RandomListOfPos = lists:filter(F, ListOfPos),

    %If no elements in the list return the original center pos
    case RandomListOfPos =:= [] of
        true -> 
            Center;
        false -> 
            RandomIndex = util:rand(length(ListOfPos)),
            RandomPos = lists:nth(RandomIndex, ListOfPos),
            RandomPos
    end.

get_random_neighbour(none) ->
    lager:info("get_random_neighbour none");
get_random_neighbour(Pos) ->
    Neighbours = neighbours(Pos),

    F = fun(NeighbourPos) ->
            obj:is_empty(NeighbourPos) and is_passable(NeighbourPos)
        end,

    ValidNeighbours = lists:filter(F, Neighbours),
    NumNeighbours = length(ValidNeighbours),

    case NumNeighbours > 0 of
        true ->
            Rand = util:rand(NumNeighbours),
            lists:nth(Rand, ValidNeighbours);
        false ->
            none
    end.

get_ford_pos(Pos, RiverPos) ->
    Neighbours = sets:from_list([Pos | neighbours(Pos)]),
    RiverNeighbours = sets:from_list(neighbours(RiverPos)),

    FordSet = sets:subtract(RiverNeighbours, Neighbours),
    
    F = fun(FordPos) ->
            obj:is_empty(FordPos) and is_passable(FordPos)
        end,

    FordList = lists:filter(F, sets:to_list(FordSet)),
    NumFord = length(FordList),

    case NumFord > 0 of
        true -> 
            Rand = util:rand(NumFord),
            lists:nth(Rand, FordList);
        false ->
            none
    end.    

num_tiles(TerrainType) ->
    case TerrainType of
        hills ->
           Hill1 = mnesia:dirty_match_object({map, '_', 7, '_'}), 
           Hill2 = mnesia:dirty_match_object({map, '_', 8, '_'}), 
           Hill3 = mnesia:dirty_match_object({map, '_', 12, '_'}), 
           Hill4 = mnesia:dirty_match_object({map, '_', 13, '_'}), 
           Hill5 = mnesia:dirty_match_object({map, '_', 16, '_'}),
           length(Hill1) + length(Hill2) + length(Hill3) + length(Hill4) + length(Hill5);
        _ ->
            0
    end.

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #data{},

    tileset(),
    load(),

    {ok, Data}.

handle_cast(none, Data) ->  
    {noreply, Data};

handle_cast({add_explored, Player, Pos, Range}, Data) ->

    %Previous Stored Explored tiles
    PrevTiles = get_explored_tiles(Player),

    %Current Explored tiles
    CurrTiles = range(Pos, Range),

    %Convert lists to sets for intersect and unique list processing
    SetPrevTiles = sets:from_list(PrevTiles),
    SetCurrTiles = sets:from_list(CurrTiles),

    SetNewTiles = sets:subtract(SetCurrTiles, SetPrevTiles),
    SetAllTiles = sets:union(SetCurrTiles, SetPrevTiles),

    NewExploredMap = #explored_map {player = Player, 
                                    tiles = sets:to_list(SetAllTiles),
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
    Tile = db:dirty_read(map, TileIndex),
    {reply, Tile, Data};

handle_call({get_nearby_objs, {X,Y}, Dist}, _From, Data) ->
    Objects = nearby_objs({X,Y}, Dist),
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

get_explored_tiles(Player) ->
    case db:read(explored_map, Player) of
        [] -> [];
        [ExploredMap] -> ExploredMap#explored_map.tiles
    end.

tiles_msg_format([], Tiles) ->
    Tiles;
tiles_msg_format([TileId | Rest], Tiles) ->
    [Map] = db:dirty_read(map, TileId),
    {X, Y} = TileId,
    NewTiles = [#{<<"x">> => X,
                  <<"y">> => Y,
                  <<"t">> => Map#map.layers} | Tiles],

    tiles_msg_format(Rest, NewTiles).

%From Amit's article on hex grid: http://www.redblobgames.com/grids/hexagons/#neighbors
neighbours({X, Y}) ->
    neighbours(X, Y).

neighbours(Q, R) ->

    CubeCoords = odd_q_to_cube({Q,R}),
    ConversionTable = conversion_table(),

    find_neighbours(ConversionTable, CubeCoords, []).

conversion_table() ->
    [{1,-1,0}, {1,0,-1}, {0,1,-1}, {-1,1,0}, {-1,0,1}, {0,-1,1}].

find_neighbours([], _CubeCoords, Neighbours) ->
    Neighbours;

find_neighbours([Conversion | Rest], CubeCoords, Neighbours) ->

    %Use Cube coords as it is easier to find neighbours
    {X, Y, Z} = CubeCoords,
    {OffsetX, OffsetY, OffsetZ} = Conversion,
    
    %Add offsets to find neighbours and convert to back to OddQ
    NeighbourCube = {X + OffsetX, Y + OffsetY, Z + OffsetZ},
    NeighbourOddQ = cube_to_odd_q(NeighbourCube),
   
    %Check if neighbour is within map
    ValidCoord = is_valid_coord(NeighbourOddQ),
    NewNeighbours = add_neighbour(ValidCoord, NeighbourOddQ, Neighbours),

    find_neighbours(Rest, CubeCoords, NewNeighbours).

add_neighbour(true, NeighbourOddQ, Neighbours) ->
    [NeighbourOddQ | Neighbours];
add_neighbour(false, _NeighbourOddQ, Neighbours) ->
    Neighbours.

direction(N) -> lists:nth(N, conversion_table()).

scale({X, Y, Z}, N) -> {X * N, Y * N, Z * N}.

add({X1, Y1, Z1}, {X2, Y2, Z2}) -> {X1 + X2, Y1 + Y2, Z1 + Z2}.

neighbour(Pos, DirN) -> add(Pos, direction(DirN)).

range(Pos, N) ->
    {CX, CY, CZ} = odd_q_to_cube(Pos),
    S = lists:seq(-1 * N, N), 
    ListOfPos = [cube_to_odd_q({CX + X, CY + Y, CZ + Z}) || X <- S, Y <- S, Z <- S, X + Y + Z  == 0],
    filter_pos(ListOfPos).

ring(Center, Radius) ->
    CubeCenter = odd_q_to_cube(Center),

    CubeDir4 = direction(4),
    CubeDir4Scale = scale(CubeDir4, Radius),

    Cube = add(CubeCenter, CubeDir4Scale),

    F = fun(I, IAcc) ->
            G = fun(_J, JAcc) ->
                    [Last | _] = JAcc,
                    [ neighbour(Last, I) | JAcc]
                end,

            lists:foldl(G, IAcc, lists:seq(1, Radius))
        end,

    ListOfCube = lists:foldl(F, [Cube], lists:seq(1, 6)),

    H = fun(CubePos) -> cube_to_odd_q(CubePos) end, 
    filter_pos(lists:map(H, ListOfCube)).

filter_pos(ListOfPos) ->
    F = fun(Pos) ->
            is_valid_coord(Pos)
        end,
    lists:filter(F, ListOfPos).

nearby_objs(SourcePos, LOSDist) ->
    T = fun() ->
            AllObjs = ets:tab2list(obj),

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
        <<"subclass">> => MapObj#obj.subclass,
        <<"type">> => MapObj#obj.name, %TODO fix client to accept name instead of type
        <<"vision">> => MapObj#obj.vision,
        <<"state">> => MapObj#obj.state} | Objs].

distance(SourcePos, TargetPos) ->
    SourceCube = odd_q_to_cube(SourcePos),
    TargetCube = odd_q_to_cube(TargetPos),
    
    {SX, SY, SZ} = SourceCube,
    {TX, TY, TZ} = TargetCube,

    (abs(SX - TX) + abs(SY - TY) + abs(SZ - TZ)) div 2.

is_valid_coord({X, Y}) ->
    is_valid_coord(X, Y, ?MAP_WIDTH, ?MAP_HEIGHT).

is_valid_coord(X, Y, Width, Height) when (X >= 0) and (X < Width) and 
                                         (Y >= 0) and (Y < Height) ->
    true;
is_valid_coord(_, _, _, _) ->
    false.

cube_to_odd_q({X, _Y, Z}) ->
    Q = X,
    R = trunc(Z + (X - (X band 1)) / 2),
    {Q, R}.

odd_q_to_cube({Q, R}) ->
    X = Q,
    Z = trunc(R - (Q - (Q band 1)) / 2),
    Y = -X-Z,
    {X, Y, Z}.

tileset() ->
    lager:debug("Parsing tileset"),
    {ok, Bin} = file:read_file(code:lib_dir(sp) ++ "/priv/static/test3.tmx"),
    {_T, _A, C} = parsexml:parse(Bin),
    TilesetList = process_tileset(C, []),
    JSON = jsx:encode(TilesetList),
    {ok, F} = file:open("tileset.json", write),
    file:write(F, JSON).

load() ->
    lager:debug("Parsing map"),
    {ok, Bin} = file:read_file(code:lib_dir(sp) ++ "/priv/static/test3.tmx"),
    {_T, A, C} = parsexml:parse(Bin),
    lager:debug("Processing map properties..."),
    process_map_properties(A),

    lager:debug("Processing layers..."),
    process_layers(C).

process_map_properties(MapProperties) ->
    {<<"width">>, WidthBin} = lists:keyfind(<<"width">>, 1, MapProperties),
    {<<"height">>, HeightBin} = lists:keyfind(<<"height">>, 1, MapProperties),
    
    Width = binary_to_integer(WidthBin),
    Height = binary_to_integer(HeightBin),

    put(map_width, Width),
    put(map_height, Height).

process_tileset([], TilesetList) ->
    lager:debug("Done processing tileset"),
    TilesetList;
process_tileset([{<<"tileset">>, TilesetInfo, TilesetData} | Rest], TilesetList) ->
    [FirstGidInfo, NameInfo, _H, _W, _TileCount] = TilesetInfo,
    {_, BinFirstGid} = FirstGidInfo,
    {_, TilesetName} = NameInfo,
    FirstGid = list_to_integer(binary_to_list(BinFirstGid)),
    lager:debug("FirstGid: ~p ~p", [FirstGid, TilesetName]),
    lager:debug("Tileset Data: ~p", [TilesetData]),
    NewTilesetList = process_tileset_data(TilesetData, {0,0}, {FirstGid, TilesetName}, TilesetList),

    process_tileset(Rest, NewTilesetList);
process_tileset(_, Tileset) ->
    Tileset.

process_tileset_data([], _, _, NewTileset) ->
    lager:debug("Done procssing tileset"),
    NewTileset;
process_tileset_data([{<<"tileoffset">>, OffsetInfo, _OffSetData} | Rest], _Offset, TilesetInfo, Tileset) ->
    lager:debug("tileoffset: ~p ", [OffsetInfo]),
    [{<<"x">>, X}, {<<"y">>, Y}] = OffsetInfo,
    TileOffset = {X, Y},
    process_tileset_data(Rest, TileOffset, TilesetInfo, Tileset);
process_tileset_data([{<<"tile">>, IdInfo, TileData} | Rest], TileOffset, TilesetInfo, Tileset) ->
    lager:debug("TileData ~p", [TileData]),  
    {FirstGid, TilesetName} = TilesetInfo,
    [{_, BinTileId}] = IdInfo,
    LocalTileId = binary_to_integer(BinTileId),
    TileId = FirstGid + LocalTileId,
    {X, Y} = TileOffset,
    
    Image = process_tile_data(TilesetName, TileId, TileData, none),
    lager:debug("TileId: ~p Image: ~p", [TileId, Image]),

    NewTileset= [#{<<"tile">> => TileId,
                   <<"image">> => Image, 
                   <<"offsetx">> => X,
                   <<"offsety">> => Y} | Tileset],
    
    process_tileset_data(Rest, TileOffset, TilesetInfo, NewTileset).

process_tile_data(_TilesetName, _TileId, [], Image) ->
    Image;
process_tile_data(TilesetName, TileId, [{<<"image">>, [_Width, _Height, Source], _Empty} | Rest], _Image) ->
    {_, BinFilePath} = Source,
    NewImage = BinFilePath,

    process_tile_data(TilesetName, TileId, Rest, NewImage);

process_tile_data(<<"resources">>, TileId, [{<<"properties">>, _, PropertiesData} | Rest], Image) ->
    lager:debug("PropertyData: ~p", [PropertiesData]), 
    Properties = process_property(PropertiesData, maps:new()),
    store_resource_def(TileId, Properties),

    process_tile_data(<<"resources">>, TileId, Rest, Image);

process_tile_data(<<"unit">>, TileId, [{<<"properties">>, _, PropertiesData} | Rest], Image) ->
    lager:debug("PropertyData: ~p", [PropertiesData]), 
    _Properties = process_property(PropertiesData, maps:new()),

    process_tile_data(<<"unit">>, TileId, Rest, Image);

process_tile_data(TileLayer, TileId, [{<<"properties">>, _, PropertiesData} | Rest], Image) ->
    lager:debug("PropertyData: ~p", [PropertiesData]), 
    Properties = process_property(PropertiesData, maps:new()),
    store_poi_def(TileId, Properties),

    process_tile_data(TileLayer, TileId, Rest, Image).

process_property([], Properties) ->
    lager:debug("Done properties processing: ~p", [Properties]),
    Properties;

process_property([{<<"property">>, PropertyData, _} | Rest], Properties) ->
    [{<<"name">>, Name},{<<"value">>, Value}] = PropertyData,
    
    NewProperties = maps:put(Name, Value, Properties),
    process_property(Rest, NewProperties).

store_resource_def(_TileId, _Properties) ->
    %ResourceName = maps:get(<<"name">>, Properties),
    %ResourceQuantity = maps:get(<<"quantity">>, Properties),
    
    %ResourceDef = #resource_def {tile = TileId,
    %                             name = ResourceName,
    %                             quantity = ResourceQuantity},
    %db:write(ResourceDef),
    none.

store_poi_def(TileId, Properties) ->
    Name = maps:get(<<"name">>, Properties),
    
    PoiDef = #poi_def {tile = TileId,
                       name = Name},
    db:write(PoiDef).

process_layers([]) ->
    lager:debug("Done processing layers");
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
    lager:debug("Processing layer ~p", [LayerProp]),
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
    lager:debug("~p", [LayerData]).

process_row(_, [], _NumRow) ->
    lager:debug("Done storing layer");
process_row(LayerType, [Row | Rest], NumRow) ->
    NewRow = string:strip(Row, right, $\r),
    ListTiles = string:tokens(NewRow, ","),
    store_tile_list(LayerType, ListTiles, NumRow, 0),
    process_row(LayerType, Rest, NumRow + 1).

store_tile_list(_, [], _NumRow, _NumCol) ->
    lager:debug("Done storing tile row");
store_tile_list(LayerType, ["0" | Rest], NumRow, NumCol) ->
    do_nothing,
    store_tile_list(LayerType, Rest, NumRow, NumCol + 1);
store_tile_list(LayerType, [Tile | Rest], NumRow, NumCol) ->
    lager:debug("Storing tile ~p ~p ~p ~p", [LayerType, Tile, NumRow, NumCol]),
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
store_tile(resource, Tile, _Pos) ->
    lager:debug("Tile: ~p", [Tile]),
    %[ResourceDef] = db:dirty_read(resource_def, list_to_integer(Tile)),
    %Quantity = resource:quantity(ResourceDef#resource_def.quantity),

    %Resource = #resource {index = Pos,
    %                      name = ResourceDef#resource_def.name,
    %                      max = Quantity,
    %                      quantity = Quantity},

    %db:dirty_write(Resource);
    none;
store_tile(poi, Tile, Pos) ->
    [PoiDef] = db:dirty_read(poi_def, list_to_integer(Tile)),
    Subclass = get_poi_subclass(PoiDef#poi_def.name),
    obj:create(Pos, -1, poi, Subclass, PoiDef#poi_def.name, none);
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

get_poi_subclass(<<"Monolith">>) -> ?MONOLITH;
get_poi_subclass(_) -> <<"poi">>.

spawn_resources() ->
    Tiles = ets:tab2list(map),

    F = fun(Tile) ->
            Rand = util:rand(100), 
            TileName = tile_name(Tile#map.tile),
            spawn_resource(TileName, Tile, Rand)
        end,

    lists:foreach(F, Tiles).

spawn_resource(?GRASSLANDS, Tile, Rand) when Rand =< 25 ->
    Quantity = util:rand(20) + 5,
    resource:create(<<"Crimson Root">>, Quantity, Tile#map.index, false);
spawn_resource(?PLAINS, Tile, Rand) when Rand =< 15 ->
    Quantity = util:rand(10) + 5,
    resource:create(<<"Crimson Root">>, Quantity, Tile#map.index, false);
spawn_resource(?HILLS_PLAINS, Tile, Rand) when Rand =< 99 ->
    {Resource, Rarity} = hill_resource(90, 8, 2),
    Quantity = quantity(Rarity, {5, 20}, {3, 10}, {0, 3}),
    resource:create(Resource, Quantity, Tile#map.index, false);
spawn_resource(?HILLS_GRASSLANDS, Tile, Rand) when Rand =< 99 ->
    {Resource, Rarity} = hill_resource(85, 13, 2),
    Quantity = quantity(Rarity, {5, 20}, {3, 10}, {0, 3}),
    resource:create(Resource, Quantity, Tile#map.index, false);
spawn_resource(?HILLS_SNOW, Tile, Rand) when Rand =< 99 ->
    {Resource, Rarity} = hill_resource(0, 0, 100),
    Quantity = quantity(Rarity, {5, 10}, {10, 20}, {10, 10}),
    resource:create(Resource, Quantity, Tile#map.index, false);
spawn_resource(?HILLS_DESERT, Tile, Rand) when Rand =< 99 ->
    {Resource, Rarity} = hill_resource(0, 100, 0),
    Quantity = quantity(Rarity, {5, 10}, {10, 20}, {10, 10}),
    resource:create(Resource, Quantity, Tile#map.index, false);
spawn_resource(?DECIDUOUS_FOREST, Tile, Rand) when Rand =< 75 ->
    {Resource, Rarity} = forest_resource(90, 8, 2),
    Quantity = quantity(Rarity, {5, 20}, {3, 10}, {0, 5}),
    resource:create(Resource, Quantity, Tile#map.index, false);
spawn_resource(?PINE_FOREST, Tile, Rand) when Rand =< 20 ->
    {Resource, Rarity} = forest_resource(75, 20, 5),
    Quantity = quantity(Rarity, {5, 20}, {3, 10}, {0, 5}),
    resource:create(Resource, Quantity, Tile#map.index, false);
spawn_resource(?FROZEN_FOREST, Tile, Rand) when Rand =< 20 ->
    {Resource, Rarity} = forest_resource(0, 67, 33),
    Quantity = quantity(Rarity, {0, 0}, {5, 10}, {5, 5}),
    resource:create(Resource, Quantity, Tile#map.index, false);
spawn_resource(_, _, _) -> nothing.

forest_resource(Low, Med, High) ->
    case util:rand(100) of 
        Num when Num =< Low -> {<<"Cragroot Popular">>, common};
        Num when Num =< (Low + Med) -> {<<"Wrapwood Birch">>, uncommon};
        Num when Num =< (Low + Med + High) -> {<<"Skyshroud Oak">>, rare};
        Num -> lager:info("Num: ~p L: ~p M: ~p H: ~p", [Num, Low, Med, High])
    end.
    

%generate_resources(TileName, Pos, Resources) ->
    
%    F = fun(Resource) ->
%            ResourceMap = resource_def:all_to_map(Resource),
%            Terrain = maps:get(<<"terrain">>, ResourceMap),
            
%            case lists:member(TileName, Terrain) of
%                true ->
%                    QuantityRate = maps:get(<<"quantity_rate">>, ResourceMap),
%                    Quantity = maps:get(<<"quantity">>, ResourceMap),
%                    Rand = util:rand(100),



hill_resource(Low, Med, High) ->
    case util:rand(100) of
        Num when Num =< Low -> {<<"Valleyrun Copper Ore">>, common};
        Num when Num =< (Low + Med) -> {<<"Quickforge Iron Ore">>, uncommon};
        Num when Num =< (Low + Med + High) -> {<<"Stronghold Mithril Ore">>, rare};
        Num -> lager:info("Num: ~p L: ~p M: ~p H: ~p", [Num, Low, Med, High])
    end.

quantity(Rarity, {CBase, CRange}, {UBase, URange}, {RBase, RRange}) ->
    case Rarity of
        common -> util:rand(CRange) + CBase;
        uncommon -> util:rand(URange) + UBase;
        rare -> util:rand(RRange) + RBase
    end.

tile_name(1) -> ?GRASSLANDS;
tile_name(2) -> ?SNOW;
tile_name(3) -> ?RIVER;
tile_name(4) -> ?RIVER;
tile_name(5) -> ?OCEAN;
tile_name(6) -> ?PLAINS;
tile_name(7) -> ?HILLS_PLAINS;
tile_name(8) -> ?HILLS_PLAINS;
tile_name(9) -> ?PLAINS;
tile_name(10) -> ?DESERT;
tile_name(11) -> ?OASIS;
tile_name(12) -> ?HILLS_DESERT;
tile_name(13) -> ?HILLS_GRASSLANDS;
tile_name(14) -> ?SWAMP;
tile_name(15) -> ?SWAMP;
tile_name(16) -> ?HILLS_SNOW;
tile_name(17) -> ?OCEAN;
tile_name(18) -> ?SWAMP;
tile_name(19) -> ?DECIDUOUS_FOREST;
tile_name(20) -> ?RAINFOREST;
tile_name(21) -> ?JUNGLE;
tile_name(22) -> ?SAVANNA;
tile_name(23) -> ?DECIDUOUS_FOREST;
tile_name(24) -> ?DECIDUOUS_FOREST;
tile_name(25) -> ?FROZEN_FOREST;
tile_name(26) -> ?FROZEN_FOREST;
tile_name(27) -> ?PINE_FOREST;
tile_name(28) -> ?FROZEN_FOREST;
tile_name(29) -> ?SAVANNA;
tile_name(30) -> ?PALM_FOREST;
tile_name(31) -> ?JUNGLE;
tile_name(32) -> ?MOUNTAIN;
tile_name(33) -> ?MOUNTAIN;
tile_name(34) -> ?MOUNTAIN;
tile_name(35) -> ?MOUNTAIN;
tile_name(36) -> ?MOUNTAIN;
tile_name(37) -> ?MOUNTAIN;
tile_name(38) -> ?MOUNTAIN;
tile_name(39) -> ?VOLCANO.

mc(?MOUNTAIN) -> 5;
mc(?HILLS_PLAINS) -> 3;
mc(?HILLS_GRASSLANDS) -> 3;
mc(?HILLS_SNOW) -> 3;
mc(?HILLS_DESERT) -> 3;
mc(?DECIDUOUS_FOREST) -> 3;
mc(?RIVER) -> 6;
mc(_) -> 1.

passable_tile(?OCEAN) -> false;
passable_tile(?RIVER) -> false;
passable_tile(?MOUNTAIN) -> false;
passable_tile(_) -> true.

def_bonus(?HILLS_PLAINS) -> 0.33;
def_bonus(?HILLS_GRASSLANDS) -> 0.33;
def_bonus(?HILLS_SNOW) -> 0.33;
def_bonus(?HILLS_DESERT) -> 0.33;
def_bonus(?DECIDUOUS_FOREST) -> 0.5;
def_bonus(?PINE_FOREST) -> 0.5;
def_bonus(?FROZEN_FOREST) -> 0.5;
def_bonus(?JUNGLE) -> 0.75;
def_bonus(?SWAMP) -> 0.66;
def_bonus(_) -> 0.
