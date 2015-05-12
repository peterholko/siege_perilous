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
-export([load_global/0, load_local/0, get_tile/1, get_tile/2, get_explored/1, get_local_explored/3, get_nearby_objs/3, get_tiles/1,
         get_nearby_objs/4, xml_test/0, tileset/0]).
-export([add_explored/2, add_local_explored/3, is_valid_pos/1]).
-export([neighbours/4, distance/2, cube_to_odd_q/1, odd_q_to_cube/1, is_adjacent/2]).
-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, map}, map, [], []).

load_global() ->
    {ok, File} = file:open(?MAP_FILE, [read]),
    load_map(File, 0, global_map).

load_local() ->
    {ok, File} = file:open(?BATTLE_MAP_FILE, [read]),
    load_map(File, 0, {local_map, 1}).

get_tile({X, Y}) ->
    get_tile(X, Y).

get_tile(X, Y) ->
    gen_server:call({global, map}, {get_tile, {X,Y}}).

get_explored(PlayerId) ->
    gen_server:call({global, map}, {get_explored, PlayerId}).

get_local_explored(PlayerId, GlobalPos, All) ->
    gen_server:call({global, map}, {get_local_explored, PlayerId, GlobalPos, All}).

get_tiles(TileIds) ->
    gen_server:call({global, map}, {get_tiles, TileIds}).

get_nearby_objs({X, Y}, MapType, Dist) ->
    get_nearby_objs(X, Y, MapType, Dist).
get_nearby_objs(X, Y, MapType, Dist) ->
    gen_server:call({global, map}, {get_nearby_objs, {X,Y}, MapType, Dist}).    

add_explored(Player, {X, Y}) ->
    gen_server:cast({global, map}, {add_explored, Player, {X, Y}}).

add_local_explored(Player, GlobalPos, Pos) ->
    gen_server:cast({global, map}, {add_local_explored, Player, GlobalPos, Pos}).

is_valid_pos({X, Y}) ->
    is_valid_coord({X, Y}, {?MAP_WIDTH, ?MAP_HEIGHT}).

is_adjacent(SourcePos, TargetPos) ->
    {SX, SY} = SourcePos,
    Neighbours = map:neighbours(SX, SY, 10, 10),
    lists:member(TargetPos, Neighbours).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data{},
    {ok, Data}.

handle_cast(none, Data) ->  
    {noreply, Data};

handle_cast({add_explored, Player, {X, Y}}, Data) ->
    ExploredMap = db:read(explored_map, Player),
    Neighbours = neighbours(X, Y, ?MAP_WIDTH, ?MAP_HEIGHT),
    NewTiles = new_explored_tiles(ExploredMap, Neighbours, [{X, Y}]),
    NewExploredMap = #explored_map {player = Player, tiles = NewTiles}, 
    db:write(NewExploredMap),

    {noreply, Data};

handle_cast({add_local_explored, Player, GlobalPos, {X, Y}}, Data) ->
    lager:info("add_local_explored: ~p ~p", [X, Y]),
    ExploredMap = db:read(explored_map, {Player, GlobalPos}),
    
    ExploredTiles = get_explored_tiles(ExploredMap),
    Neighbours = neighbours(X, Y, 38, 32),
    NeighboursTwo = neighbours_two(Neighbours, []),
    lager:info("NeighboursTwo: ~p", [NeighboursTwo]),
    LatestTiles = NeighboursTwo ++ [{X, Y}],

    %Convert lists to sets for intersect and unique list processing
    SetExploredTiles = sets:from_list(ExploredTiles),
    SetLatestTiles = sets:from_list(LatestTiles),

    SetNewTiles = sets:subtract(SetLatestTiles, SetExploredTiles),
    SetNewExploredTiles = sets:union(SetExploredTiles, SetLatestTiles),

    NewExploredMap = #explored_map {player = {Player, GlobalPos}, 
                                    tiles = sets:to_list(SetNewExploredTiles),
                                    new_tiles = sets:to_list(SetNewTiles)},
    db:write(NewExploredMap),

    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({get_explored, PlayerId}, _From, Data) ->
    ExploredMap = db:read(explored_map, PlayerId),
    ExploredTiles = explored_map(ExploredMap, none, all), 

    {reply, ExploredTiles, Data};

handle_call({get_local_explored, PlayerId, GlobalPos, All}, _From, Data) ->
    ExploredMap = db:read(explored_map, {PlayerId, GlobalPos}),
    Exploredtiles = explored_map(ExploredMap, GlobalPos, All),

    {reply, Exploredtiles, Data};

handle_call({get_tile, TileIndex}, _From, Data) ->
    Tile = db:dirty_read(global_map, TileIndex),
    {reply, Tile, Data};

handle_call({get_tiles, TileIds}, _From, Data) ->
    Tiles = tiles_msg_format(TileIds, [], none),
    {reply, Tiles, Data};

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
explored_map([], _, _) ->
    [];
explored_map([ExploredMap], GlobalPos, all) ->
    TileIds = ExploredMap#explored_map.tiles,
    Tiles = tiles_msg_format(TileIds, [], GlobalPos),
    Tiles;
explored_map([ExploredMap], GlobalPos, new) ->
    TileIds = ExploredMap#explored_map.new_tiles,
    Tiles = tiles_msg_format(TileIds, [], GlobalPos),
    Tiles.

new_explored_tiles([], NewExploredTiles, Pos) ->
    util:unique_list(NewExploredTiles ++ Pos);
new_explored_tiles([ExploredMap], NewExploredTiles, CurrPos) ->
    util:unique_list(ExploredMap#explored_map.tiles ++ NewExploredTiles ++ CurrPos).
    
get_explored_tiles([]) ->
    [];
get_explored_tiles([ExploredMap]) ->
    ExploredMap#explored_map.tiles.

tiles_msg_format([], Tiles, _) ->
    Tiles;

tiles_msg_format([TileId | Rest], Tiles, none) ->
    [Map] = db:dirty_read(global_map, TileId),
    {X, Y} = TileId,
    NewTiles = [#{<<"x">> => X, 
                  <<"y">> => Y,
                  <<"t">> => Map#global_map.tile} | Tiles],

    tiles_msg_format(Rest, NewTiles, none);
tiles_msg_format([TileId | Rest], Tiles, GlobalPos) ->
    %Fix GlobalPos
    [Map] = db:dirty_read(local_map, {1, TileId}),
    {X, Y} = TileId,
    NewTiles = [#{<<"x">> => X,
                  <<"y">> => Y,
                  <<"t">> => Map#local_map.misc} | Tiles],

    tiles_msg_format(Rest, NewTiles, GlobalPos).

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

nearby_objs(SourcePos, global_map, LOSDist) ->
    T = fun() ->
            F = fun(Obj, NearbyObjs) ->
                    build_nearby_list(SourcePos, Obj, NearbyObjs, LOSDist)
                end,
            mnesia:foldl(F, [], obj)
        end,

    {atomic, Result} = mnesia:transaction(T),
    Result;

nearby_objs(SourcePos, {local_map, GlobalPos}, LOSDist) ->
    T = fun() ->
            AllObjs = db:index_read(local_obj, GlobalPos, #local_obj.global_pos),

            F = fun(MapObj, NearbyObjs) ->
                    build_nearby_list(SourcePos, MapObj, NearbyObjs, LOSDist)
                end,

            lists:foldl(F, [], AllObjs)
        end,
    {atomic, Result} = mnesia:transaction(T),
    Result.

build_nearby_list(SourcePos, MapObj, Objs, LOSDist) ->
    MapObjPos = get_pos(MapObj),
    check_distance(distance(SourcePos, MapObjPos), LOSDist, MapObj, Objs).

get_pos(MapObj) when is_record(MapObj, obj) ->
    MapObj#obj.pos;
get_pos(MapObj) when is_record(MapObj, local_obj) ->
    MapObj#local_obj.pos.
get_player(MapObj) when is_record(MapObj, obj) ->
    MapObj#obj.player;
get_player(MapObj) when is_record(MapObj, local_obj) ->
    MapObj#local_obj.player.
get_type(MapObj) when is_record(MapObj, obj) ->
    MapObj#obj.type;
get_type(MapObj) when is_record(MapObj, local_obj) ->
    MapObj#local_obj.type.
get_state(MapObj) when is_record(MapObj, obj) ->
    MapObj#obj.state;
get_state(MapObj) when is_record(MapObj, local_obj) ->
    MapObj#local_obj.state.
get_id(MapObj) when is_record(MapObj, obj) ->
    MapObj#obj.id;
get_id(MapObj) when is_record(MapObj, local_obj) ->
    MapObj#local_obj.id.

check_distance(Distance, Range, MapObj, Objs) when Distance =< Range ->
    build_message(MapObj, Objs);
check_distance(Distance, Range, _MapObj, Objs) when Distance > Range ->
    Objs.

build_message(MapObj, Objs) ->
    {X, Y} = get_pos(MapObj),
    [ #{<<"id">> => get_id(MapObj), 
        <<"player">> => get_player(MapObj), 
        <<"x">> => X,
        <<"y">> => Y,
        <<"type">> => get_type(MapObj),
        <<"state">> => get_state(MapObj)} | Objs].

distance(SourcePos, TargetPos) ->
    SourceCube = odd_q_to_cube(SourcePos),
    TargetCube = odd_q_to_cube(TargetPos),
    
    {SX, SY, SZ} = SourceCube,
    {TX, TY, TZ} = TargetCube,

    abs(SX - TX) + abs(SY - TY) + abs(SZ - TZ).

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

load_map(File, RowNum, MapType) ->
    case file:read_line(File) of
        eof ->
            done;
        {error, Reason} ->
            lager:info("Error: ~p", [Reason]),
            error;
        {ok, RawLine} ->
            Line = string:strip(RawLine, right, $\n),
            TypeList = string:tokens(Line, ","),
            store_tile(TypeList, 0, RowNum, MapType),
            load_map(File, RowNum + 1, MapType)
     end.

store_tile([], _ColNum, _RowNum, _MapType) ->
    lager:info("Done storing tiles");
store_tile([TileType | Rest], ColNum, RowNum, MapType) ->
    case MapType of
        global_map ->
            Tile = #global_map {pos = {ColNum, RowNum},
                                tile = list_to_integer(TileType)},

            db:dirty_write(Tile);
        _ ->
            nothing
    end,
            
    store_tile(Rest, ColNum + 1, RowNum, MapType).

xml_test() ->
    lager:info("Parsing map"),
    {ok, Bin} = file:read_file("lib/sp-1/priv/test1.tmx"),
    {_T, _A, C} = parsexml:parse(Bin),
    lager:info("Processing layers"),
    process_layers(C).

tileset() ->
    lager:info("Parsing tileset"),
    {ok, Bin} = file:read_file("lib/sp-1/priv/test1.tmx"),
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
    NewTilesetList = process_tileset_data(TilesetData, {0,0}, FirstGid, TilesetList),

    process_tileset(Rest, NewTilesetList);
process_tileset(_, Tileset) ->
    Tileset.

process_tileset_data([], _, _, NewTileset) ->
    lager:info("Done procssing tileset"),
    NewTileset;
process_tileset_data([{<<"tileoffset">>, OffsetInfo, _OffSetData} | Rest], _Offset, FirstGid, Tileset) ->
    lager:info("tileoffset: ~p ", [OffsetInfo]),
    [{<<"x">>, X}, {<<"y">>, Y}] = OffsetInfo,
    TileOffset = {X, Y},
    process_tileset_data(Rest, TileOffset, FirstGid, Tileset);
process_tileset_data([{<<"tile">>, IdInfo, ImageInfo} | Rest], TileOffset, FirstGid, Tileset) ->
    [{_, BinTileId}] = IdInfo,
    LocalTileId = binary_to_integer(BinTileId),
    TileId = FirstGid + LocalTileId,
    Image = get_image(ImageInfo),
    lager:info("~p - ~p", [TileId, Image]),

    {X, Y} = TileOffset,

    NewTileset= [#{<<"tile">> => TileId,
                   <<"image">> => Image, 
                   <<"offsetx">> => X,
                   <<"offsety">> => Y} | Tileset],

    process_tileset_data(Rest, TileOffset, FirstGid, NewTileset).

get_image([{_, [_Width, _Height, Source], _Empty}]) ->
    {_, BinFilePath} = Source,
    %FilePath = binary_to_list(BinFilePath),
    %list_to_binary(string:sub_string(FilePath, 69)).
    BinFilePath.

process_layers([]) ->
    lager:info("Done processing layers");
process_layers([{<<"layer">>, LayerProp, LayerData} | Rest]) ->
    lager:info("Processing layer ~p", [LayerProp]),
    process_layer_data(LayerData),
    process_layers(Rest);
process_layers([_MapData | Rest]) ->
    process_layers(Rest).

process_layer_data([{<<"data">>, _Encoding, Data}]) ->
    [BinData] = Data,
    ListData = binary_to_list(BinData),
    ListSplit = string:tokens(ListData, "\n"),

    process_row(ListSplit, 0);
process_layer_data(LayerData) ->
    lager:info("~p", [LayerData]).

process_row([], _NumRow) ->
    lager:info("Done storing layer");
process_row([Row | Rest], NumRow) ->
    ListTiles = string:tokens(Row, ","),
    store_tile_list(ListTiles, NumRow, 0),
    process_row(Rest, NumRow + 1).

store_tile_list([], _NumRow, _NumCol) ->
    lager:info("Done storing tile row");
store_tile_list(["0" | Rest], NumRow, NumCol) ->
    do_nothing,
    store_tile_list(Rest, NumRow, NumCol + 1);
store_tile_list([Tile | Rest], NumRow, NumCol) ->
    
    lager:info("Storing tile ~p ~p ~p", [Tile, NumRow, NumCol]),
    Pos = {NumCol, NumRow},
    case db:dirty_read(local_map, {1, Pos}) of
        [] ->
            NewTile = #local_map {index = {1, Pos},
                                  misc = [list_to_integer(Tile)]},
            db:dirty_write(NewTile);
        [LocalTile] ->
            NewMisc = [list_to_integer(Tile) | LocalTile#local_map.misc],
            NewTile = LocalTile#local_map { misc = NewMisc},
            db:dirty_write(NewTile)
    end,

    store_tile_list(Rest, NumRow, NumCol + 1).
 
