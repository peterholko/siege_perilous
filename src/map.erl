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
-export([load_global/0, load_local/0, get_tile/1, get_tile/2, get_explored/1, get_nearby_objs/1, get_tiles/1,
         get_nearby_objs/2]).
-export([add_explored/2, is_valid_pos/1]).
-export([neighbours/4, distance/2]).
-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, map}, map, [], []).

load_global() ->
    {ok, File} = file:open(?MAP_FILE, [read]),
    load_map(File, 0, global).

load_local() ->
    {ok, File} = file:open(?BATTLE_MAP_FILE, [read]),
    load_map(File, 0, {local, 1}).

get_tile({X, Y}) ->
    get_tile(X, Y).

get_tile(X, Y) ->
    gen_server:call({global, map}, {get_tile, {X,Y}}).

get_explored(PlayerId) ->
    gen_server:call({global, map}, {get_explored, PlayerId}).

get_tiles(TileIds) ->
    gen_server:call({global, map}, {get_tiles, TileIds}).

get_nearby_objs({X, Y}) ->
    get_nearby_objs(X, Y).
get_nearby_objs(X, Y) ->
    gen_server:call({global, map}, {get_nearby_objs, {X,Y}}).    

add_explored(Player, {X, Y}) ->
    gen_server:cast({global, map}, {add_explored, Player, {X, Y}}).

is_valid_pos({X, Y}) ->
    is_valid_coord({X, Y}, {?MAP_WIDTH, ?MAP_HEIGHT}).

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

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({get_explored, PlayerId}, _From, Data) ->
    ExploredMap = db:read(explored_map, PlayerId),
    ExploredTiles = explored_map(ExploredMap), 

    {reply, ExploredTiles, Data};

handle_call({get_tile, TileIndex}, _From, Data) ->
    Tile = db:dirty_read(map, TileIndex),
    {reply, Tile, Data};

handle_call({get_tiles, TileIds}, _From, Data) ->
    Tiles = tiles_msg_format(TileIds, []),
    {reply, Tiles, Data};

handle_call({get_nearby_objs, {X,Y}}, _From, Data) ->

    Objects = nearby_objs({X,Y}),

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
explored_map([]) ->
    [];

explored_map([ExploredMap]) ->
    TileIds = ExploredMap#explored_map.tiles,
    Tiles = tiles_msg_format(TileIds, []),
    Tiles.

new_explored_tiles([], NewExploredTiles, _Pos) ->
    NewExploredTiles;
new_explored_tiles([ExploredMap], NewExploredTiles, CurrPos) ->
    lager:info("ExploredMap: ~p", [ExploredMap]),
    util:unique_list(ExploredMap#explored_map.tiles ++ NewExploredTiles ++ CurrPos).
    
tiles_msg_format([], Tiles) ->
    Tiles;

tiles_msg_format([TileId | Rest], Tiles) ->
    [Map] = db:dirty_read(map, TileId),
    {X, Y} = TileId,
    NewTiles = [#{<<"x">> => X, 
                  <<"y">> => Y,
                  <<"t">> => Map#global_map.tile} | Tiles],

    tiles_msg_format(Rest, NewTiles).

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

nearby_objs(SourcePos) ->
    T = fun() ->
            F = fun(MapObj, Objs) ->
                    
                    Dist = distance(SourcePos, MapObj#obj.pos),
                    check_distance(Dist, 2, MapObj, Objs)
                end,

            mnesia:foldl(F, [], obj)
        end,

    {atomic, Result} = mnesia:transaction(T),
    Result.

check_distance(Distance, Range, MapObj, Objs) when Distance =< Range ->
    {X, Y} = MapObj#obj.pos,
    [ #{<<"id">> => MapObj#obj.id, 
        <<"player">> => MapObj#obj.player, 
        <<"x">> => X,
        <<"y">> => Y,
        <<"type">> => MapObj#obj.type,
        <<"state">> => MapObj#obj.state} | Objs];

check_distance(Distance, Range, _MapObj, Objs) when Distance > Range ->
    Objs.
                              
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
        global ->
            Tile = #global_map {pos = {ColNum, RowNum},
                                tile = list_to_integer(TileType)},

            db:dirty_write(Tile);
        {local, LocalType} ->
            LocalMap = #local_map {type = LocalType,
                                   pos = {ColNum, RowNum},
                                   tile = list_to_integer(TileType)},
            db:dirty_write(LocalMap)
    end,
            
    store_tile(Rest, ColNum + 1, RowNum, MapType).

