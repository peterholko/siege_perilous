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
-export([load/0, get_tile/2, get_explored/1, get_neighbours/2, get_nearby_objs/1, get_tiles/1,
         get_nearby_objs/2, move_obj/2, convert_coords/1]).
-export([add_explored/2]).
-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, map}, map, [], []).

load() ->
    gen_server:call({global, map}, load).

get_tile(X, Y) ->
    gen_server:call({global, map}, {get_tile, {X,Y}}).

get_explored(PlayerId) ->
    gen_server:call({global, map}, {get_explored, PlayerId}).

get_neighbours(X, Y) ->
    neighbours(X,Y).  

get_tiles(TileIds) ->
    gen_server:call({global, map}, {get_tiles, TileIds}).

get_nearby_objs({X, Y}) ->
    get_nearby_objs(X, Y).
get_nearby_objs(X, Y) ->
    gen_server:call({global, map}, {get_nearby_objs, {X,Y}}).    

move_obj(Id, Pos) ->
    move(Id, Pos). 

add_explored(Player, {X, Y}) ->
    gen_server:cast({global, map}, {add_explored, Player, {X, Y}}).

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
    Neighbours = neighbours(X, Y),
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
    Tile = db:dirty_read(tile, TileIndex),
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
    [Tile] = db:dirty_read(tile, TileId),
    NewTiles = [{convert_coords(TileId), Tile#tile.type} | Tiles],

    tiles_msg_format(Rest, NewTiles).

%From Amit's article on hex grid: http://www.redblobgames.com/grids/hexagons/#neighbors
neighbours(Q, R) ->

    CubeCoords = odd_q_to_cube({Q,R}),
    ConversionTable = conversion_table(),

    neighbours(ConversionTable, CubeCoords, []).

conversion_table() ->
    [{1,-1,0}, {1,0,-1}, {0,1,-1}, {-1,1,0}, {-1,0,1}, {0,-1,1}].

neighbours([], _CubeCoords, Neighbours) ->
    Neighbours;

neighbours([Conversion | Rest], CubeCoords, Neighbours) ->

    %Use Cube coords as it is easier to find neighbours
    {X, Y, Z} = CubeCoords,
    {OffsetX, OffsetY, OffsetZ} = Conversion,
    
    %Add offsets to find neighbours and convert to back to OddQ
    NeighbourCube = {X + OffsetX, Y + OffsetY, Z + OffsetZ},
    NeighbourOddQ = cube_to_odd_q(NeighbourCube),
   
    %Check if neighbour is within map
    ValidCoord = is_valid_coord(NeighbourOddQ),
    NewNeighbours = add_neighbour(ValidCoord, NeighbourOddQ, Neighbours),

    neighbours(Rest, CubeCoords, NewNeighbours).

add_neighbour(true, NeighbourOddQ, Neighbours) ->
    [NeighbourOddQ | Neighbours];
add_neighbour(false, _NeighbourOddQ, Neighbours) ->
    Neighbours.

nearby_objs(SourcePos) ->

    T = fun() ->

            F = fun(MapObj, Objs) ->
                    
                    Dist = distance(SourcePos, MapObj#map_obj.pos),
                    check_distance(Dist, 2, MapObj, Objs)
                end,

            mnesia:foldl(F, [], map_obj)
        end,

    {atomic, Result} = mnesia:transaction(T),
    Result.

check_distance(Distance, Range, MapObj, Objs) when Distance =< Range ->
    Coords = convert_coords(MapObj#map_obj.pos),
    <<Id:96>> = MapObj#map_obj.id,
    [[{id, Id}, 
      {player, MapObj#map_obj.player}, 
      {pos, Coords}] | Objs];

check_distance(Distance, Range, _MapObj, Objs) when Distance > Range ->
    Objs.
                              
distance(SourcePos, TargetPos) ->
    lager:info("Source: ~p Target: ~p", [SourcePos, TargetPos]),
    SourceCube = odd_q_to_cube(SourcePos),
    TargetCube = odd_q_to_cube(TargetPos),
    
    {SX, SY, SZ} = SourceCube,
    {TX, TY, TZ} = TargetCube,

    abs(SX - TX) + abs(SY - TY) + abs(SZ - TZ).

is_valid_coord({X, Y}) ->
    GuardX = (X >= 0) and (X < ?MAP_WIDTH),
    GuardY = (Y >= 0) and (Y < ?MAP_HEIGHT),
    
    if
        (GuardX and GuardY) ->
            Result = true;
        true ->
            Result = false
    end,
    
    Result.

convert_coords(TileIndex) when is_tuple(TileIndex) ->
    {X, Y} = TileIndex,
    Y * ?MAP_HEIGHT + X;

convert_coords(TileIndex) ->
    TileX = TileIndex rem ?MAP_WIDTH,
    TileY = TileIndex div ?MAP_HEIGHT,
    {TileX , TileY}.

move(Id, Pos) ->
    [Obj] = mnesia:dirty_read(map_obj, Id),
    NewObj = Obj#map_obj {pos = Pos},
    mnesia:dirty_write(NewObj).

cube_to_odd_q({X, _Y, Z}) ->
    Q = X,
    R = trunc(Z + (X - (X band 1)) / 2),
    {Q, R}.

odd_q_to_cube({Q, R}) ->
    X = Q,
    Z = trunc(R - (Q - (Q band 1)) / 2),
    Y = -X-Z,
    {X, Y, Z}.
