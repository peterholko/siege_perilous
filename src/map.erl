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
-export([load/0, get_tile/2, get_explored_map/1, get_neighbours/2,
         get_nearby_objs/2]).
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

get_explored_map(TilesList) ->
    gen_server:call({global, map}, {get_explored_map, TilesList}).

get_neighbours(X, Y) ->
    neighbours(X,Y).   

get_nearby_objs(X, Y) ->
    gen_server:call({global, map}, {get_nearby_objs, {X,Y}}).    


    

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #module_data{},
    {ok, Data}.

handle_cast(none, Data) ->  
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call(load, _From, Data) ->
    lager:info("Loading map..."),
    App = util:get_app(?MODULE),
    Priv = code:priv_dir(App),

    case file:open(Priv ++ "/tiles.bin", read) of
        {ok, TilesFileRef} ->
            load_tiles(TilesFileRef, false, 0);
        {error, Reason} ->
            lager:info("Failed to open tiles.bin - ~w", [Reason]);
        _ ->
            lager:info("System limit reached")
    end,   

    {reply, ok, Data};

handle_call({get_explored_map, TileIndexList}, _From, Data) ->
    MapTiles = get_map_tiles(TileIndexList, []),
    {reply, MapTiles, Data};

handle_call({get_tile, TileIndex}, _From, Data) ->
    Tile = db:dirty_read(tile, TileIndex),
    {reply, Tile, Data};

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

load_tiles(_FileRef, true, _TileIndex) ->
    lager:info("Loading tiles completed."),
    done;

load_tiles(FileRef, false, TileIndex) ->
    EOF = case file:read(FileRef, 1) of
            {ok, Data} ->
                [TileType] = Data,
                Tile = #tile {pos = TileIndex, type = TileType},
                db:dirty_write(Tile),
                false;
            eof ->
                true
        end,
    load_tiles(FileRef, EOF, TileIndex + 1).
            
get_map_tiles([], MapList) ->
    MapList;

get_map_tiles(TileIndexList, MapList) ->
    [TileIndex | Rest] = TileIndexList,
    lager:info("TileIndex: ~p", [TileIndex]),
    [Tile] = db:dirty_read(tile, TileIndex),
    lager:info("Tile: ~p", [Tile]),
    NewMapList = [{convert_coords(TileIndex), Tile#tile.type} | MapList],

    get_map_tiles(Rest, NewMapList).

cube_to_odd_q({X, _Y, Z}) ->
    Q = X,
    R = trunc(Z + (X - (X band 1)) / 2),
    {Q, R}.

odd_q_to_cube({Q, R}) ->
    X = Q,
    Z = trunc(R - (Q - (Q band 1)) / 2),
    Y = -X-Z,
    {X, Y, Z}.

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
    [{id, Id}, 
     {player, MapObj#map_obj.player}, 
     {pos, Coords} | Objs];
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


