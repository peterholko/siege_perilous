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
-export([load/0, get_tile/2, get_explored_map/1, get_neighbours/2]).
-export([convert_coords/1, convert_coords/2]).
-record(module_data, {}).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, map}, map, [], []).

load() ->
    gen_server:call({global, map}, load).

get_tile(X, Y) ->
    TileIndex = convert_coords(X,Y),
    gen_server:call({global, map}, {get_tile, TileIndex}).

get_explored_map(TileIndexList) ->
    gen_server:call({global, map}, {get_explored_map, TileIndexList}).

get_neighbours(X, Y) ->
    Tiles2D = surrounding_tiles_2D(X, Y, 1),
    TileList = surrounding_tiles(Tiles2D),
    TileList.

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
                Tile = #tile {index = TileIndex, type = TileType},
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

    if
        TileIndex >= 0 ->
            [Tile] = db:dirty_read(tile, TileIndex),
            NewMapList = [{integer_to_binary(TileIndex), Tile#tile.type} | MapList];
        true ->
            NewMapList = MapList
    end,

    get_map_tiles(Rest, NewMapList).

convert_coords(X, Y) ->
    Y * ?MAP_HEIGHT + X.

convert_coords(TileIndex) ->
    TileX = TileIndex rem ?MAP_WIDTH,
    TileY = TileIndex div ?MAP_HEIGHT,
    {TileX , TileY}.

is_valid_coords(X, Y) ->
    GuardX = (X >= 0) and (X < ?MAP_WIDTH),
    GuardY = (Y >= 0) and (Y < ?MAP_HEIGHT),
    
    if
        (GuardX and GuardY) ->
            Result = true;
        true ->
            Result = false
    end,
    
    Result.

surrounding_tiles_2D(X, Y, ViewRange) ->
    MinX = X - ViewRange,
    MinY = Y - ViewRange,
    MaxX = X + ViewRange + 1,
    MaxY = Y + ViewRange + 1,
    tiles_y_2D(MinX, MinY, MaxX, MaxY, []).

tiles_y_2D(_, MaxY, _, MaxY, Tiles) ->
    Tiles;

tiles_y_2D(X, Y, MaxX, MaxY, Tiles) ->
    NewTiles = tiles_x_2D(X, Y, MaxX, MaxY, Tiles),
    tiles_y_2D(X, Y + 1, MaxX, MaxY, NewTiles).

tiles_x_2D(MaxX, _, MaxX, _, Tiles) ->
    Tiles;

tiles_x_2D(X, Y, MaxX, MaxY, Tiles) ->
    Tile = {X, Y},
    NewTiles = [Tile | Tiles],
    tiles_x_2D(X + 1, Y, MaxX, MaxY, NewTiles).

%% TODO: Combine with above tiles x,y looping
surrounding_tiles(Tiles2D) ->
    
    F = fun(Tile2D, Tiles) ->
                
                {X, Y} = Tile2D,
                ValidTile = is_valid_coords(X, Y),
                
                if
                    ValidTile ->
                        Tile = convert_coords(X, Y),
                        NewTiles = [Tile | Tiles];
                    true ->
                        NewTiles = Tiles
                end,
                
                NewTiles
        end,
    
    lists:foldl(F, [], Tiles2D).

     
