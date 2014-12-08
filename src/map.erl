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

    [Tile] = db:dirty_read(tile, TileIndex),
    NewMapList = [{TileIndex, Tile#tile.type} | MapList],

    get_map_tiles(Rest, NewMapList).

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

%From Amit's article on hex grid: http://www.redblobgames.com/grids/hexagons/#neighbors
neighbours(Q, R) ->

    Parity = Q band 1,
    ConversionList = conversion_list(Parity),

    neighbours(ConversionList, Q, R, []).

conversion_list(0) ->
    [{1,0}, {1,-1}, {0,-1}, {-1,-1}, {-1,0}, {0,1}];
conversion_list(1) ->
     [{1,1}, {1,0}, {0,-1}, {-1,0}, {-1,1}, {0,1}].

neighbours([], _Q, _R, Neighbours) ->
    Neighbours;

neighbours([Conversion | Rest], Q, R, Neighbours) ->

    {OffsetQ, OffsetR} = Conversion,
    Neighbour = {Q + OffsetQ, R + OffsetR},
    NewNeighbours = [Neighbour | Neighbours],

    neighbours(Rest, Q, R, NewNeighbours).
