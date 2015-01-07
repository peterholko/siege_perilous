% Author: Peter
% %% Created: Nov 11, 2014
% %% Description: Player module
-module(player).

-include("schema.hrl").
-include("common.hrl").

-export([init_perception/1, 
         get_info/1,
         move_obj/2,
         attack_obj/2]).

init_perception(PlayerId) ->

    %Get armies
    Armies = get_armies(PlayerId),

    %Get explored tile list
    ExploredTiles = map:get_explored(PlayerId),
    lager:info("ExploredTiles: ~p", [ExploredTiles]),

    %Get visible map (should be in explored unless first login)
    VisibleTiles = get_visible_map(Armies, []),
    lager:info("VisibleTiles: ~p", [VisibleTiles]),

    %Get visible objs
    Objs = get_visible_objs(Armies, []),

    AllExplored = util:unique_list(ExploredTiles ++ VisibleTiles),

    lager:info("AllExplored: ~p", [AllExplored]),
    lager:info("Objs: ~p", [Objs]),

    [{<<"player">>, PlayerId}, {<<"explored">>, AllExplored}, {<<"objs">>, Objs}].

get_info(Id) ->
    %Must have { } tuple around Id, mongo convention
    Cursor = mongo:find(mdb:get_conn(), <<"obj">>, {'_id', {Id}}),
    [Obj] = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Obj.

move_obj(Id, Pos1D) ->

    Player = get(player_id),
    NumTicks = 8,
    NewPos = map:convert_coords(Pos1D), 

    %Get Obj
    Obj = map:get_obj(Id),

    %Validate obj state
    ValidState = is_valid_state(Obj#map_obj.state),

    %Validate player owned obj
    ValidPlayer=  is_player_owned(Obj#map_obj.player, Player),
    
    %Validate position
    ValidPos = map:is_valid_pos(NewPos),

    Result = ValidState and ValidPlayer and ValidPos,

    add_move(Result, {Obj, NewPos}, NumTicks).

add_move(false, _EventData, _Ticks) ->
    lager:info("Move failed"),
    none;
add_move(true, {Obj, NewPos}, NumTicks) ->
    %Update obj state
    map:update_obj_state(Obj, moving),

    %Create event data 
    EventData = {Obj#map_obj.player,
                 Obj#map_obj.id,
                 NewPos},

    game:add_event(self(), move_obj, EventData, NumTicks).

attack_obj(SourceId, TargetId) ->

    Player = get(player_id),
    NumTicks = 8,

    %Get objs
    SourceObj = map:get_obj(SourceId),
    TargetObj = map:get_obj(TargetId),

    %TODO add validation
    Result = true,

    add_attack(Result, {SourceObj, TargetObj}, NumTicks).
  
add_attack(false, _EventData, _Ticks) ->
    lager:info("Attack failed"),
    none;
add_attack(true, {SourceObj, TargetObj}, NumTicks) ->
    %Update obj state
    map:update_obj_state(SourceObj, combat),
    map:update_obj_state(TargetObj, combat),

    EventData = {SourceObj,
                 TargetObj},

    game:add_event(self(), attack_obj, EventData, NumTicks).

get_armies(PlayerId) ->
    db:index_read(map_obj, PlayerId, #map_obj.player).

get_visible_map([], VisibleMap) ->
    VisibleMap;

get_visible_map([Obj | Rest], VisibleMap) ->
    lager:info("Obj: ~p", [Obj]),

    {X, Y} = Obj#map_obj.pos,

    [CurrentTile] = map:get_tile(X, Y),
    Type = CurrentTile#tile.type,
    Pos1D = map:convert_coords({X, Y}),

    NeighbourTileIds = map:get_neighbours(X, Y),
    Neighbours = map:get_tiles(NeighbourTileIds),

    NewVisibleMap = Neighbours ++ VisibleMap ++ [{Pos1D, Type}],

    get_visible_map(Rest, NewVisibleMap).

get_visible_objs([], Objs) ->
    Objs;

get_visible_objs([Obj | Rest], Objs) ->

    {X, Y} = Obj#map_obj.pos,

    NearbyObjs = map:get_nearby_objs(X, Y),
    NewObjs = NearbyObjs ++ Objs,

    get_visible_objs(Rest, NewObjs).

is_valid_state(none) ->
    true;
is_valid_state(_State) ->
    false.

is_player_owned(ObjPlayer, Player) ->
    ObjPlayer == Player.
    
