% Author: Peter
% %% Created: Nov 11, 2014
% %% Description: Player module
-module(player).

-include("schema.hrl").
-include("common.hrl").

-export([init_perception/1, 
         get_info_tile/1,
         get_info_obj/1,
         get_info_unit/1,
         move_obj/2,
         attack_obj/2,
         attack_unit/2,
         harvest/2,
         equip/2]).

init_perception(PlayerId) ->

    %Get armies
    Armies = get_armies(PlayerId),

    %Get explored tile list
    Explored = map:get_explored(PlayerId),
    Objs = get_visible_objs(Armies, []),

    lager:info("Explored: ~p", [Explored]),
    lager:info("Objs: ~p", [Objs]),

    {PlayerId, Explored, Objs}.

get_info_tile(Pos) ->
    %TODO check if player can see Pos
    lager:info("info_tile").

get_info_obj(Id) ->
    Player = get(player_id),
    obj:get_info(Player, Id).

get_info_unit(Id) ->
    unit:get_info(Id).

move_obj(Id, Pos1D) ->

    Player = get(player_id),
    NumTicks = 8,
    NewPos = map:convert_coords(Pos1D), 

    %Get Obj
    Obj = map:get_obj(Id),

    %Validate obj state
    ValidState = is_valid_state(Obj#map_obj.state),

    %Validate player owned obj
    ValidPlayer = is_player_owned(Obj#map_obj.player, Player),
    
    %Validate position
    ValidPos = map:is_valid_pos(NewPos),

    Result = ValidState and ValidPlayer and ValidPos,

    add_move(Result, {Obj, NewPos}, NumTicks).

attack_obj(SourceId, TargetId) ->
    Player = get(player_id),
    NumTicks = 8,

    %TODO add validation
    Result = true,

    add_attack_obj_event(Result, {SourceId, TargetId}, NumTicks).
  
attack_unit(SourceId, TargetId) ->
    battle:attack_unit(SourceId, TargetId).

harvest(Id, Resource) ->
    Player = get(player_id),
    Obj = map:get_obj(Id),
    NumTicks = 40,

    ValidState = is_valid_state(Obj#map_obj.state),
    ValidPlayer = is_player_owned(Obj#map_obj.player, Player),
    ValidResource = resource:contains(Resource, Obj#map_obj.pos),

    Result = ValidState and ValidPlayer and ValidResource,

    add_harvest_event(Result, {Id, Resource}, NumTicks).

equip(Id, ItemId) ->
    Player = get(player_id),

    [Unit] = unit:get(Id),
    [Item] = item:get(ItemId),

    lager:info("Unit: ~p Item: ~p", [Unit, Item]),

    {ObjId} = bson:lookup(obj_id, Unit),
    Obj = map:get_obj(ObjId),

    {ItemOwner} = bson:lookup(owner, Item),

    ValidItem = Id == ItemOwner,
    ValidState = is_valid_state(Obj#map_obj.state),
    ValidPlayer = is_player_owned(Obj#map_obj.player, Player),

    Result = ValidItem and ValidState and ValidPlayer,

    ReturnMsg = add_equip(Result, ItemId),
    ReturnMsg.
%
%Internal functions
%

add_harvest_event(false, _EventData, _Ticks) ->
    lager:info("Harvest failed"),
    none;
add_harvest_event(true, {ObjId, Resource}, NumTicks) ->
    %Update obj state
    map:update_obj_state(ObjId, harvesting),

    EventData = {ObjId, Resource},
    game:add_event(self(), harvest, EventData, NumTicks).

add_attack_obj_event(false, _EventData, _Ticks) ->
    lager:info("Attack failed"),
    none;
add_attack_obj_event(true, {SourceId, TargetId}, NumTicks) ->

    EventData = {SourceId, TargetId},
    game:add_event(self(), attack_obj, EventData, NumTicks).

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

add_equip(false, _EventData) ->
    lager:info("Equip failed"),
    none;
add_equip(true, ItemId) ->
    item:equip(ItemId).

get_armies(PlayerId) ->
    db:index_read(map_obj, PlayerId, #map_obj.player).

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

