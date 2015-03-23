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
         get_info_item/1,
         get_info_battle/1,
         move_obj/2,
         move_unit/2,
         attack_obj/2,
         attack_unit/2,
         harvest/2,
         explore/2,
         exit_local/0,
         build/3,
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

get_info_tile(_Pos) ->
    %TODO check if player can see Pos
    lager:info("info_tile").

get_info_obj(Id) ->
    Player = get(player_id),
    obj:get_info(Player, Id).

get_info_unit(Id) ->
    unit:get_info(Id).

get_info_item(Id) ->
    Info = item:get_info(Id),
    lager:info("Item Info: ~p", [Info]),
    Info.

get_info_battle(Id) ->
    Player = get(player_id),
    Info = case battle:check_player(Player, Id) of
                true ->
                    BattlePerception = battle:info(Id),
                    {battle_perception, BattlePerception};
                false ->
                    ObjInfo = obj:get_info(Player, Id),
                    {obj_info, ObjInfo} 
           end,
    Info.

move_obj(Id, Pos) ->
    Player = get(player_id),
    NumTicks = 8,

    %Get Obj
    Obj = obj:get_map_obj(Id),

    %Validate obj state
    ValidState = is_valid_state(Obj#obj.state),

    %Validate player owned obj
    ValidPlayer = is_player_owned(Obj#obj.player, Player),
    
    %Validate position
    ValidPos = map:is_valid_pos(Pos),

    Result = ValidState and ValidPlayer and ValidPos,

    add_move(Result, {Obj, Pos}, NumTicks).

attack_obj(SourceId, TargetId) ->
    Player = get(player_id),
    NumTicks = 8,

    %TODO add validation
    Result = true,

    add_attack_obj_event(Result, {SourceId, TargetId}, NumTicks).
  
attack_unit(SourceId, TargetId) ->
    battle:attack_unit(SourceId, TargetId).

move_unit(UnitId, Pos) ->
    Player = get(player_id),
    NumTicks = 8,
    %TODO add validation
    Result = true,
    
    [Unit] = db:read(local_obj, UnitId),
    add_move_unit(Result, {Unit#local_obj.global_pos, Player, UnitId, Pos}, NumTicks).

harvest(Id, Resource) ->
    Player = get(player_id),
    Obj = obj:get_map_obj(Id),
    NumTicks = 40,

    ValidState = is_state(none, Obj#obj.state),
    ValidPlayer = is_player_owned(Obj#obj.player, Player),
    ValidResource = resource:contains(Resource, Obj#obj.pos),

    Result = ValidState and ValidPlayer and ValidResource,

    add_harvest_event(Result, {Id, Resource}, NumTicks).

explore(_Id, _GlobalPos) ->
    %TODO add validation
    PlayerId = get(player_id),

    [Obj] = db:index_read(obj, PlayerId, #obj.player),
    lager:info("Obj: ~p", [Obj]),

    case Obj#obj.state of
        none ->
            obj:update_state(Obj, local),
            local:enter_map(PlayerId, Obj#obj.id, Obj#obj.pos, Obj#obj.last_pos);
        _ ->
            nothing
    end,

    InitPerception = local:init_perception(PlayerId, Obj#obj.pos, 1),
    InitPerception.

exit_local() ->
    %TODO add validation
    PlayerId = get(player_id),
    [Obj] = db:index_read(obj, PlayerId, #obj.player),
    lager:info("Obj: ~p", [Obj]),
    NumTicks = 16,

    ValidExit = is_state(local, Obj#obj.state) andalso
                local:is_exit_valid(Obj#obj.id),

    add_exit_local(ValidExit, {Obj#obj.id, Obj#obj.pos}, NumTicks).

build(Id, LocalPos, Structure) ->
    %TODO add validation

    GlobalPos = {1,1},
    NumTicks = 40,
    Result = true,

    add_build_event(Result, {Id, GlobalPos, LocalPos, Structure}, NumTicks).

equip(Id, ItemId) ->
    Player = get(player_id),

    [Unit] = unit:get(Id),
    [Item] = item:get(ItemId),

    lager:info("Unit: ~p Item: ~p", [Unit, Item]),

    {ObjId} = bson:lookup(obj_id, Unit),
    Obj = obj:get_map_obj(ObjId),

    {ItemOwner} = bson:lookup(owner, Item),

    ValidItem = Id == ItemOwner,
    ValidState = is_valid_state(Obj#obj.state),
    ValidPlayer = is_player_owned(Obj#obj.player, Player),

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
    obj:update_state(ObjId, harvesting),

    EventData = {ObjId, Resource},
    game:add_event(self(), harvest, EventData, NumTicks).

add_build_event(false, _EventData, _Ticks) ->
    lager:info("Build failed"),
    none;
add_build_event(true, {Id, GlobalPos, LocalPos, Structure}, NumTicks) ->
    %Begin building structure
    StructureId = structure:start_build(Id, GlobalPos, LocalPos, Structure),

    EventData = {Id, GlobalPos, LocalPos, StructureId},
    game:add_event(self(), build, EventData, NumTicks).

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
    obj:update_state(Obj, moving),

    %Create event data 
    EventData = {Obj#obj.player,
                 Obj#obj.id,
                 NewPos},

    game:add_event(self(), move_obj, EventData, NumTicks).

add_move_unit(true, {GlobalPos, Player, UnitId, NewPos}, NumTicks) ->
    %Update unit state
    local:update_state(UnitId, moving),
    
    %Create event data
    EventData = {GlobalPos,
                 Player,
                 UnitId,
                 NewPos},

    game:add_event(self(), move_local_obj, EventData, NumTicks);

add_move_unit(false, _, _) ->
    lager:info("Move unit failed"),
    none.

add_exit_local(true, {GlobalObjId, GlobalPos}, NumTicks) ->
    EventData = {GlobalObjId, GlobalPos},
    
    game:add_event(self(), exit_local, EventData, NumTicks);

add_exit_local(false, _, _) ->
    lager:info("Exit failed"),
    none.

add_equip(false, _EventData) ->
    lager:info("Equip failed"),
    none;
add_equip(true, ItemId) ->
    item:equip(ItemId).


get_armies(PlayerId) ->
    db:index_read(obj, PlayerId, #obj.player).

get_visible_objs([], Objs) ->
    Objs;

get_visible_objs([Obj | Rest], Objs) ->

    {X, Y} = Obj#obj.pos,
    NearbyObjs = map:get_nearby_objs(X, Y, global_map, 2),
    NewObjs = NearbyObjs ++ Objs,

    get_visible_objs(Rest, NewObjs).

is_valid_state(none) ->
    true;
is_valid_state(_State) ->
    false.

is_player_owned(ObjPlayer, Player) ->
    ObjPlayer == Player.

is_state(ExpectedState, State) when ExpectedState =:= State -> true;
is_state(_ExpectdState, _State) -> false.
