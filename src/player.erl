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
         survey/1,
         harvest/2,
         loot/2,
         item_transfer/2,
         explore/2,
         exit_local/0,
         structure_list/0,
         build/2,
         finish_build/2,
         equip/2,
         cancel/1]).

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
    local_obj:get_info(Id).

get_info_item(Item) ->
    lager:info("get_info_item ~p", [Item]),
    item:get_info(Item).

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
    [Unit] = db:read(local_obj, UnitId),
    
    ValidClass = Unit#local_obj.class =:= unit,
    ValidAdjacent = map:is_adjacent(Unit#local_obj.pos, Pos),
    ValidPos = local:is_empty(Unit#local_obj.global_pos, Pos),

    lager:info("move_unit validation: ~p ~p ~p", [ValidClass, ValidAdjacent, ValidPos]),   
 
    Result = ValidClass andalso 
             ValidAdjacent andalso
             ValidPos,
    
    add_move_unit(Result, {Unit#local_obj.global_pos, Player, UnitId, Pos}, NumTicks).

survey(LocalObjId) ->
    lager:info("Survey: ~p", [LocalObjId]),
    Player = get(player_id),
    [LocalObj] = db:read(local_obj, LocalObjId),

    ValidPlayer = is_player_owned(LocalObj#local_obj.player, Player),
    
    Result = case ValidPlayer of
                 true ->
                     resource:survey(LocalObj#local_obj.pos);
                 false -> 
                     <<"Invalid obj">>
             end,
    Result.

harvest(LocalObjId, Resource) ->
    Player = get(player_id),
    NumTicks = 8,

    [LocalObj] = db:read(local_obj, LocalObjId),

    ValidPlayer = is_player_owned(LocalObj#local_obj.player, Player),
    ValidState = is_state(LocalObj#local_obj.state, none),

    %TODO add validation
    Result = ValidPlayer and ValidState,

    add_harvest_event(Result, {LocalObjId, Resource}, NumTicks).

loot(SourceId, ItemId) ->
    %TODO add validation
    item:transfer(ItemId, SourceId),
    Items = item:get_by_owner(SourceId),
    Items.

item_transfer(TargetId, ItemId) ->
    Player = get(player_id),
    [Item] = item:get(ItemId),
    {Owner} = bson:lookup(owner, Item),
    [OwnerObj] = db:read(local_obj, Owner),   
    [TargetObj] = db:read(local_obj, TargetId), 

    ValidOwner = Player =:= OwnerObj#local_obj.player andalso
                 OwnerObj#local_obj.pos =:= TargetObj#local_obj.pos,

    case ValidOwner of 
        true ->
            lager:info("Transfering item"),
            item:transfer(ItemId, TargetId),
            <<"success">>;
        false ->
            lager:info("Player does not own item: ~p", [ItemId]),
            <<"Player does not own item">>
    end.

explore(_Id, _GlobalPos) ->
    %TODO add validation
    PlayerId = get(player_id),

    [Obj] = db:index_read(obj, PlayerId, #obj.player),
    lager:info("Obj: ~p", [Obj]),

    case Obj#obj.state of
        none ->
            game:trigger_global(),
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

structure_list() ->
    _PlayerId = get(player_id),
    structure:list().

build(LocalObjId, Structure) ->
    %TODO add validation
    PlayerId = get(player_id),
    [LocalObj] = db:read(local_obj, LocalObjId),

    ValidBuild = PlayerId =:= LocalObj#local_obj.player,

    case ValidBuild of
        true ->
            structure:start_build(PlayerId, 
                                  LocalObj#local_obj.global_pos, 
                                  LocalObj#local_obj.pos, 
                                  Structure);
        false ->
            lager:info("Build failed")
    end.

finish_build(SourceId, StructureId) ->
    lager:info("finish build ~p", [StructureId]),
    PlayerId = get(player_id),
    [Structure] = db:read(local_obj, StructureId),
    lager:info("Structure: ~p", [Structure]),
    StructureM = local_obj:get_stats(StructureId),
    lager:info("StructureM: ~p", [StructureM]),
    {NumTicks} = bson:lookup(build_time, StructureM),
    %TODO add validation to make sure id is a structure
    ValidFinish = PlayerId =:= Structure#local_obj.player andalso
                  structure:check_req(StructureM),

    add_finish_build(ValidFinish, {SourceId, Structure#local_obj.global_pos, StructureId}, NumTicks),

    atom_to_binary(ValidFinish, latin1).

equip(Id, ItemId) ->
    Player = get(player_id),

    [LocalObj] = db:read(local_obj, Id),
    [Item] = item:get(ItemId),
    {ItemOwner} = bson:lookup(owner, Item),

    ValidPlayer = is_player_owned(LocalObj#local_obj.player, Player),
    ValidState = is_state(LocalObj#local_obj.state, none),
    ValidItem = Id == ItemOwner,

    Result = ValidItem and ValidState and ValidPlayer,

    ReturnMsg = add_equip(Result, ItemId),
    ReturnMsg.

cancel(SourceId) ->
    PlayerId = get(player_id),
    [LocalObj] = db:read(local_obj, SourceId),

    ValidOwner = PlayerId =:= LocalObj#local_obj.player,
    
    cancel_event(ValidOwner, SourceId).
%
%Internal functions
%

add_harvest_event(false, _EventData, _Ticks) ->
    lager:info("Harvest failed"),
    none;
add_harvest_event(true, {LocalObjId, Resource}, NumTicks) ->
    %Update obj state
    local:update_state(LocalObjId, harvesting),

    EventData = {LocalObjId, Resource},
    game:add_event(self(), harvest, EventData, LocalObjId, NumTicks).

add_finish_build(false, _EventData, _Ticks) ->
    lager:info("Finish Build failed"),
    none;

add_finish_build(true, {LocalObjId, GlobalPos, StructureId}, NumTicks) ->
    EventData = {LocalObjId, GlobalPos, StructureId},

    local:update_state(LocalObjId, building),

    game:add_event(self(), finish_build, EventData, LocalObjId, NumTicks).

add_attack_obj_event(false, _EventData, _Ticks) ->
    lager:info("Attack failed"),
    none;
add_attack_obj_event(true, {SourceId, TargetId}, NumTicks) ->

    EventData = {SourceId, TargetId},
    game:add_event(self(), attack_obj, EventData, none, NumTicks).

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

    game:add_event(self(), move_obj, EventData, none, NumTicks).

add_move_unit(true, {GlobalPos, Player, UnitId, NewPos}, NumTicks) ->
    %Update unit state
    local:update_state(UnitId, moving),
    
    %Create event data
    EventData = {GlobalPos,
                 Player,
                 UnitId,
                 NewPos},

    game:add_event(self(), move_local_obj, EventData, UnitId, NumTicks);

add_move_unit(false, _, _) ->
    lager:info("Move unit failed"),
    none.

add_exit_local(true, {GlobalObjId, GlobalPos}, NumTicks) ->
    EventData = {GlobalObjId, GlobalPos},
    
    game:add_event(self(), exit_local, EventData, GlobalObjId, NumTicks);

add_exit_local(false, _, _) ->
    lager:info("Exit failed"),
    none.

add_equip(false, _EventData) ->
    lager:info("Equip failed"),
    none;
add_equip(true, ItemId) ->
    item:equip(ItemId).

cancel_event(false, _SourceId) ->
    <<"Invalid sourceid">>;
cancel_event(true, SourceId) ->
    local:update_state(SourceId, none),
    game:cancel_event(SourceId).

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
