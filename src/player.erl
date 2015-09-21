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
         move_obj/2,
         move_unit/2,
         attack/3,
         guard/1,
         dodge/1,
         survey/1,
         harvest/2,
         loot/2,
         item_transfer/2,
         item_split/2,
         explore/2,
         exit_local/0,
         structure_list/0,
         build/2,
         finish_build/2,
         recipe_list/1,
         craft/2,
         equip/2,
         assign/2,
         cancel/1,
         set_event_lock/1]).

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
    item:get(Item).

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

attack(AttackType, SourceId, TargetId) ->
    Result = not is_event_locked() andalso
             combat:has_stamina(SourceId, {attack, AttackType}),

    add_action(Result, attack, {AttackType, SourceId, TargetId}).

guard(SourceId) ->
    Result = not is_event_locked(),
    add_action(Result, guard, SourceId).

dodge(SourceId) ->
    Result = not is_event_locked() andalso
             combat:has_stamina(SourceId, dodge),

    add_action(Result, dodge, SourceId). 

move_unit(UnitId, Pos) ->
    Player = get(player_id),
    [Unit] = db:read(local_obj, UnitId),
    NumTicks = local:movement_cost(Unit, Pos),
    lager:info("NumTicks: ~p", [NumTicks]),
   
    ValidState = Unit#local_obj.state =/= dead, 
    ValidClass = Unit#local_obj.class =:= unit,
    ValidAdjacent = map:is_adjacent(Unit#local_obj.pos, Pos),
    ValidPos = local:is_empty(Pos),
    NearbyHero = local_obj:is_nearby_hero(Unit, Player),

    lager:info("move_unit validation: ~p ~p ~p ~p", [ValidClass, ValidAdjacent, ValidPos, NearbyHero]),   
 
    Result = ValidState andalso
             ValidClass andalso 
             ValidAdjacent andalso
             ValidPos andalso
             NearbyHero,
    
    add_move_unit(Result, {Unit, Pos}, NumTicks).

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
    NumTicks = 20,

    [LocalObj] = db:read(local_obj, LocalObjId),

    ValidPlayer = is_player_owned(LocalObj#local_obj.player, Player),
    ValidState = is_state(LocalObj#local_obj.state, none),
    ValidResource = resource:is_valid(LocalObj#local_obj.pos, Resource),

    Result = ValidPlayer andalso
             ValidState andalso
             ValidResource,

    %Get objs on the same tile
    LocalObjs = db:index_read(local_obj, LocalObj#local_obj.pos, #local_obj.pos),

    AutoHarvest = resource:is_auto(LocalObjs, Resource),

    add_harvest_event(Result, {LocalObjId, Resource, AutoHarvest}, NumTicks).

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

item_split(ItemId, Quantity) ->
    Player = get(player_id),
    [Item] = item:get(ItemId),
    {Owner} = bson:lookup(owner, Item),
    {CurrentQuantity} = bson:lookup(quantity, Item),

    [OwnerObj] = db:read(local_obj, Owner),

    ValidSplit = Player =:= OwnerObj#local_obj.player andalso
                 CurrentQuantity > 1 andalso
                 CurrentQuantity > Quantity,

    case ValidSplit of
        true ->
            lager:info("Splitting item"),
            item:split(Item, Quantity),
            Perception = item:obj_perception(Owner),
            lager:info("Perception: ~p", [Perception]),
            <<"Item split">>;
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
    lager:info("Build ~p ~p", [LocalObjId, Structure]),
    PlayerId = get(player_id),

    %Validates LocalObj and Structure, player process will crash if not valid
    [LocalObj] = db:read(local_obj, LocalObjId),
    lager:info("LocalObj: ~p", [LocalObj]),
    StructureType = local_obj:get_type(Structure),
    lager:info("StructureType: ~p", [StructureType]),
    {StructureSubclass} = bson:lookup(subclass, StructureType),    
    lager:info("StructureSubclass: ~p", [StructureSubclass]),
    ValidPlayer = PlayerId =:= LocalObj#local_obj.player,
    ValidLocation = structure:valid_location(StructureSubclass,
                                             LocalObj#local_obj.global_pos,
                                             LocalObj#local_obj.pos),

    ValidBuild = ValidPlayer andalso ValidLocation,

    case ValidBuild of
        true ->
            structure:start_build(PlayerId, 
                                  LocalObj#local_obj.global_pos, 
                                  LocalObj#local_obj.pos, 
                                  StructureType);
        false ->
            lager:info("Build failed")
    end.

finish_build(SourceId, StructureId) ->
    PlayerId = get(player_id),

    [Source] = db:read(local_obj, SourceId),
    [Structure] = db:read(local_obj, StructureId),

    lager:info("Structure state: ~p", [Structure#local_obj.state]),

    finish_build(PlayerId, Source, Structure).

finish_build(PlayerId, Source, Structure = #local_obj {state = founded}) -> 
    StructureM = local_obj:get_stats(Structure#local_obj.id),
    {NumTicks} = bson:lookup(build_time, StructureM),
    
    ValidFinish = Source#local_obj.pos =:= Structure#local_obj.pos andalso
                  Structure#local_obj.player =:= PlayerId andalso
                  structure:check_req(StructureM),

    add_finish_build(ValidFinish, {Source#local_obj.id, 
                                   Structure#local_obj.global_pos, 
                                   Structure#local_obj.id}, NumTicks),

    [{<<"result">>, atom_to_binary(ValidFinish, latin1)},
    {<<"build_time">>, NumTicks * 4}];

finish_build(PlayerId, Source, Structure = #local_obj {state = under_construction}) ->
    StructureM = local_obj:get_stats(Structure#local_obj.id),
    {NumTicks} = bson:lookup(build_time, StructureM),

    ValidFinish = Source#local_obj.pos =:= Structure#local_obj.pos andalso
                  Structure#local_obj.player =:= PlayerId,
 
     add_finish_build(ValidFinish, {Source#local_obj.id, 
                                   Structure#local_obj.global_pos, 
                                   Structure#local_obj.id}, NumTicks),

    [{<<"result">>, atom_to_binary(ValidFinish, latin1)},
    {<<"build_time">>, NumTicks}].

recipe_list(SourceId) ->
    Player = get(player_id),
    [LocalObj] = db:read(local_obj, SourceId),

    ValidPlayer = Player =:= LocalObj#local_obj.player,

    Result = case ValidPlayer of
                true ->
                    structure:recipe_list(LocalObj);
                false ->
                    <<"Source not owned by player">>
             end,
    Result.

craft(SourceId, Recipe) ->
    Player = get(player_id),
    [LocalObj] = db:read(local_obj, SourceId),

    ValidPlayer = Player =:= LocalObj#local_obj.player,

    Result = case ValidPlayer of
                true ->
                    structure:craft(LocalObj, Recipe);
                false ->
                    <<"Source not owned by player">>
             end,
    Result.

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

assign(SourceId, TargetId) ->
    Player = get(player_id),
    
    [SourceObj] = db:read(local_obj, SourceId),
    [TargetObj] = db:read(local_obj, TargetId),

    ValidPlayer1 = is_player_owned(SourceObj#local_obj.player, Player),
    ValidPlayer2 = is_player_owned(TargetObj#local_obj.player, Player),
    NearbyHero = local_obj:is_nearby_hero(SourceObj, Player),
    
    Result = ValidPlayer1 and ValidPlayer2 and NearbyHero,

    ReturnMsg = case Result of
                    true ->
                        villager:assign(SourceId, TargetId);
                    false ->
                        <<"Source or target not owned by player">>
                end,
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
add_harvest_event(true, {LocalObjId, Resource, Auto}, NumTicks) ->
    %Update obj state
    local:update_state(LocalObjId, harvesting),

    EventData = {LocalObjId, Resource, NumTicks, Auto},
    game:add_event(self(), harvest, EventData, LocalObjId, NumTicks).

add_finish_build(false, _EventData, _Ticks) ->
    lager:info("Finish Build failed"),
    none;

add_finish_build(true, {LocalObjId, GlobalPos, StructureId}, NumTicks) ->
    game:cancel_event(LocalObjId),

    EventData = {LocalObjId, GlobalPos, StructureId},

    local:update_state(LocalObjId, building),
    local:update_state(StructureId, under_construction),

    game:add_event(self(), finish_build, EventData, LocalObjId, NumTicks).

add_action(false, _, _) ->
    lager:info("Action failed"),
    none;
add_action(true, attack, ActionData) -> 
    set_event_lock(true),
    {AttackType, SourceId, TargetId} = ActionData,
    combat:attack(AttackType, SourceId, TargetId),
    combat:sub_stamina(SourceId, combat:stamina_cost({attack, AttackType})),
    NumTicks = combat:num_ticks({attack, AttackType}),
    game:add_event(self(), action, SourceId, SourceId, NumTicks);
add_action(true, guard, SourceId) ->
    set_event_lock(true),
    combat:guard(SourceId),
    NumTicks = combat:num_ticks(guard),
    game:add_event(self(), action, SourceId, SourceId, NumTicks);
add_action(true, dodge, SourceId) ->
    set_event_lock(true),
    combat:dodge(SourceId),
    combat:sub_stamina(SourceId, combat:stamina_cost(dodge)),
    NumTicks = combat:num_ticks(dodge),
    game:add_event(self(), action, SourceId, SourceId, NumTicks).

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

add_move_unit(true, {Unit, NewPos}, NumTicks) ->
    %Cancel any events associated with previous state
    game:cancel_event(Unit#local_obj.id),

    %Update unit state
    local:update_state(Unit#local_obj.id, moving),
    
    %Create event data
    EventData = {Unit#local_obj.global_pos,
                 Unit#local_obj.player,
                 Unit#local_obj.id,
                 NewPos},

    game:add_event(self(), move_local_obj, EventData, Unit#local_obj.id, NumTicks);

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

set_event_lock(State) ->
    put(event_lock, State).

is_event_locked() ->
    is_event_locked(get(event_lock)).
is_event_locked(undefined) ->
    false;
is_event_locked(State) ->
    State.


