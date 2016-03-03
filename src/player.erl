% Author: Peter
% %% Created: Nov 11, 2014
% %% Description: Player module
-module(player).

-include("schema.hrl").
-include("common.hrl").

-export([init_perception/1,
         get_stats/1, 
         get_info_tile/2,
         get_info_unit/1,
         get_info_item/1,
         move/2,
         ford/2,
         attack/3,
         defend/2,
         survey/1,
         harvest/2,
         loot/2,
         item_transfer/2,
         item_split/2,
         structure_list/0,
         build/2,
         finish_build/2,
         recipe_list/1,
         process_resource/1,
         craft/2,
         equip/1,
         unequip/1,
         rest/1,
         assign/2,
         cancel/1,
         set_event_lock/2,
         process_checks/1]).

init_perception(PlayerId) ->
    %Get objs
    AllObjs = db:index_read(obj, PlayerId, #obj.player),

    %Get explored tile list
    Explored = map:get_explored(PlayerId, all),
    Objs = get_visible_objs(AllObjs, []),

    lager:info([{player, PlayerId}], "Explored: ~p", [Explored]),
    lager:info([{player, PlayerId}], "Objs: ~p", [Objs]),

    {PlayerId, Explored, Objs}.

get_stats(Id) ->
    Player = get(player_id),
    [Obj] = db:read(obj, Id),

    ValidPlayer = is_player_owned(Obj#obj.player, Player),

    case ValidPlayer of
        true ->
            obj:get_stats(Id);
        false ->
            []
    end.

get_info_tile(Id, Pos) ->
    lager:info("info_tile"),
    [Unit] = db:read(obj, Id),

    Distance = map:distance(Unit#obj.pos, Pos),

    case Distance =< Unit#obj.vision of
        true ->
            game:get_info_tile(Pos);
        false ->
            #{<<"errmsg">> => "Cannot see that tile"}
    end.

get_info_unit(Id) ->
    [Unit] = db:read(obj, Id),
    case Unit#obj.player =:= get(player_id) of
        true -> 
            obj:get_info(Id);
        false ->
            obj:get_info_other(Id)
    end.

get_info_item(ItemId) when is_tuple(ItemId) ->
    lager:info("get_info_item ~p", [ItemId]),
    item:get_map(ItemId);
get_info_item(ItemName) ->
    item:get_map_by_name(ItemName).

attack(AttackType, SourceId, TargetId) ->
    PlayerId = get(player_id),
    [SourceObj] = db:read(obj, SourceId),
    [TargetObj] = db:read(obj, TargetId),

    Checks = [{is_player_owned(SourceObj#obj.player, PlayerId), "Unit is not owned by player"},
              {not game:has_pre_events(SourceId), "Unit is busy"},
              {map:is_adjacent(SourceObj#obj.pos, TargetObj#obj.pos), "Target is not adjacent"},
              {combat:is_target_alive(TargetObj), "Target is dead"},
              {combat:is_targetable(TargetObj), "Cannot attack target"},
              {combat:has_stamina(SourceId, {attack, AttackType}), "Not enough stamina"}],

    case process_checks(Checks) of
        true ->
            NumTicks = combat:num_ticks({attack, AttackType}),
            StaminaCost = combat:stamina_cost({attack, AttackType}),
    
            combat:attack(AttackType, SourceId, TargetId),

            game:add_event(self(), attack, AttackType, SourceId, NumTicks),

            #{<<"sourceid">> => util:bin_to_hex(SourceId),
              <<"attacktype">> => AttackType,
              <<"cooldown">> => NumTicks / ?TICKS_SEC,
              <<"stamina_cost">> => StaminaCost};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

defend(DefendType, SourceId) ->
    PlayerId = get(player_id),
    [Obj] = db:read(obj, SourceId),

    Checks = [{is_player_owned(Obj#obj.player, PlayerId), "Unit is not owned by player"},
              {not game:has_pre_events(SourceId), "Unit is busy"},
              {combat:has_stamina(SourceId, {defend, DefendType}), "Not enough stamina"}],

    case process_checks(Checks) of
        true ->             
            NumTicks = combat:num_ticks({defend, DefendType}),
            StaminaCost = combat:stamina_cost({defend, DefendType}),
            
            combat:defend(DefendType, SourceId),

            EventData = {SourceId, DefendType},

            game:add_event(self(), defend, EventData, SourceId, NumTicks),

            #{<<"sourceid">> => util:bin_to_hex(SourceId),
              <<"defendtype">> => DefendType,
              <<"cooldown">> => NumTicks / ?TICKS_SEC,
              <<"stamina_cost">> => StaminaCost};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

move(SourceId, Pos) ->
    PlayerId = get(player_id),
    [Obj] = db:read(obj, SourceId),

    Checks = [{is_player_owned(Obj#obj.player, PlayerId), "Unit is not owned by player"},
              {not game:has_pre_events(SourceId), "Unit is busy"},
              {Obj#obj.class =:= unit, "Obj cannot move"},
              {Obj#obj.state =/= dead, "Unit is dead"}, 
              {map:is_adjacent(Obj#obj.pos, Pos), "Unit is not adjacent to position"},
              {map:is_passable(Pos), "Tile is not passable"},
              {not obj:is_villager(Obj), "Cannot move villager"},
              {obj:is_empty(Pos), "Position is occupied"},
              {obj:is_hero_nearby(Obj, PlayerId), "Unit not near Hero"}],
              
    case process_checks(Checks) of
        true ->
            game:cancel_event(SourceId),

            %Update state to moving
            obj:update_state(Obj#obj.id, moving),

            %Create event data
            EventData = {PlayerId,
                         SourceId,
                         Pos},

            NumTicks = obj:movement_cost(Obj, Pos),

            game:add_event(self(), move, EventData, SourceId, NumTicks),

            #{<<"move_time">> => NumTicks * ?TICKS_SEC};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

ford(SourceId, Pos) ->
    PlayerId = get(player_id),
    [Obj] = db:read(obj, SourceId),

    Checks = [{is_player_owned(Obj#obj.player, PlayerId), "Unit is not owned by player"},
              {not game:has_pre_events(SourceId), "Unit is busy"},
              {Obj#obj.class =:= unit, "Obj cannot move"},
              {Obj#obj.state =/= dead, "Unit is dead"}, 
              {map:is_adjacent(Obj#obj.pos, Pos), "Unit is not adjacent to position"},
              {map:is_river(Pos), "Can only ford rivers"},
              {obj:is_hero_nearby(Obj, PlayerId), "Unit not near Hero"}],

    case process_checks(Checks) of
        true ->
            game:cancel_event(SourceId),

            %Update state to fording
            obj:update_state(Obj#obj.id, fording),

            %Create event data
            EventData = {PlayerId,
                         SourceId,
                         Obj#obj.pos, 
                         Pos},

            NumTicks = obj:movement_cost(Obj, Pos),

            game:add_event(self(), ford, EventData, SourceId, NumTicks),

            #{<<"move_time">> => NumTicks * ?TICKS_SEC};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

survey(ObjId) ->
    lager:info("Survey: ~p", [ObjId]),
    Player = get(player_id),
    [Obj] = db:read(obj, ObjId),

    ValidPlayer = is_player_owned(Obj#obj.player, Player),
    
    Result = case ValidPlayer of
                 true ->
                     resource:survey(Obj#obj.pos);
                 false -> 
                     <<"Invalid obj">>
             end,
    Result.

harvest(ObjId, Resource) ->
    Player = get(player_id),
    NumTicks = 20,

    [Obj] = db:read(obj, ObjId),

    ValidPlayer = is_player_owned(Obj#obj.player, Player),
    ValidState = is_state(Obj#obj.state, none),
    ValidResource = resource:is_valid(Obj#obj.pos),

    Result = ValidPlayer andalso
             ValidState andalso
             ValidResource,

    %Get objs on the same tile
    Objs = db:index_read(obj, Obj#obj.pos, #obj.pos),

    AutoHarvest = resource:is_auto(Objs, Resource),

    add_harvest_event(Result, {ObjId, Resource, Obj#obj.pos, AutoHarvest}, NumTicks).

loot(SourceId, ItemId) ->
    Item = item:get_rec(ItemId),
    Owner = Item#item.owner,

    item:transfer(ItemId, SourceId),

    Items = item:get_by_owner(Owner),
    {Owner, Items}.

item_transfer(TargetId, ItemId) ->
    Player = get(player_id),
    Item = item:get_rec(ItemId),
    Owner = Item#item.owner,

    OwnerObj = obj:get(Owner),   
    TargetObj = obj:get(TargetId), 
    lager:info("OwnerObj: ~p", [OwnerObj]),
    lager:info("TargetObj: ~p", [TargetObj]),

    Checks = [{TargetObj =/= false, "Invalid transfer target"},
              {is_player_owned(OwnerObj, Player), "Item not owned by player"},              
              {is_same_pos(OwnerObj, TargetObj) or
               map:is_adjacent(OwnerObj, TargetObj), "Item is not nearby"}],

    case process_checks(Checks) of 
        true ->
            lager:info("Transfering item"),
            ItemMap = item:transfer(ItemId, TargetId),
            obj:item_transfer(TargetObj, ItemMap),
            #{<<"result">> => <<"success">>};
       {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

item_split(ItemId, Quantity) ->
    Player = get(player_id),

    Checks = [{item:is_valid_split(Player, ItemId, Quantity), "Cannot split item"}],

    case process_checks(Checks) of
        true ->
            lager:info("Splitting item"),
            item:split(ItemId, Quantity),
            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

structure_list() ->
    structure:list().

build(ObjId, StructureName) ->
    lager:info("Build ~p ~p", [ObjId, StructureName]),
    PlayerId = get(player_id),

    %Validates Obj and Structure, player process will crash if not valid
    [Obj] = db:read(obj, ObjId),
    lager:info("Obj: ~p", [Obj]),
    StructureSubclass = obj_def:value(StructureName, <<"subclass">>),
    lager:info("StructureSubclass: ~p", [StructureSubclass]),

    ValidPlayer = PlayerId =:= Obj#obj.player,
    ValidLocation = structure:valid_location(StructureSubclass,
                                             Obj#obj.pos),

    ValidBuild = ValidPlayer andalso ValidLocation,

    case ValidBuild of
        true ->
            structure:start_build(PlayerId, 
                                  Obj#obj.pos, 
                                  StructureName,
                                  StructureSubclass);
        false ->
            lager:info("Build failed")
    end.

finish_build(SourceId, StructureId) ->
    PlayerId = get(player_id),

    [Source] = db:read(obj, SourceId),
    [Structure] = db:read(obj, StructureId),

    lager:info("Structure state: ~p", [Structure#obj.state]),

    finish_build(PlayerId, Source, Structure).

finish_build(PlayerId, Source, Structure = #obj {state = founded}) -> 
    NumTicks = obj_attr:value(Structure#obj.id, <<"build_time">>),
    
    ValidFinish = Source#obj.pos =:= Structure#obj.pos andalso
                  Structure#obj.player =:= PlayerId andalso
                  structure:check_req(Structure#obj.id),

    add_finish_build(ValidFinish, {Source#obj.id, 
                                   Structure#obj.id}, NumTicks),

    [{<<"result">>, atom_to_binary(ValidFinish, latin1)},
    {<<"build_time">>, NumTicks * 4}];

finish_build(PlayerId, Source, Structure = #obj {state = under_construction}) ->
    NumTicks = obj_attr:value(Structure#obj.id, <<"build_time">>),

    ValidFinish = Source#obj.pos =:= Structure#obj.pos andalso
                  Structure#obj.player =:= PlayerId,
 
     add_finish_build(ValidFinish, {Source#obj.id, 
                                   Structure#obj.id}, NumTicks),

    [{<<"result">>, atom_to_binary(ValidFinish, latin1)},
    {<<"build_time">>, NumTicks}].

recipe_list(SourceId) ->
    Player = get(player_id),
    [Obj] = db:read(obj, SourceId),

    lager:info("Player: ~p Obj.player: ~p", [Player, Obj#obj.player]),
    ValidPlayer = Player =:= Obj#obj.player,

    Result = case ValidPlayer of
                true ->
                    structure:recipe_list(Obj);
                false ->
                    <<"Source not owned by player">>
             end,
    Result.

process_resource(StructureId) ->
    Player = get(player_id),
    [Structure] = db:read(obj, StructureId),
    [Unit] = obj:get_unit_by_pos(Structure#obj.pos),
    NumTicks = ?TICKS_SEC * 10,

    Checks = [{not is_event_locked(StructureId), "Event in progress"},
              {Player =:= Structure#obj.player, "Structure not owned by player"},
              {Player =:= Unit#obj.player, "Unit not owned by player"},
              {structure:has_process_res(StructureId), "No resources in structure"}],

    Result = process_checks(Checks),
    lager:info("Process Resource: ~p", [Result]),
    add_process_resource(Result, StructureId, Unit#obj.id, NumTicks),

    Reply = to_reply(Result) ++ [{<<"process_time">>, NumTicks}],
    lager:info("Reply: ~p", [Reply]),
    Reply.
    
craft(StructureId, Recipe) ->
    Player = get(player_id),
    [Structure] = db:read(obj, StructureId),
    [Unit] = obj:get_unit_by_pos(Structure#obj.pos),

    Checks = [{not is_event_locked(StructureId), "Event in process"},
              {Player =:= Structure#obj.player, "Structure not owned by player"},
              {Player =:= Unit#obj.player, "Unit not owned by player"},
              {structure:check_recipe_req(StructureId, Recipe), "Missing recipe requirements"}],

    NumTicks = ?TICKS_SEC * 10,

    Result = process_checks(Checks),

    add_craft(Result, StructureId, Unit#obj.id, Recipe, NumTicks),

    Reply = to_reply(Result) ++ [{<<"process_time">>, NumTicks}],
    Reply.

equip(ItemId) ->
    Player = get(player_id),

    Item = item:get_rec(ItemId),
    ItemOwner = Item#item.owner,
    ItemSlot = item_attr:value(ItemId, <<"slot">>),

    [Obj] = db:read(obj, ItemOwner),

    Checks = [{item:is_equipable(Item), "Item not equipable"},
              {item:is_slot_free(ItemOwner, ItemSlot), "Item slot not empty"},
              {Obj#obj.class =:= unit, "Only units can equip items"},
              {Player =:= Obj#obj.player, "Item not owned by player"},
              {is_state(Obj#obj.state, none), "Unit is busy"}],

    Result = process_checks(Checks),

    add_equip(Result, ItemId),

    Reply = to_reply(Result),
    Reply.

unequip(ItemId) ->
    Player = get(player_id),
    Item = item:get_rec(ItemId),
    ItemOwner = Item#item.owner,
    
    [Obj] = db:read(obj, ItemOwner),

    Checks = [{Player =:= Obj#obj.player, "Item not owned by player"},
              {is_state(Obj#obj.state, none), "Unit is busy"}],

    Result = process_checks(Checks),

    add_unequip(Result, ItemId),

    Reply = to_reply(Result),
    Reply.

rest(ObjId) ->
    Player = get(player_id),

    [Obj] = db:read(obj, ObjId),

    Checks = [{is_player_owned(Player, Obj#obj.player), "Unit not owned by player"},
              {is_state(Obj#obj.state, none), "Unit is busy"}],

    case process_checks(Checks) of
        true ->
            lager:info("Resting"),
            obj:update_state(Obj, resting),
            
            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

assign(SourceId, TargetId) ->
    Player = get(player_id),
    
    SourceObj = obj:get(SourceId),
    TargetObj = obj:get(SourceId),

    Checks = [{is_player_owned(SourceObj, Player), "Source is not owned by player"},
              {is_player_owned(TargetObj, Player), "Target is not owned by player"},
              {obj:is_hero_nearby(TargetObj, Player), "Unit is not near Hero"}],

    case process_checks(Checks) of
        true ->
            lager:info("Assigning villager"),
            villager:assign(SourceId, TargetId),
            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

cancel(SourceId) ->
    PlayerId = get(player_id),
    [Obj] = db:read(obj, SourceId),

    ValidOwner = PlayerId =:= Obj#obj.player,
    
    cancel_event(ValidOwner, SourceId).
%
%Internal functions
%

add_harvest_event(false, _EventData, _Ticks) ->
    lager:info("Harvest failed"),
    none;
add_harvest_event(true, {ObjId, Resource, Pos, Auto}, NumTicks) ->
    %Update obj state
    obj:update_state(ObjId, harvesting),

    %Check for encounter
    encounter:check(Pos),

    EventData = {ObjId, Resource, Pos, NumTicks, Auto},
    game:add_event(self(), harvest, EventData, ObjId, NumTicks).

add_finish_build(false, _EventData, _Ticks) ->
    lager:info("Finish Build failed"),
    none;

add_finish_build(true, {ObjId, StructureId}, NumTicks) ->
    game:cancel_event(ObjId),

    EventData = {ObjId, StructureId},

    obj:update_state(ObjId, building),
    obj:update_state(StructureId, under_construction),

    game:add_event(self(), finish_build, EventData, ObjId, NumTicks).

add_process_resource({false, Error}, _StructureId, _UnitId, _NumTicks) ->
    lager:info("Process_resource error: ~p", [Error]);
add_process_resource(true, StructureId, UnitId, NumTicks) ->
    EventData = {StructureId, UnitId, NumTicks},

    obj:update_state(UnitId, processing),

    game:add_event(self(), process_resource, EventData, UnitId, NumTicks).

add_craft({false, Error}, _StructureId, _UnitId, _Recipe, _NumTicks) ->
    lager:info("Craft error: ~p", [Error]);
add_craft(true, StructureId, UnitId, Recipe, NumTicks) ->
    EventData = {StructureId, UnitId, Recipe},

    obj:update_state(UnitId, crafting),
    game:add_event(self(), craft, EventData, UnitId, NumTicks).

add_equip({false, Error}, _Data) ->
    lager:info("Equip failed error: ~p", [Error]);
add_equip(true, ItemId) ->
    item:equip(ItemId).

add_unequip({false, Error}, _Data) ->
    lager:info("Unequip failed error: ~p", [Error]);
add_unequip(true, ItemId) ->
    item:unequip(ItemId).

cancel_event(false, _SourceId) ->
    <<"Invalid sourceid">>;
cancel_event(true, SourceId) ->
    obj:update_state(SourceId, none),
    game:cancel_event(SourceId).

get_visible_objs([], Objs) ->
    Objs;

get_visible_objs([Obj | Rest], Objs) ->

    {X, Y} = Obj#obj.pos,
    NearbyObjs = map:get_nearby_objs(X, Y, 2),
    NewObjs = NearbyObjs ++ Objs,

    get_visible_objs(Rest, NewObjs).

is_player_owned(invalid, _Player) ->
    false;
is_player_owned(ObjPlayer, Player) when is_record(ObjPlayer, obj) ->
    ObjPlayer#obj.player == Player;
is_player_owned(ObjPlayer, Player) ->
    ObjPlayer == Player.

is_same_pos(SourceObj, TargetObj) when (SourceObj =:= false) or (TargetObj =:= false) ->
    false;
is_same_pos(SourceObj, TargetObj) ->
    SourceObj#obj.pos =:= TargetObj#obj.pos.

is_state(ExpectedState, State) when ExpectedState =:= State -> true;
is_state(_ExpectdState, _State) -> false.

set_event_lock(SourceId, State) ->
    put({event_lock, SourceId}, State).

is_event_locked(SourceId) ->
    EventLock = get({event_lock, SourceId}),
    Result = case EventLock of
                 undefined -> false;
                 State -> State
             end,
    Result.

process_checks([]) ->
    true;
process_checks([{false, Error} | _Rest]) ->
    {false, Error};
process_checks([_Check | Rest]) ->
    process_checks(Rest).

to_reply(true) ->
    [{<<"result">>, <<"success">>}];
to_reply({false, Error}) ->
    [{<<"result">>, <<"failed">>}, {<<"errmsg">>, list_to_binary(Error)}].
