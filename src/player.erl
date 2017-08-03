% Author: Peter
% %% Created: Nov 11, 2014
% %% Description: Player module
-module(player).

-include("schema.hrl").
-include("common.hrl").

-export([init_perception/1,
         init_state/1,
         get_stats/1, 
         get_info_tile/2,
         get_info_unit/1,
         get_info_item/1,
         get_info_item_name/1,
         move/2,
         ford/2,
         combo/2,
         attack/3,
         defend/2,
         survey/1,
         prospect/1,
         harvest/2,
         loot/2,
         item_transfer/2,
         item_split/2,
         structure_list/0,
         build/2,
         upgrade/1,
         finish_build/2,
         recipe_list/1,
         refine/1,
         craft/2,
         equip/1,
         unequip/1,
         rest/1,
         hide/1,
         assign/2,
         follow/1,
         order_harvest/1,
         order_attack/2,
         clear/1,
         cancel/1,
         revent_response/1,
         set_event_lock/2,
         process_checks/1,
         is_player_owned/2]).

init_perception(PlayerId) ->
    %Get objs
    AllObjs = db:index_read(obj, PlayerId, #obj.player),

    %Get explored tile list
    Explored = map:get_explored(PlayerId, all),
    Objs = util:unique_list(get_visible_objs(AllObjs, [])),

    lager:info([{player, PlayerId}], "Explored: ~p", [Explored]),
    lager:info([{player, PlayerId}], "Objs: ~p", [Objs]),

    {PlayerId, Explored, Objs}.

init_state(PlayerId) ->
    Hero = obj:get_hero(PlayerId),

    case Hero#obj.state of
        revent -> 
            REvent = revent:get(Hero#obj.id),
            REventMap = revent:to_map(REvent),
            game:send_revent(PlayerId, REventMap);
        _ -> nothing
    end.

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
            Info = obj:get_info_other(Id),
            lager:info("Info Unit: ~p", [Info]),
            Info
    end.

get_info_item(ItemId) ->
    lager:info("get_info_item ~p", [ItemId]),
    case item:get_all_attr(ItemId) of
        invalid -> #{<<"errmsg">> => <<"Invalid Item">>};
        ItemMap -> ItemMap
    end.
get_info_item_name(ItemName) ->
    item:get_map_by_name(ItemName).

combo(SourceId, ComboType) ->
    PlayerId = get(player_id),
    [SourceObj] = db:read(obj, SourceId),
 
    Checks = [{is_player_owned(SourceObj#obj.player, PlayerId), "Unit is not owned by player"},
              {is_hero(SourceObj), "Can only attack with your hero"},
              {combat:is_combo_type(ComboType), "Is not a valid combo type"}],
  
    case process_checks(Checks) of
        true ->
            combat:combo(SourceId, ComboType),
            #{<<"result">> => <<"Success">>};
        {false, Error} ->
           #{<<"errmsg">> => list_to_binary(Error)}
  end.

attack(_AttackType, _, -1) ->
    #{<<"errmsg">> => <<"Invalid target">>};
attack(AttackType, SourceId, TargetId) ->
    PlayerId = get(player_id),
    [SourceObj] = db:read(obj, SourceId),
    [TargetObj] = db:read(obj, TargetId),

    Checks = [{is_player_owned(SourceObj#obj.player, PlayerId), "Unit is not owned by player"},
              {is_hero(SourceObj), "Can only attack with your hero"},
              %{not game:has_pre_events(SourceId), "Unit is busy"},
              {combat:in_range(SourceObj, TargetObj), "Target is not in range"},
              {combat:is_target_alive(TargetObj), "Target is dead"},
              {combat:is_targetable(TargetObj), "Cannot attack target"},
              {combat:has_stamina(SourceId, {attack, AttackType}), "Not enough stamina"}],

    case process_checks(Checks) of
        true ->
            NumTicks = combat:num_ticks({attack, AttackType}),
            StaminaCost = combat:stamina_cost({attack, AttackType}),
    
            combat:attack(AttackType, SourceId, TargetId),

            game:add_event(self(), attack, AttackType, SourceId, NumTicks),

            #{<<"sourceid">> => SourceId,
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
              {is_hero(Obj), "Can only defend with your hero"},
              %{not game:has_pre_events(SourceId), "Unit is busy"},
              {combat:has_stamina(SourceId, {defend, DefendType}), "Not enough stamina"}],

    case process_checks(Checks) of
        true ->             
            NumTicks = combat:num_ticks({defend, DefendType}),
            StaminaCost = combat:stamina_cost({defend, DefendType}),
            
            combat:defend(DefendType, SourceId),

            EventData = {SourceId, DefendType},

            game:add_event(self(), defend, EventData, SourceId, NumTicks),

            #{<<"sourceid">> => SourceId,
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
              {is_hero(Obj), "Can only move your hero"},
              {not game:has_pre_events(SourceId), "Unit is busy"},
              {Obj#obj.class =:= unit, "Obj cannot move"},
              {Obj#obj.state =/= dead, "Unit is dead"}, 
              {map:is_adjacent(Obj#obj.pos, Pos), "Unit is not adjacent to position"},
              {map:is_passable(Pos), "Tile is not passable"},
              {not obj:is_subclass(?VILLAGER, Obj), "Cannot move villager"},
              {obj:is_empty(Obj, Pos), "Position is occupied"},
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
              {is_hero(Obj), "Can only ford with your hero"},
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
    PlayerId = get(player_id),
    [Obj] = db:read(obj, ObjId),

    Checks = [{is_player_owned(Obj#obj.player, PlayerId), "Unit is not owned by player"},
              {is_hero(Obj), "Can only survey with your hero"},
              {not game:has_pre_events(ObjId), "Unit is busy"}],

    case process_checks(Checks) of
        true ->
            resource:survey(Obj#obj.pos);
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

prospect(ObjId) ->
    lager:info("Prospect: ~p", [ObjId]),
    PlayerId = get(player_id),
    [Obj] = db:read(obj, ObjId),

    Checks = [{is_player_owned(Obj#obj.player, PlayerId), "Unit is not owned by player"},
              {is_hero(Obj), "Can only survey with your hero"},
              {not game:has_pre_events(ObjId), "Unit is busy"}],

    case process_checks(Checks) of
        true ->
            obj:update_state(ObjId, prospecting),

            NumTicks = 8,

            lager:info("Adding Prospect event"),
            EventData = {ObjId, Obj#obj.pos},
            game:add_event(self(), prospect, EventData, ObjId, NumTicks),
            
            #{<<"prospect_time">> => NumTicks * ?TICKS_SEC};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.



harvest(ObjId, Resource) ->
    PlayerId = get(player_id),
    NumTicks = 20,

    [Obj] = db:read(obj, ObjId),

    Checks = [{is_player_owned(Obj#obj.player, PlayerId), "Unit is not owned by player"},
              {is_hero(Obj), "Can only survey with your hero"},
              {not game:has_pre_events(ObjId), "Unit is busy"},
              {resource:is_valid(Resource, Obj#obj.pos), "Invalid resource"}],

    case process_checks(Checks) of
        true ->
            %Get objs on the same tile
            Objs = db:index_read(obj, Obj#obj.pos, #obj.pos),
            AutoHarvest = resource:is_auto(Objs, Resource),
        
            %Update obj state
            obj:update_state(ObjId, harvesting),

            %Check for encounter
            encounter:check(Obj#obj.pos),

            EventData = {ObjId, Resource, Obj#obj.pos, NumTicks, AutoHarvest},
            game:add_event(self(), harvest, EventData, ObjId, NumTicks),

            #{<<"harvest_time">> => NumTicks * ?TICKS_SEC};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

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

    lager:info("OwnerObj: ~p TargetObj: ~p", [OwnerObj, TargetObj]),

    Checks = [{TargetObj =/= false, "Invalid transfer target"},
              {is_player_owned(OwnerObj, Player), "Item not owned by player"},              
              {is_same_pos(OwnerObj, TargetObj) or
               map:is_adjacent(OwnerObj, TargetObj), "Item is not nearby"},
              {is_structure_req(Item, TargetObj), "Item not required for structure construction"}],

    case process_checks(Checks) of 
        true ->
            lager:info("Transfering item"),

            process_item_transfer(Item, TargetObj),
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

build(BuilderId, StructureName) ->
    PlayerId = get(player_id),
    lager:info("PlayerId: ~p", [PlayerId]),

    Builder = obj:get(BuilderId),
    lager:info("Builder: ~p", [Builder]),
    StructureSubclass = obj_def:value(StructureName, <<"subclass">>),

    Checks = [{is_player_owned(Builder, PlayerId), "Builder not owned by player"},              
              {structure:valid_location(StructureSubclass, Builder#obj.pos), "Invalid structure location"},
              {Builder#obj.state =:= ?NONE, "Builder is busy"}],
    lager:info("Checks: ~p", [Checks]),
    case process_checks(Checks) of
        true ->
            lager:info("Building structure"),

            structure:start_build(PlayerId, 
                                  Builder#obj.pos, 
                                  StructureName,
                                  StructureSubclass),
            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

upgrade(StructureId) ->
    Player = get(player_id),

    Structure = obj:get(StructureId),   

    Checks = [{is_player_owned(Structure, Player), "Structure not owned by player"},              
              {Structure#obj.class =:= structure, "Object cannot be upgraded"},
              {Structure#obj.state =:= ?NONE, "Structure is busy"}, 
              {structure:has_upgrade_req(StructureId), "Structure missing required items to upgrade"}],

    case process_checks(Checks) of
        true ->
            lager:info("Upgrading structure"),

            obj:update_state(StructureId, ?UPGRADING),

            EventData = StructureId,
            NumTicks = obj_attr:value(StructureId, <<"upgrade_time">>),

            game:add_event(self(), upgrade, EventData, StructureId, NumTicks),

            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.
  
finish_build(SourceId, StructureId) ->
    PlayerId = get(player_id),
    [Structure] = db:read(obj, StructureId),

    lager:info("Structure state: ~p", [Structure#obj.state]),

    finish_build(PlayerId, SourceId, Structure).

finish_build(PlayerId, SourceId, Structure = #obj {state = ?FOUNDED}) -> 
    [Source] = db:read(obj, SourceId),
    NumTicks = obj_attr:value(Structure#obj.id, <<"build_time">>),
    
    Checks = [{Source#obj.pos =:= Structure#obj.pos, "Builder must be on the structure"},
              {Structure#obj.player =:= PlayerId, "Structure not owned by player"},
              {structure:has_req(Structure#obj.id), "Structure is missing required items"}],

    case process_checks(Checks) of
        true ->
            lager:info("Finishing structure"),
            game:cancel_event(SourceId),

            EventData = {SourceId, Structure#obj.id},

            obj:update_state(SourceId, building),
            obj:update_state(Structure#obj.id, ?PROGRESSING),

            game:add_event(self(), finish_build, EventData, SourceId, NumTicks),

            #{<<"build_time">> => NumTicks * 4};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end;

finish_build(PlayerId, SourceId, Structure = #obj {state = ?PROGRESSING}) ->
    [Source] = db:read(obj, SourceId),
    NumTicks = obj_attr:value(Structure#obj.id, <<"build_time">>),
    
    Checks = [{Source#obj.pos =:= Structure#obj.pos, "Builder must be on the structure"},
              {Structure#obj.player =:= PlayerId, "Structure not owned by player"}],

    case process_checks(Checks) of
        true ->
            lager:info("Finishing structure"),
            game:cancel_event(SourceId),

            EventData = {SourceId, Structure#obj.id},

            obj:update_state(SourceId, building),
            obj:update_state(Structure#obj.id, ?PROGRESSING),

            game:add_event(self(), finish_build, EventData, SourceId, NumTicks),

            #{<<"build_time">> => NumTicks * 4};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end;

finish_build(_PlayerId, _SourceId, Structure) ->
    lager:info("Invalid state of structure: ~p", [Structure#obj.state]).

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

refine(StructureId) when is_binary(StructureId) ->
    Structure = obj:get(StructureId),
    refine(Structure); 
refine(Structure) when is_record(Structure, obj) ->
    Player = get(player_id),
    Checks = [{not is_event_locked(Structure#obj.id), "Event in progress"},
              {is_player_owned(Structure, Player), "Structure not owned by player"},
              {Player =:= Structure#obj.player, "Structure not owned by player"},
              {villager:has_assigned(Structure#obj.id), "Missing assigned villager to structure"},
              {structure:has_refine_resources(Structure#obj.id), "No resources in structure"}],

    case process_checks(Checks) of
        true ->
            lager:info("Process resource process_checks success"),
            VillagerId = villager:get_by_structure(Structure#obj.id),
            villager:set_order_refine(VillagerId),
            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end;
refine(_StructureId) ->
    #{<<"errmsg">> => list_to_binary("Invalid structure")}.

craft(StructureId, Recipe) ->
    Player = get(player_id),
    [Structure] = db:read(obj, StructureId),

    Checks = [{not is_event_locked(StructureId), "Event in process"},
              {Player =:= Structure#obj.player, "Structure not owned by player"},
              {villager:has_assigned(StructureId), "Missing assigned villager to structure"},
              {structure:check_recipe_req(StructureId, Recipe), "Missing recipe requirements"},
               recipe:is_refine(Recipe), "Cannot craft refine recipe"],

    case process_checks(Checks) of
        true ->
            lager:info("Craft process_checks success"),
            VillagerId = villager:get_by_structure(StructureId),
            villager:set_order_craft(VillagerId, Recipe),
            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

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
    PlayerId = get(player_id),

    [Obj] = db:read(obj, ObjId),

    Checks = [{is_player_owned(PlayerId, Obj#obj.player), "Unit not owned by player"},
              {is_state(Obj#obj.state, none), "Unit is busy"}],

    case process_checks(Checks) of
        true ->
            lager:info("Resting"),
            obj:update_state(Obj, resting),
            
            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

hide(ObjId) ->
    PlayerId = get(player_id),
    [Obj] = db:read(obj, ObjId),

    Checks = [{is_player_owned(Obj#obj.player, PlayerId), "Unit is not owned by player"},
              {is_hero(Obj), "Can only ford with your hero"},
              {not game:has_pre_events(ObjId), "Unit is busy"},
              {Obj#obj.class =:= unit, "Obj cannot move"},
              {Obj#obj.state =/= dead, "Unit is dead"}],
 
    case process_checks(Checks) of
        true ->
            lager:info("Hiding"),
            obj:update_state(Obj, hiding),
            
            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.
      

assign(SourceId, TargetId) ->
    Player = get(player_id),
    
    SourceObj = obj:get(SourceId),
    TargetObj = obj:get(TargetId),

    Checks = [{is_player_owned(SourceObj, Player), "Source is not owned by player"},
              {is_player_owned(TargetObj, Player), "Target is not owned by player"},
              {obj:is_hero_nearby(TargetObj, Player), "Unit is not near Hero"},
              {obj:is_subclass(?VILLAGER, SourceObj), "Can only assign villagers"},
              {obj:is_subclass(?HARVESTER, TargetObj) or 
               obj:is_subclass(?CRAFT, TargetObj), "Can only assign to crafting or harvesting buildings"}],

    case process_checks(Checks) of
        true ->
            lager:info("Assigning villager"),
            villager:assign(SourceId, TargetId),
            villager:set_order_harvest(SourceId),
            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

follow(VillagerId) ->
    Player = get(player_id),
    VillagerObj = obj:get(VillagerId),

    Checks = [{is_player_owned(VillagerObj, Player), "Villager is not owned by player"},
              {obj:is_hero_nearby(VillagerObj, Player), "Villager is not near Hero"},
              {obj:is_subclass(?VILLAGER, VillagerObj), "Not a villager"}],

    case process_checks(Checks) of
        true ->
            lager:info("Villager following"),
            villager:set_order_follow(VillagerId),

            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

order_harvest(VillagerId) ->
    Player = get(player_id),
    VillagerObj = obj:get(VillagerId),
    
    StructureObj = case VillagerObj of
                       invalid -> nothing;
                       _ -> obj:get_assignable(VillagerObj#obj.pos)
                   end,

    Checks = [{is_player_owned(VillagerObj, Player), "Villager is not owned by player"},
              {is_player_owned(StructureObj, Player), "Invalid structure"},
              {obj:is_hero_nearby(VillagerObj, Player), "Villager is not near Hero"},
              {obj:is_subclass(?VILLAGER, VillagerObj), "Not a villager"}],

    case process_checks(Checks) of
        true ->
            lager:info("Villager harvest"),
            villager:assign(VillagerId, StructureObj#obj.id),
            villager:set_order_harvest(VillagerId),

            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end. 

order_attack(VillagerId, TargetId) ->
    Player = get(player_id),
    VillagerObj = obj:get(VillagerId),
    [TargetObj] = db:read(obj, TargetId),

    %TODO check if villager can see target, should not be able to order attack when not nearby
    Checks = [{is_player_owned(VillagerObj, Player), "Villager is not owned by player"},
              {obj:is_hero_nearby(VillagerObj, Player), "Villager is not near Hero"},
              {obj:is_subclass(?VILLAGER, VillagerObj), "Not a villager"},
              {combat:is_target_alive(TargetObj), "Target is dead"},
              {combat:is_targetable(TargetObj), "Cannot attack target"}],

    case process_checks(Checks) of
        true ->
            lager:info("Villager attack"),
            villager:set_order_attack(VillagerId),
            villager:set_target(VillagerId, TargetId),

            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.
    

clear(VillagerId) ->
    PlayerId = get(player_id),
    Villager = db:read(villager, VillagerId),
    NewVillager = Villager#villager {order = none},
    db:write(NewVillager).
    
cancel(SourceId) ->
    PlayerId = get(player_id),
    [Obj] = db:read(obj, SourceId),

    ValidOwner = PlayerId =:= Obj#obj.player,
    
    cancel_event(ValidOwner, SourceId).

revent_response(ResponseNum) ->
    PlayerId = get(player_id),
    Hero = obj:get_hero(PlayerId),
    REvent = revent:get(Hero#obj.id),

    obj:update_state(Hero#obj.id, none),

    lager:info("Apply revent effects"),
    {ResolutionText, EffectsText} = revent:apply_effect(Hero#obj.id, REvent, ResponseNum),

    #{<<"title">> => REvent#revent.title,
      <<"text">> => ResolutionText,
      <<"effects">> => EffectsText}.
%
%Internal functions
%

process_item_transfer(Item, TargetObj = #obj{id = Id, class = Class, state = State}) when Class =:= structure, State =/= none ->
    ReqList = obj_attr:value(Id, <<"req">>),
    
    F = fun(Req) ->
            ReqType = maps:get(<<"type">>, Req),
            ReqType =:= Item#item.subclass
        end,

    [First | _] = lists:filter(F, ReqList),
    ReqQuantity = maps:get(<<"quantity">>, First),
    ItemMap = item:transfer(Item#item.id, Id, ReqQuantity),
    obj:item_transfer(TargetObj, ItemMap);
process_item_transfer(Item, TargetObj) ->
    ItemMap = item:transfer(Item#item.id, TargetObj#obj.id),
    obj:item_transfer(TargetObj, ItemMap).

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

is_hero(Obj) -> Obj#obj.subclass =:= <<"hero">>.

is_same_pos(SourceObj, TargetObj) when (SourceObj =:= invalid) or (TargetObj =:= invalid) ->
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

is_structure_req(Item, #obj{id = Id, class = Class, state = State}) when Class =:= structure, State =/= none ->
    ReqList = obj_attr:value(Id, <<"req">>),

    F = fun(Req) ->
            ReqType = maps:get(<<"type">>, Req),
            ReqType =:= Item#item.subclass
        end,

    lists:any(F, ReqList);
is_structure_req(_Item, _Obj) -> true.

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
