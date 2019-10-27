% Author: Peter
% %% Created: Nov 11, 2014
% %% Description: Player module
-module(player).

-include("schema.hrl").
-include("common.hrl").

-export([perception/1,
         init_state/1,
         get_stats/1, 
         get_info_tile/1,
         get_info_unit/1,
         get_info_item/1,
         get_info_item_name/1,
         get_info_inventory/1,
         get_info_item_transfer/2,
         get_info_hauling/1,
         get_info_attrs/1,
         get_info_skills/1,
         move/2,
         ford/2,
         combo/2,
         attack/3,
         defend/2,
         survey/1,
         explore/1,
         harvest/2,
         loot/2,
         buy_item/2, 
         sell_item/3,
         hire/2, 
         item_transfer/2,
         item_split/2,
         structure_list/0,
         create_foundation/2,
         upgrade/1,
         build/2,
         recipe_list/1,
         order_refine/1,
         order_craft/2,
         equip/1,
         unequip/1,
         rest/1,
         hide/1,
         assign_list/0,
         assign/2,
         order_follow/1,
         order_explore/1,
         order_harvest/1,
         order_gather/2,
         order_attack/2,
         order_build/2,
         clear/1,
         cancel/1,
         revent_response/1,
         set_event_lock/2,
         process_checks/1,
         is_player_owned/2,
         is_player/1,
         is_online/1,
         is_class/2,
         set_player_online/1,
         get_conn/1]).

is_player(PlayerId) -> PlayerId > ?NPC_ID.

is_online(PlayerId) ->
    case is_player(PlayerId) of
        true ->
            case db:read(connection, PlayerId) of
                [Conn] -> 
                    case Conn#connection.status =:= online of
                        true -> Conn;
                        false -> false
                    end;
                _ -> 
                    false
            end;
        false ->
            false
    end.

is_class(Player, Class) when is_record(Player, player) ->
    Player#player.class =:= Class.

set_player_online(PlayerId) ->
    case db:read(connection, PlayerId) of
        [Conn] ->
            NewConn = Conn#connection{status = online},
            db:write(NewConn);
        _ ->
            nothing
    end.

get_conn(PlayerId) ->
    case db:read(connection, PlayerId) of
        [Conn] -> Conn;
        _ -> false
    end.

perception(PlayerId) ->
    Objs = perception:get_by_player(PlayerId),
    Map = map:get_explored(PlayerId, all),    

    #{<<"map">> => Map,
      <<"objs">> => Objs}.

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

get_info_tile(Pos) ->
    lager:info("info_tile"),
    Player = get(player_id),

    add_active_info(Pos, Player, Pos),

    case is_visible(Pos) of
        true ->
            game:get_info_tile(Pos);
        false ->
            #{<<"errmsg">> => "Cannot see that tile"}
    end.

get_info_unit(Id) ->
    [Unit] = db:read(obj, Id),
    Player = get(player_id),

    add_active_info({Player, obj, Id}, Player, Id),

    Info = case Unit#obj.player =:= Player of
        true -> 
            obj:trigger_inspect(Unit),
            obj:get_info(Id);
        false ->
            obj:trigger_inspect(Unit),
            obj:get_info_other(Id)
    end,

    Info.

get_info_item(ItemId) ->
    lager:info("get_info_item ~p", [ItemId]),
    case item:get_all_attr(ItemId) of
        invalid -> #{<<"errmsg">> => <<"Invalid Item">>};
        ItemMap -> ItemMap
    end.
get_info_item_name(ItemName) ->
    item:get_map_by_name(ItemName).

get_info_inventory(Id) ->
    [Obj] = db:read(obj, Id),
    Player = get(player_id),

    obj:trigger_inspect(Obj),
    Info = obj:get_info_inventory(Player, Obj),

    Info.

get_info_item_transfer(SourceId, TargetId) ->
    Player = get(player_id),
    [SourceObj] = db:read(obj, SourceId),
    [TargetObj] = db:read(obj, TargetId),

    obj:trigger_inspect(SourceObj),
    obj:trigger_inspect(TargetObj),

    SourceItems = obj:get_info_inventory(Player, SourceObj),
    TargetItems = obj:get_info_inventory(Player, TargetObj),

    Info = #{<<"sourceid">> => SourceId,
             <<"sourceitems">> => SourceItems,
             <<"targetid">> => TargetId,
             <<"targetitems">> => TargetItems},
    Info.

get_info_hauling(SourceId) ->
    Player = get(player_id),
    [SourceObj] = db:read(obj, SourceId),

    HaulingObjs = obj:get_info_hauling(Player, SourceObj),

    Info = #{<<"data">> => HaulingObjs},
    Info.

get_info_attrs(Id) ->
    [Obj] = db:read(obj, Id),
    Player = get(player_id),

    Info = case Player =:= Obj#obj.player of
                true ->
                    obj:get_info_attrs(Obj);
                false ->
                    #{<<"errmsg">> => "Obj is not owned by player"}
           end,
    Info.

get_info_skills(Id) ->
    [Obj] = db:read(obj, Id),
    Player = get(player_id),

    Info = case Player =:= Obj#obj.player of
                true ->
                    obj:get_info_skills(Obj);
                false ->
                    #{<<"errmsg">> => "Obj is not owned by player"}
           end,
    Info.

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
    lager:info("Move: ~p", [SourceId]),
    PlayerId = get(player_id),
    [Obj] = db:read(obj, SourceId),

    Checks = [{is_player_owned(Obj#obj.player, PlayerId), "Unit is not owned by player"},
              {is_hero(Obj), "Can only move your hero"},
              {not game:has_pre_events(SourceId), "Unit is busy"},
              {Obj#obj.class =:= unit, "Obj cannot move"},
              {Obj#obj.state =/= dead, "Unit is dead"}, 
              {map:is_adjacent(Obj#obj.pos, Pos), "Unit is not adjacent to position"},
              {map:is_passable(Pos, Obj), "Tile is not passable"},
              {not obj:is_subclass(?VILLAGER, Obj), "Cannot move villager"},
              {obj:is_empty(Obj, Pos), "Position is occupied"},
              {obj:is_hero_nearby(Obj, PlayerId), "Unit not near Hero"}],
              
    case process_checks(Checks) of
        true ->
            game:cancel_event(SourceId),

            SourcePos = Obj#obj.pos,
            DestPos = Pos,
            MoveTicks = obj:movement_cost(Obj, DestPos),

            %Add obj update state to change to moving state on next tick
            game:add_obj_update(self(), SourceId, ?STATE, ?MOVING),
                
            %Add obj move event to execute in MoveTicks
            game:add_obj_move(self(), SourceId, SourcePos, DestPos, MoveTicks),

            #{<<"move_time">> => MoveTicks * ?TICKS_SEC};
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

explore(ObjId) ->
    lager:info("Explore: ~p", [ObjId]),
    PlayerId = get(player_id),
    [Obj] = db:read(obj, ObjId),
    
    Checks = [{is_player_owned(Obj#obj.player, PlayerId), "Unit is not owned by player"},
              {not game:has_pre_events(ObjId), "Unit is busy"}],

    case process_checks(Checks) of
        true ->
            game:add_obj_update(self(), ObjId, ?STATE, ?EXPLORING),

            NumTicks = 12,

            lager:info("Adding explore event"),
            EventData = ObjId,
            game:add_event(self(), explore, EventData, ObjId, NumTicks),
            
            #{<<"explore_time">> => NumTicks * ?TICKS_SEC};
        {false, Error} ->
            lager:info("Exploring error: ~p", [Error]),
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

harvest(ObjId, Resource) ->
    PlayerId = get(player_id),

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
            NumTicks = 20,
        
            %Update obj state
            game:add_obj_update(self(), ObjId, ?STATE, ?HARVESTING),

            %Check for encounter
            encounter:check(Obj#obj.pos),

            EventData = {ObjId, Resource, Obj#obj.pos, NumTicks, AutoHarvest},
            game:add_event(self(), harvest, EventData, ObjId, NumTicks),

            #{<<"harvest_time">> => NumTicks * ?TICKS_SEC};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

is_valid_buysell(TargetId, ItemId, Quantity) ->
    case item:has_price(ItemId) of
        true ->
            TargetGold = item:total_gold(TargetId),
            {TargetGold < (item:price(ItemId) * Quantity), "Insufficent gold"};
        false ->
            {false, "Item is not for sale"}
    end.

buy_item(ItemId, Quantity) ->
    PlayerId = get(player_id),
    Hero = obj:get_hero(PlayerId),

    Item = item:get_rec(ItemId),
    lager:info("Item: ~p", [Item]),
    ItemOwnerId = item:owner(Item),
    ItemOwner = obj:get(ItemOwnerId),

    Checks = [{map:is_adjacent(Hero, ItemOwner), "Item is not nearby"},
              {Quantity =< item:quantity(Item), "Purchase quantity too large"},
              is_valid_buysell(obj:id(Hero), ItemId, Quantity)],

    case process_checks(Checks) of
        true -> 
            %Transfer gold
            item:transfer_by_class(obj:id(Hero), 
                                   ItemOwnerId, 
                                   ?GOLD_COINS, 
                                   item:price(ItemId) * Quantity),

            %Transfer purchased item
            ItemMap = item:transfer(Item#item.id, obj:id(Hero), Quantity),

            %Trigger object transfer hooks
            obj:item_transfer(Hero, ItemMap),

            SourceItems = obj:get_info_inventory(PlayerId, Hero),
            TargetItems = obj:get_info_inventory(PlayerId, ItemOwner),

            #{<<"result">> => <<"success">>,
              <<"sourceid">> => obj:id(Hero),
              <<"sourceitems">> => SourceItems,
              <<"targetid">> => ItemOwnerId,
              <<"targetitems">> => TargetItems};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

sell_item(ItemId, TargetId, Quantity) ->
    PlayerId = get(player_id),
    Hero = obj:get_hero(PlayerId),
    Target = obj:get(TargetId),

    Item = item:get_rec(ItemId),

    Checks = [{map:is_adjacent(Hero, Target), "Target is not nearby"},
              {Quantity =< item:quantity(Item), "Sell quantity too large"},
              is_valid_buysell(TargetId, ItemId, Quantity)],

    case process_checks(Checks) of
        true -> 
            %Transfer gold
            item:transfer_by_class(TargetId, 
                                   obj:id(Hero), 
                                   ?GOLD_COINS, 
                                   item:price(ItemId) * Quantity),

            %Transfer purchased item
            ItemMap = item:transfer(Item#item.id, TargetId, Quantity),

            %Trigger object transfer hooks
            obj:item_transfer(TargetId, ItemMap),

            SourceItems = obj:get_info_inventory(PlayerId, Hero),
            TargetItems = obj:get_info_inventory(PlayerId, Target),

            lager:info("~p", [SourceItems]),
            lager:info("~p", [TargetItems]),

            #{<<"result">> => <<"success">>,
              <<"sourceid">> => obj:id(Hero),
              <<"sourceitems">> => SourceItems,
              <<"targetid">> => TargetId,
              <<"targetitems">> => TargetItems};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

is_hire_valid(Hero, TargetId) ->
    case obj:has_wage(TargetId) of
        true ->
            HeroGold = item:total_gold(obj:id(Hero)),
            {HeroGold < obj:wage(TargetId), "Insufficient gold"};
        false ->
            {false, "Target is not for hire"}
    end.

hire(MerchantId, TargetId) ->
    PlayerId = get(player_id),
    Hero = obj:get_hero(PlayerId),
    Merchant = obj:get(MerchantId),

    Checks = [{map:is_adjacent(Hero, Merchant), "Merchant is not nearby"},
              {obj:is_hauling(MerchantId, TargetId), "Target is not being hauled"},
              is_hire_valid(Hero, TargetId)],

    case process_checks(Checks) of
        true ->
            %Transfer gold
            item:transfer_by_class(obj:id(Hero), 
                                   MerchantId, 
                                   ?GOLD_COINS, 
                                   obj:wage(TargetId)),

            %Remove obj from merchant
            obj:unload(MerchantId, TargetId),

            %Transfer obj to player
            obj:transfer(TargetId, PlayerId),

            %Move obj to hero location
            game:add_obj_move(self(), TargetId, {-50, -50}, obj:pos(Hero), 4),

            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

loot(SourceId, ItemId) ->
    Item = item:get_rec(ItemId),
    %Will fail if item id is invalid
    Owner = Item#item.owner,

    item:transfer(ItemId, SourceId),

    Items = item:get_by_owner(Owner),
    {Owner, Items}.

item_transfer(TargetId, ItemId) ->
    Player = get(player_id),
    %Will fail if item id is invalid
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

            SourceItems = obj:get_info_inventory(Player, OwnerObj),
            TargetItems = obj:get_info_inventory(Player, TargetObj),

            #{<<"result">> => <<"success">>,
              <<"sourceid">> => Owner,
              <<"sourceitems">> => SourceItems,
              <<"targetid">> => TargetId,
              <<"targetitems">> => TargetItems};
       {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

item_split(ItemId, Quantity) ->
    Player = get(player_id),

    Checks = [{item:is_valid_split(Player, ItemId, Quantity), "Cannot split item"}],

    case process_checks(Checks) of
        true ->
            lager:info("Splitting item"),
            SourceItem = item:split(ItemId, Quantity),
            #{<<"result">> => <<"success">>,
              <<"owner">> => item:owner(SourceItem)};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

structure_list() ->
    structure:list().

create_foundation(BuilderId, StructureName) ->
    PlayerId = get(player_id),
    lager:info("PlayerId: ~p", [PlayerId]),

    Builder = obj:get(BuilderId),
    lager:info("Builder: ~p", [Builder]),
    StructureSubclass = obj_template:value(StructureName, <<"subclass">>),

    Checks = [{is_player_owned(Builder, PlayerId), "Builder not owned by player"},              
              {structure:valid_location(StructureSubclass, Builder#obj.pos), "Invalid structure location"},
              {Builder#obj.state =:= ?NONE, "Builder is busy"}],

    lager:info("Checks: ~p", [Checks]),
    case process_checks(Checks) of
        true ->
            lager:info("Building structure"),

            structure:create_foundation(PlayerId, 
                                        Builder#obj.pos, 
                                        StructureName),
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
  
build(BuilderId, StructureId) ->
    PlayerId = get(player_id),
    [Structure] = db:read(obj, StructureId),

    lager:info("Structure state: ~p", [Structure#obj.state]),

    build(PlayerId, BuilderId, Structure).

build(PlayerId, BuilderId, Structure = #obj {state = ?FOUNDED}) -> 
    [Builder] = db:read(obj, BuilderId),
    
    Checks = [{obj:pos(Builder) =:= obj:pos(Structure), "Builder must be on the structure"},
              {Structure#obj.player =:= PlayerId, "Structure not owned by player"},
              {structure:has_req(Structure#obj.id), "Structure is missing required items"}],

    case process_checks(Checks) of
        true ->
            lager:info("Finishing structure"),
            game:cancel_event(BuilderId),

            %Get the build time and calculate the end time
            BuildTimeTicks = obj_attr:value(Structure#obj.id, <<"build_time">>),

            Start = game:get_tick(),
            End = Start + BuildTimeTicks,

            obj_attr:set(obj:id(Structure), <<"start_time">>, Start),
            obj_attr:set(obj:id(Structure), <<"end_time">>, End),
            obj_attr:set(obj:id(Structure), <<"builder">>, BuilderId),
            obj_attr:set(obj:id(Structure), <<"progress">>, 0),

            EventData = {BuilderId, Structure#obj.id},

            %Add obj update state to change to moving state on next tick
            game:add_obj_update(self(), BuilderId, ?STATE, ?BUILDING),
            game:add_obj_update(self(), obj:id(Structure), ?STATE, ?PROGRESSING),

            game:add_event(self(), build, EventData, BuilderId, BuildTimeTicks),

            #{<<"build_time">> => trunc(BuildTimeTicks / ?TICKS_SEC)};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end;

build(PlayerId, BuilderId, Structure = #obj {state = ?STALLED}) ->
    [Builder] = db:read(obj, BuilderId),
    
    Checks = [{Builder#obj.pos =:= Structure#obj.pos, "Builder must be on the structure"},
              {Structure#obj.player =:= PlayerId, "Structure not owned by player"}],

    case process_checks(Checks) of
        true ->
            lager:info("Finishing structure"),
            game:cancel_event(BuilderId),

            %Get the build time and calculate the end time
            BuildTimeTicks = obj_attr:value(obj:id(Structure), <<"build_time">>),
            Progress = obj_attr:value(obj:id(Structure), <<"progress">>),

            Start = game:get_tick(),
            End = Start + (BuildTimeTicks * (1 - Progress)),

            obj_attr:set(obj:id(Structure), <<"start_time">>, Start),
            obj_attr:set(obj:id(Structure), <<"end_time">>, End),

            EventData = {BuilderId, Structure#obj.id},

            game:add_obj_update(self(), BuilderId, ?STATE, ?BUILDING),
            game:add_obj_update(self(), obj:id(Structure), ?STATE, ?PROGRESSING),

            game:add_event(self(), build, EventData, BuilderId, BuildTimeTicks),

            #{<<"build_time">> => trunc(BuildTimeTicks / ?TICKS_SEC)};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end;

build(_PlayerId, _SourceId, Structure) ->
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

order_refine(Structure) when is_record(Structure, obj) ->
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
            villager:set_order(VillagerId, ?ORDER_REFINE),
            #{<<"result">> => <<"success">>};
        {false, Error} ->
            lager:info("Refine failed: ~p", [Error]),
            #{<<"errmsg">> => list_to_binary(Error)}
    end;
order_refine(StructureId) ->
    Structure = obj:get(StructureId),
    order_refine(Structure).

order_craft(StructureId, Recipe) ->
    Player = get(player_id),
    [Structure] = db:read(obj, StructureId),

    Checks = [{not is_event_locked(StructureId), "Event in process"},
              {Player =:= Structure#obj.player, "Structure not owned by player"},
              {villager:has_assigned(StructureId), "Missing assigned villager to structure"},
              {structure:check_recipe_req(StructureId, Recipe), "Missing recipe requirements"}],

    case process_checks(Checks) of
        true ->
            lager:info("Craft process_checks success"),
            VillagerId = villager:get_by_structure(StructureId),
            villager:set_order(VillagerId, ?ORDER_CRAFT, #{recipe => Recipe}),
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

assign_list() ->
    Player = get(player_id),
    lager:info("Assign list"),
    villager:assign_list(Player).

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

            case obj:subclass(TargetObj) of
                ?HARVESTER -> villager:set_order(SourceId, ?ORDER_HARVEST);
                ?CRAFT ->  villager:set_order(SourceId, ?ORDER_REFINE);
                _ -> none
            end,

            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

order_follow(VillagerId) ->
    Player = get(player_id),
    VillagerObj = obj:get(VillagerId),

    Checks = [{is_player_owned(VillagerObj, Player), "Villager is not owned by player"},
              {obj:is_hero_nearby(VillagerObj, Player), "Villager is not near Hero"},
              {obj:is_subclass(?VILLAGER, VillagerObj), "Not a villager"}],

    case process_checks(Checks) of
        true ->
            lager:info("Villager following"),
            villager:set_order(VillagerId, ?ORDER_FOLLOW),

            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

order_explore(VillagerId) ->
    Player = get(player_id),
    VillagerObj = obj:get(VillagerId),
    
    Checks = [{is_player_owned(VillagerObj, Player), "Villager is not owned by player"},
              {obj:is_hero_nearby(VillagerObj, Player), "Villager is not near Hero"},
              {obj:is_subclass(?VILLAGER, VillagerObj), "Not a villager"}],

    case process_checks(Checks) of
         true ->
            lager:info("Villager explore"),
            villager:set_order(VillagerId, ?ORDER_EXPLORE),

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
            villager:set_order(VillagerId, ?ORDER_HARVEST),

            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end. 

order_gather(VillagerId, ResourceType) ->
    Player = get(player_id),
    VillagerObj = obj:get(VillagerId),

    Checks = [{is_player_owned(VillagerObj, Player), "Villager is not owned by player"},
              {obj:is_hero_nearby(VillagerObj, Player), "Villager is not near Hero"},
              {obj:is_subclass(?VILLAGER, VillagerObj), "Not a villager"},
              {resource:is_valid_type(ResourceType, obj:pos(VillagerObj)), "Not valid resource type"}],
 
    case process_checks(Checks) of
        true ->
            lager:info("Villager gather"),
            OrderData = #{gatherpos => obj:pos(VillagerObj),
                          restype => ResourceType},           
            villager:set_order(VillagerId, ?ORDER_GATHER, OrderData),

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
            villager:set_order(VillagerId, ?ORDER_ATTACK),
            villager:set_target(VillagerId, TargetId),

            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.
    
order_build(VillagerId, StructureId) ->
    Player = get(player_id),
    VillagerObj = obj:get(VillagerId),
    StructureObj = obj:get(StructureId),

    Checks = [{StructureObj =:= invalid, "Invalid structure"},
              {obj:pos(VillagerObj) =:= obj:pos(StructureObj), "Builder must be on the structure"},
              {obj:player(StructureObj) =:= Player, "Structure not owned by player"},
              {structure:has_req(obj:id(StructureObj)), "Structure is missing required items"}],

    case process_checks(Checks) of
        true ->
            OrderData = #{structure => StructureId},           
            villager:set_order(VillagerId, ?ORDER_BUILD, OrderData),

            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.

clear(VillagerId) ->
    PlayerId = get(player_id),
    VillagerObj = obj:get(VillagerId),


    Checks = [{VillagerObj =:= invalid, "Invalid villager"},
              {obj:player(VillagerObj) =:= PlayerId, "Villager not owned by player"}],

    case process_checks(Checks) of
        true ->
            villager:set_order(VillagerId, none),
            #{<<"result">> => <<"success">>};
        {false, Error} ->
            #{<<"errmsg">> => list_to_binary(Error)}
    end.
    
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

%TODO move to some other module
process_item_transfer(Item, TargetObj = #obj{id = Id, class = Class, state = State}) when Class =:= structure, State =/= none ->
    ReqList = obj_attr:value(Id, <<"req">>),
    
    F = fun(Req) ->
            ReqType = maps:get(<<"type">>, Req),
            MatchSubClass = ReqType =:= Item#item.subclass,
            MatchClass = ReqType =:= Item#item.class,
            MatchSubClass or MatchClass
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

%TODO move to structure module
is_structure_req(Item, #obj{id = Id, class = Class, state = State}) when Class =:= structure, State =/= none ->
    ReqList = obj_attr:value(Id, <<"req">>),

    F = fun(Req) ->
            ReqType = maps:get(<<"type">>, Req),
            MatchSubClass = ReqType =:= Item#item.subclass,
            MatchClass = ReqType =:= Item#item.class,
            MatchSubClass or MatchClass
        end,

    lists:any(F, ReqList);
is_structure_req(_Item, _Obj) -> true.

is_visible(Pos) ->
    Player = get(player_id),
    Units = db:index_read(obj, Player, #obj.player),

    F = fun(Unit) ->
            Distance = map:distance(Unit#obj.pos, Pos),
            Distance =< Unit#obj.vision
        end,

    lists:any(F, Units).

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

add_active_info(Index, Player, Id) ->
    ActiveInfo = #active_info{index = Index,
                              player = Player,
                              id = Id},
    db:write(ActiveInfo).

delete_active_info(Index) ->
    db:delete(active_info, Index).

