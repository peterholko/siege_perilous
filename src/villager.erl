%% -------------------------------------------------------------------
%% Author  : Peter Holko
%%% Description : Villager Manager server
%%%
%%% Created : Aug 6, 2015
%%% -------------------------------------------------------------------
-module(villager).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([has_assigned/1, has_assigned/2, assign/2, remove_structure/1, get_by_structure/1]).
-export([process/1, set_behavior/2]).
-export([enemy_visible/1, move_to/2, move_to_pos/1, move_random_pos/1, hero_nearby/1]).
-export([set_pos_shelter/1, set_pos_hero/1, set_pos_structure/1, set_pos_gather/1]).
-export([morale_normal/1, morale_low/1, morale_very_low/1]).
-export([set_order/2, set_order/3, set_target/2]).
-export([structure_needed/1, shelter_needed/1, storage_needed/1, harvest/1]).
-export([has_shelter/1, assigned_harvester/1, assigned_craft/1, has_storage/1]).
-export([free_structure/1, free_harvester/1, free_craft/1, free_shelter/1, free_storage/1]).
-export([find_shelter/1, find_harvester/1, find_craft/1, find_storage/1]).
-export([structure_not_full/1, storage_not_full/1, load_resources/1, unload_resources/1]).
-export([set_pos_storage/1, set_hauling/1, set_none/1, not_hauling/1]).
-export([set_activity/2]).
-export([has_resources/1, has_food/1, has_food_storage/1, find_food_storage/1, transfer_food/1,
         has_water/1, has_water_storage/1, find_water_storage/1, transfer_water/1, has_tools/1]).
-export([idle/1, explore/1, gather/1, build/1, refine/1, craft/1, experiment/1, eat/1, drink/1, sleep/1]).
-export([move_to_target/1, melee_attack/1, is_full/1]).
-export([has_order_follow/1, has_order_attack/1, has_order_guard/1, has_order_harvest/1, 
         has_order_refine/1, has_order_craft/1, has_order_experiment/1]).
-export([has_order/2, has_effect_any/2, has_effect/2, has_not_effect/2]).
-export([tile_has_unrevealed/1]).
-export([create/1, create/3, remove/1, assign_list/1, info/1]).
-export([id/1, player/1, order/1, action/1, morale/1, shelter/1, structure/1, storage/1]).

%% ====================================================================
%% External functions
%% ====================================================================
%% Call %%
start() ->
    gen_server:start({global, villager}, villager, [], []).

create(Level) ->
    %Offmap position and not real playerid    
    PlayerId = -9999, 
    OffMapPos = {-9999, 9999},
    gen_server:call({global, villager}, {create, Level, PlayerId, OffMapPos}).

create(Level, Player, Pos) ->
    gen_server:call({global, villager}, {create, Level, Player, Pos}).

assign_list(Player) ->
    gen_server:call({global, villager}, {assign_list, Player}).

info(VillagerId) ->
    gen_server:call({global, villager}, {info, VillagerId}).

has_assigned(StructureId) ->
    gen_server:call({global, villager}, {has_assigned, StructureId}).
has_assigned(StructureId, VillagerId) ->
    gen_server:call({global, villager}, {has_assigned, StructureId, VillagerId}).

get_by_structure(StructureId) ->
    gen_server:call({global, villager}, {get_by_structure, StructureId}).


%%% Cast %%%
remove(Id) ->
    gen_server:cast({global, villager}, {remove, Id}).

process(Tick) ->
    gen_server:cast({global, villager}, {process, Tick}).

set_behavior(VillagerId, Behavior) ->
    gen_server:cast({global, villager}, {set_behavior, VillagerId, Behavior}).

assign(VillagerId, TargetId) ->
    gen_server:cast({global, villager}, {assign, VillagerId, TargetId}).

set_order(VillagerId, Order) ->
    set_order(VillagerId, Order, #{}).
set_order(VillagerId, Order, Data) ->
    gen_server:cast({global, villager}, {set_order, VillagerId, Order, Data}).

set_target(VillagerId, TargetId) ->
    gen_server:cast({global, villager}, {set_target, VillagerId, TargetId}).

remove_structure(StructureId) ->
    gen_server:cast({global, villager}, {remove_structure, StructureId}).

id(Villager = #villager{}) -> Villager#villager.id.
player(Villager = #villager{}) -> Villager#villager.player.
order(Villager = #villager{}) -> Villager#villager.order.
morale(Villager = #villager{}) -> Villager#villager.morale.

shelter(Villager = #villager{}) -> 
    case Villager#villager.shelter of
        none -> <<"none">>;        
        {_Id, _Pos, Name} -> Name            
    end.

structure(Villager = #villager{}) -> 
    case Villager#villager.structure of
        none -> <<"none">>;        
        {_Id, _Pos, Name} -> Name            
    end.
storage(Villager = #villager{}) -> 
    case Villager#villager.storage of
        none -> <<"none">>;        
        {_Id, _Pos, Name} -> Name
    end. 

action(#villager{activity = Activity}) ->
    Activity.

%action(#villager{task_index = TaskIndex, plan = Plan}) when TaskIndex > 0 ->
%    case lists:nth(TaskIndex, Plan) of
%        {move_to, pos_hero} -> <<"moving to hero">>;
%        Task -> atom_to_binary(Task, latin1)
%    end;
%action(_) -> <<"idle">>.
        

%%%
%%% HTN Conditions %%%
%%%

enemy_visible(Villager) ->
    Villager#villager.enemies =/= [].

has_shelter(Villager) ->
    Villager#villager.shelter =/= none.

assigned_harvester(Villager) ->
    {Id, _Pos, _Name} = Villager#villager.structure,
    case obj:get(Id) of
        invalid -> false;
        Obj -> 
            case obj:subclass(Obj) of
                <<"resource">> -> true;
                _ -> false
            end
    end.

assigned_craft(Villager) ->
    {Id, _Pos, _Name} = Villager#villager.structure,
    case obj:get(Id) of
        invalid -> false;
        Obj -> 
            case obj:subclass(Obj) of
                <<"craft">> -> true;
                _ -> false
            end
    end.

has_storage(Villager) ->
    Villager#villager.storage =/= none.

hero_nearby(_Id) ->
    true.
    %[VillagerObj] = db:read(obj, Id),
    %TODO check if villager has a hero
    %case obj:get_hero(VillagerObj#obj.player) of
    %    false -> 
    %        false;
    %    [Hero] ->
    %        map:distance(VillagerObj#obj.pos, Hero#obj.pos) =< 3
    %end.

morale_normal(Villager) ->
    morale(Villager, 50).
morale_low(Villager) ->
    morale(Villager, 25).
morale_very_low(Villager) ->
    morale(Villager, 0).

morale(Villager, Value) ->
    Villager#villager.morale >= Value.

has_order(Villager, OrderType) ->
    Villager#villager.order =:= OrderType.

has_effect_any(Villager, EffectTypeList) ->
    F = fun(EffectType) ->
            effect:has_effect(Villager#villager.id, EffectType)
        end,

    lists:any(F, EffectTypeList).

has_effect(Villager, EffectType) ->
    Result = effect:has_effect(Villager#villager.id, EffectType),
    lager:debug("Villager: ~p, Has ~p: ~p", [Villager#villager.id, EffectType, Result]),
    Result.

has_not_effect(Villager, EffectType) ->
    [VillagerObj] = db:read(obj, Villager#villager.id),
    not effect:has_effect(VillagerObj#obj.id, EffectType).

has_order_follow(Villager) ->
    Villager#villager.order =:= ?ORDER_FOLLOW.

has_order_guard(Villager) ->
    Villager#villager.order =:= ?ORDER_GUARD.

has_order_attack(Villager) ->
    Villager#villager.order =:= ?ORDER_ATTACK.

has_order_harvest(Villager) ->
    Villager#villager.order =:= ?ORDER_HARVEST.

has_order_refine(Villager) ->
    Villager#villager.order =:= ?ORDER_REFINE.

has_order_craft(Villager) ->
    Villager#villager.order =:= ?ORDER_CRAFT.

has_order_experiment(Villager) ->
    Villager#villager.order =:= ?ORDER_EXPERIMENT.

shelter_needed(Villager) ->
    Villager#villager.shelter =:= none.

structure_needed(Villager) ->
    Villager#villager.structure =:= none.

storage_needed(Villager) ->
    Villager#villager.storage =:= none.

free_structure(Villager) -> free_structure(Villager, structure, [{subclass, [<<"resource">>, <<"craft">>]}]).
free_harvester(Villager) -> free_structure(Villager, structure, [{subclass, <<"resource">>}]).
free_craft(Villager) -> free_structure(Villager, structure, [{subclass, <<"craft">>}]).
free_shelter(Villager) -> free_structure(Villager, shelter, [{subclass, <<"shelter">>}]).
free_storage(Villager) -> free_structure(Villager, storage, [{subclass, <<"storage">>}]).
                        
free_structure(Villager, StructureClass, StructureTypes) ->    
    ClaimedList = case StructureClass of
                      structure -> get_claimed_structures(Villager#villager.player);
                      shelter -> get_claimed_shelters(Villager#villager.player);
                      storage -> [] % Cannot claim storage
                  end,

    StateComplete = [{state, none}],    
    Structures = obj:get_by_attr(Villager#villager.player, StructureTypes ++ StateComplete),

    lager:debug("ClaimedList: ~p", [ClaimedList]),

    F = fun(Structure, Acc) -> 
            FreeStructure = not lists:member(Structure#obj.id, ClaimedList),
            FreeStructure or Acc 
        end,

    Result = lists:foldl(F, false, Structures),
    lager:debug("free_structure: ~p ~p", [StructureTypes, Result]),
    Result.

structure_not_full(Villager) ->
    {StructureId, _StructurePos, _StructureName} = Villager#villager.structure,
    Capacity = obj:get_capacity(StructureId),
    
    %Assume "full" is 85% of capacity or over
    %and if 15% of capacity cannot fit, structure is full
    Capacity15 = erlang:trunc(Capacity * 0.15),

    obj:has_space(StructureId, Capacity15).

storage_not_full(Villager) ->
    {StorageId, _StoragePos, _StorageName} = Villager#villager.storage,
    Capacity = obj:get_capacity(StorageId),
    
    %Assume "full" is 85% of capacity or over
    %and if 15% of capacity cannot fit, structure is full
    Capacity15 = erlang:trunc(Capacity * 0.10),

    obj:has_space(StorageId, Capacity15).


not_hauling(Villager) ->
    Villager#villager.activity =/= hauling.

has_resources(Villager) ->
    case Villager#villager.storage =/= none of
        true -> true;
        false -> false
    end.

has_food(Villager) ->
    item:has_by_class(Villager#villager.id, ?FOOD).

has_food_storage(Villager) ->
    find_stored_items_by_class(Villager, ?FOOD) =/= [].

has_water(Villager) ->
    item:has_by_class(Villager#villager.id, ?WATER).

has_water_storage(Villager) ->
    find_stored_items_by_class(Villager, ?WATER) =/= [].

is_full(Villager) ->
    Capacity = obj:get_capacity(Villager#villager.id),
    
    %Assume "full" is 85% of capacity or over
    %and if 15% of capacity cannot fit, structure is full
    Capacity15 = erlang:trunc(Capacity * 0.15),

    %If obj does not have space for 15% it is hence full
    not obj:has_space(Villager#villager.id, Capacity15).


tile_has_unrevealed(Villager) ->
    VillagerObj = obj:get(Villager#villager.id),
    NumUnrevealed = resource:get_num_unrevealed(obj:pos(VillagerObj)),
    NumUnrevealed =/= 0.

has_tools(Villager) ->
    %Assume order gather for now, TODO expand to other orders
    
    ResType = maps:get(restype, Villager#villager.data),

    case ResType of
        ?ORE -> item:has_by_subclass(Villager#villager.id, ?PICK_AXE);
        ?WOOD -> item:has_by_subclass(Villager#villager.id, ?CHOPPING_AXE);
        ?STONE -> item:has_by_subclass(Villager#villager.id, ?CHISEL);
        ?GAME -> item:has_by_subclass(Villager#villager.id, ?HUNTING_KNIFE);
        ?WATER -> item:has_by_subclass(Villager#villager.id, ?WATER_FLASK);
        ?PLANT -> item:has_by_subclass(Villager#villager.id, ?GATHERING_KIT);
        _ -> false
   end. 

%%% 
%%% HTN Primitive Tasks
%%%

find_shelter(Villager) ->
    Shelter = find_structure(Villager, <<"shelter">>),
    Villager#villager {shelter = {Shelter#obj.id, 
                                  Shelter#obj.pos, 
                                  Shelter#obj.name},
                       dest = Shelter#obj.pos,
                       task_state = completed}.

find_harvester(Villager) ->
    Harvester = find_structure(Villager, <<"resource">>),
    Villager#villager {structure = {Harvester#obj.id, 
                                    Harvester#obj.pos, 
                                    Harvester#obj.name},
                       dest = Harvester#obj.pos,
                       task_state = completed}.

find_craft(Villager) ->
    Craft = find_structure(Villager, <<"craft">>),
    Villager#villager {structure = {Craft#obj.id, 
                                    Craft#obj.pos,
                                    Craft#obj.name},
                       dest = Craft#obj.pos,
                       task_state = completed}.

find_storage(Villager) ->
    Storage = find_structure(Villager, <<"storage">>),
    Villager#villager {storage = {Storage#obj.id, 
                                  Storage#obj.pos,
                                  Storage#obj.name},
                       dest = Storage#obj.pos,
                       task_state = completed}.

find_water_storage(Villager) -> find_storage_by_item_class(Villager, ?WATER).
find_food_storage(Villager) -> find_storage_by_item_class(Villager, ?FOOD).

find_storage_by_item_class(Villager, Class) ->
    [Item | _Rest] = find_stored_items_by_class(Villager, Class),
    OwnerObj = obj:get(item:owner(Item)),
    OwnerPos = obj:pos(OwnerObj),

    Villager#villager{storage = obj:id(OwnerObj),
                      dest = OwnerPos,
                      task_state = completed}.



set_pos_shelter(Villager) ->
    {_Id, Pos, _Name} = Villager#villager.shelter,
    Villager#villager {dest = Pos, task_state = completed}.

set_pos_structure(Villager) ->
    lager:debug("Villager: ~p", [Villager]),
    {_Id, Pos, _Name} = Villager#villager.structure,
    Villager#villager {dest = Pos, task_state = completed}.

set_pos_storage(Villager) ->
    {_Id, Pos, _Name} = Villager#villager.storage,
    Villager#villager {dest = Pos, task_state = completed}.

set_pos_hero(Villager) ->
    [VillagerObj] = db:read(obj, Villager#villager.id),
    Hero = obj:get_hero(VillagerObj#obj.player),
    
    Villager#villager {dest = Hero#obj.pos, task_state = completed}.

set_pos_gather(Villager) ->
    GatherPos = maps:get(gatherpos, Villager#villager.data),
    Villager#villager {dest = GatherPos, task_state = completed}.

set_hauling(Villager) ->
    Villager#villager {activity = hauling, task_state = completed}.

set_none(Villager) ->
    Villager#villager {activity = none, task_state = completed}.

get_pos(Villager, pos_hero) -> 
    HeroObj = obj:get_hero(Villager#villager.player),
    obj:pos(HeroObj);
get_pos(Villager, pos_gather) ->
    GatherPos = maps:get(gatherpos, Villager#villager.data),
    GatherPos.

set_activity(Villager, NewActivity) ->
    FinalVillager = case Villager#villager.activity =:= NewActivity of
                      true -> 
                            Villager#villager{task_state = completed};
                      false ->
                            NewVillager = Villager#villager{activity = NewActivity,
                                                            task_state = completed},
                            game:send_villager_change(NewVillager),
                            NewVillager
                  end,
    FinalVillager.

move_to(Villager, Pos) ->
    [VillagerObj] = db:read(obj, Villager#villager.id),

    Dest = get_pos(Villager, Pos),
    NewVillager = Villager#villager{dest = Dest},

    %If dest is set and dest does not equal villager current pos
    FinalVillager = case (Dest =/= none) and (Dest =/= VillagerObj#obj.pos) of
                        true ->
                            PathResult = astar:astar(VillagerObj#obj.pos, Dest, VillagerObj),

                            case PathResult of
                                {success, Path} -> 
                                    %Move to next path location
                                    move_unit(VillagerObj, lists:nth(2, Path)),
                                    NewVillager#villager {task_state = running, path = Path};
                                {nearby, _Dist, _Closest} ->
                                    NewVillager#villager {task_state = completed};
                                {failed, _} -> 
                                    %No path, move task completed
                                    NewVillager#villager {task_state = completed}
                            end;
                        false ->
                            lager:debug("Dest: ~p Pos: ~p", [Dest, VillagerObj#obj.pos]),
                            NewVillager#villager {task_state = skipped}
                 end,
    FinalVillager.

move_to_pos(Villager) ->
    [VillagerObj] = db:read(obj, Villager#villager.id),

    Dest = Villager#villager.dest,

    %If dest is set and dest does not equal villager current pos
    NewVillager = case (Dest =/= none) and (Dest =/= VillagerObj#obj.pos) of
                    true ->
                        PathResult = astar:astar(VillagerObj#obj.pos, Dest, VillagerObj),

                        case PathResult of
                             {success, Path} ->
                                 %Move to next path location
                                 move_unit(VillagerObj, lists:nth(2, Path)),
                                 Villager#villager {task_state = running, path = Path};
                             {nearby, _Dist, _Closest} ->
                                 %No path, move task completed
                                 Villager#villager {task_state = completed};
                             {failed, _} -> 
                                 %No path, move task completed
                                 Villager#villager {task_state = completed}
                        end;
                    false ->
                        lager:debug("Dest: ~p Pos: ~p", [Dest, VillagerObj#obj.pos]),
                        Villager#villager {task_state = completed}
                  end,
    NewVillager.

move_random_pos(Villager) ->
    [VillagerObj] = db:read(obj, Villager#villager.id),

    NewVillager = case game:get_valid_tiles(VillagerObj#obj.pos, VillagerObj) of
                      [] -> 
                          Villager;
                      Tiles -> 
                          Random = util:rand(length(Tiles)),
                          lager:info("Move randomly: ~p", [Random]),
                          TilePos = lists:nth(Random, Tiles),
                          
                          move_unit(VillagerObj, TilePos),
                          Villager#villager {task_state = running}
                  end,
    NewVillager.

move_to_target(Villager) ->
    [VillagerObj] = db:read(obj, Villager#villager.id),

    NewVillager = case Villager#villager.target of
                      none ->
                          %Invalid target due to either moving out of range or dying
                          Villager#villager {task_state = completed};
                      _ -> 
                          [TargetObj] = db:read(obj, Villager#villager.target),
                            
                          IsAdjacent = map:is_adjacent(VillagerObj#obj.pos, TargetObj#obj.pos),

                          case IsAdjacent of
                              false ->
                                  %Assume path is found due to adjacent obj
                                  {success, Path} = astar:astar(VillagerObj#obj.pos, TargetObj#obj.pos, VillagerObj),

                                  move_next_path(VillagerObj, Path),
                                  Villager#villager {path = Path,
                                                     task_state = completed};
                              true ->
                                  Villager#villager {task_state = completed}
                          end
                  end,

    NewVillager.


melee_attack(Villager) ->
    [VillagerObj] = db:read(obj, Villager#villager.id),

    TargetObj = combat:is_valid_target(Villager#villager.target),

    Checks = TargetObj =/= false andalso
             map:is_adjacent(VillagerObj#obj.pos, TargetObj#obj.pos) andalso
             combat:is_target_alive(TargetObj) andalso
             combat:is_targetable(TargetObj),

    lager:info("Villager melee_attack Checks: ~p", [Checks]),

    case Checks of 
        true ->
            combat:attack(?QUICK, Villager#villager.id, Villager#villager.target),

            game:add_event(self(), 
                           attack, 
                           Villager#villager.id, 
                           Villager#villager.id, 
                           16),

            Villager#villager {task_state = running};
        false ->
            Villager#villager {task_state = completed}
    end.

harvest(Villager) ->
    {StructureId, _, _} = Villager#villager.structure,

    VillagerObj = obj:get(Villager#villager.id),
    StructureObj = obj:get(StructureId),

    ResourceType = structure:resource_type(StructureObj),

    SkillType = structure:to_skill(StructureObj),

    SkillTypeLower = string:lowercase(SkillType),
    SkillTypeAtom = binary_to_atom(SkillTypeLower, latin1),

    obj:update_state(Villager#villager.id, SkillTypeAtom),

    EventData = {Villager#villager.id, 
                 StructureId,
                 ResourceType,
                 obj:pos(VillagerObj)},

    NumTicks = ?TICKS_SEC * 10,

    lager:info("Added harvest event"),
    game:add_event(self(), harvest, EventData, Villager#villager.id, NumTicks),

    Villager#villager {task_state = running}.

gather(Villager) ->
    lager:info("Villager gather"),
    VillagerObj = obj:get(Villager#villager.id),
    ResType = maps:get(restype, Villager#villager.data),

    EventData =  {Villager#villager.id, 
                  ResType,
                  obj:pos(VillagerObj)},
    
    SkillType = resource:type_to_skill(ResType),

    %TODO remove once obj states are converted to binary instead of atoms
    SkillTypeLower = string:lowercase(SkillType),
    SkillTypeAtom = binary_to_atom(SkillTypeLower, latin1),

    obj:update_state(Villager#villager.id, SkillTypeAtom),

    game:add_event(self(), gather, EventData, Villager#villager.id, ?TICKS_SEC * 10),

    Villager#villager {task_state = running}.

load_resources(Villager) ->
    VillagerObj = obj:get(Villager#villager.id),
    AllObjs = obj:get_by_pos(VillagerObj#obj.pos),
    [Structure | _Rest] = lists:filter(fun(Obj) -> Obj#obj.class =:= structure end, AllObjs),
    StructureId = Structure#obj.id,

    Capacity = obj:get_capacity(Villager#villager.id),
    Items = item:get_non_equiped(StructureId),

    F = fun(Item) ->
            ItemId = maps:get(<<"id">>, Item),
            ItemWeight = maps:get(<<"weight">>, Item),
            ItemQuantity = maps:get(<<"quantity">>, Item),
            ItemTotalWeight = ItemWeight * ItemQuantity,

            TotalWeight = item:get_total_weight(Villager#villager.id),

            case (TotalWeight + ItemTotalWeight) =< Capacity of
                true ->
                    item:transfer(ItemId, Villager#villager.id);
                false ->
                    Space = Capacity - TotalWeight,
                    Quantity = erlang:trunc(Space / ItemWeight),

                    case Quantity of
                        0 -> 
                            nothing;
                        Quantity -> 
                            item:split(ItemId, Quantity),
                            item:transfer(ItemId, Villager#villager.id)
                    end
            end
        end,

    lists:foreach(F, Items),

    Villager#villager {task_state = completed}.

unload_resources(Villager) ->
    VillagerObj = obj:get(Villager#villager.id),
    AllObjs = obj:get_by_pos(VillagerObj#obj.pos),
    [Structure | _Rest] = lists:filter(fun(Obj) -> Obj#obj.class =:= structure end, AllObjs),
    StructureId = Structure#obj.id,
   
    Capacity = obj:get_capacity(StructureId),
    Items = item:get_non_equiped(Villager#villager.id),

    F = fun(Item) ->           
            ItemId = maps:get(<<"id">>, Item),
            ItemWeight = maps:get(<<"weight">>, Item),
            ItemQuantity = maps:get(<<"quantity">>, Item),
            ItemTotalWeight = ItemWeight * ItemQuantity,
           
            TotalWeight = item:get_total_weight(StructureId),

            case (TotalWeight + ItemTotalWeight) =< Capacity of
                true ->
                    item:transfer(ItemId, StructureId);
                false ->
                    Space = Capacity - TotalWeight,
                    Quantity = erlang:trunc(Space / ItemWeight),

                    case Quantity of
                        0 ->
                            nothing;
                        Quantity ->
                            item:split(ItemId, Quantity),
                            item:transfer(ItemId, StructureId)
                    end
            end
        end,

    lists:foreach(F, Items),

    Villager#villager {task_state = completed}.

transfer_water(Villager) -> transfer_item_by_class(Villager, ?WATER).
transfer_food(Villager) -> transfer_item_by_class(Villager, ?FOOD).

transfer_item_by_class(Villager, ItemClass) ->
    {StorageId, _StoragePos, _StorageName} = Villager#villager.storage,
    %TODO check weight and capacity
    case item:get_by_class(StorageId, ItemClass) of
        [Item | _Rest] -> 
            item:transfer(item:id(Item), Villager#villager.id);
        _ -> 
            none
    end,

    Villager#villager {task_state = completed}.
    

idle(Villager) ->
    Villager#villager {task_state = completed}.

explore(Villager) ->
    lager:info("Villager explore"),
    EventData = Villager#villager.id,

    obj:update_state(Villager#villager.id, ?EXPLORING),
    game:add_event(self(), explore, EventData, Villager#villager.id, ?TICKS_SEC * 3),

    Villager#villager {task_state = running}.

build(Villager) ->
    lager:info("Villager build"),
    EventData = Villager#villager.id,

    StructureId = maps:get(structure, Villager#villager.data),
    NumTicks = obj_attr:value(StructureId, <<"build_time">>),

    EventData = {Villager#villager.id, StructureId},

    %Add obj update state to change to moving state on next tick
    game:add_obj_update(self(), Villager#villager.id, ?STATE, ?BUILDING, 0),
    game:add_obj_update(self(), StructureId, ?STATE, ?PROGRESSING, 0),

    game:add_event(self(), build, EventData, Villager#villager.id, NumTicks).

refine(Villager) ->
    lager:info("Villager refine"),
    {StructureId, _StructurePos, _StructureName} = Villager#villager.structure,

    EventData = {StructureId, 
                 Villager#villager.id, 
                 ?TICKS_SEC * 10},

    obj:update_state(Villager#villager.id, ?REFINING),
    game:add_event(self(), refine, EventData, Villager#villager.id, ?TICKS_SEC * 10),

    Villager#villager {task_state = running}.

craft(Villager) ->
    lager:info("Villager crafting"),
    RecipeName = maps:get(recipe, Villager#villager.data),
    
    {StructureId, _StructurePos, _StructureName} = Villager#villager.structure,
    
    EventData = {StructureId, 
                 Villager#villager.id, 
                 RecipeName},

    lager:info("Craft event_data: ~p", [EventData]),
    obj:update_state(Villager#villager.id, ?CRAFTING),
    game:add_event(self(), craft, EventData, Villager#villager.id, ?TICKS_SEC * 10),

    Villager#villager {task_state = running}.

experiment(Villager) ->
    lager:info("Villager experimenting"),
    {StructureId, _StructurePos, _StructureName} = Villager#villager.structure,

    EventData = {Villager#villager.player,
                 StructureId,
                 Villager#villager.id},

    obj:update_state(Villager#villager.id, ?EXPERIMENTING),
    game:add_event(self(), experiment, EventData, Villager#villager.id, ?TICKS_SEC * 10),

    Villager#villager {task_state = running}.

eat(Villager) ->
    lager:info("Villager eating"),
    
    % Assume food exists as check or transfer was completed immediately before
    [Food | _Rest] = item:get_by_class(Villager#villager.id, ?FOOD),

    item:update(item:id(Food), item:quantity(Food) - 1),

    obj:update_state(Villager#villager.id, ?EATING),
    game:add_event(self(), ?EATING, Villager#villager.id, Villager#villager.id, ?TICKS_SEC * 10),

    Villager#villager {task_state = running}.

drink(Villager) ->
    lager:info("Villager drinking"),
    
    [Water | _Rest] = item:get_by_class(Villager#villager.id, ?WATER),

    item:update(item:id(Water), item:quantity(Water) - 1),

    obj:update_state(Villager#villager.id, ?DRINKING),
    game:add_event(self(), ?DRINKING, Villager#villager.id, Villager#villager.id, ?TICKS_SEC * 10),
    
Villager#villager {task_state = running}.

sleep(Villager) ->
    lager:info("Villager sleeping"),

    obj:update_state(Villager#villager.id, ?SLEEPING),
    game:add_event(self(), ?SLEEPING, Villager#villager.id, Villager#villager.id, ?TICKS_SEC * 30),

    Villager#villager {task_state = running}.

%%% End of HTN functions %%%



%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Data = #{},
    {ok, Data}.

handle_cast({process, Tick}, Data) ->   

    NewData = create_new_plans(Tick, Data),
    NewData2 = run_new_plans(Tick, NewData),

    {noreply, NewData2};

handle_cast({remove, VillagerId}, Data) ->
    NewData = maps:remove(VillagerId, Data),
    {noreply, NewData};

handle_cast({set_behavior, VillagerId, Behavior}, Data) ->
    Villager = maps:get(VillagerId, Data),
    NewVillager = Villager#villager{behavior = Behavior},
    NewData = maps:update(VillagerId, NewVillager, Data),

    {noreply, NewData};

handle_cast({assign, VillagerId, TargetId}, Data) ->
    Villager = maps:get(VillagerId, Data),
    StructureObj = obj:get(TargetId),
    Structure = {TargetId, obj:pos(StructureObj), obj:name(StructureObj)},
    NewVillager = Villager#villager{structure = Structure}, 
    NewData = maps:update(VillagerId, NewVillager, Data),

    {noreply, NewData};

handle_cast({set_order, VillagerId, Order, OrderData}, Data) ->
    Villager = maps:get(VillagerId, Data),
    VillagerData = Villager#villager.data,

    NewVillager = Villager#villager{order = Order,
                                    data = maps:merge(VillagerData, OrderData)},

    NewData = maps:update(VillagerId, NewVillager, Data),

    %Send talk message confirming order
    sound:talk(NewVillager#villager.id, order_speech(Order)),

    %Potentially send villager change to player
    game:send_villager_change(NewVillager),

    {noreply, NewData};

handle_cast({set_target, VillagerId, TargetId}, Data) ->
    Villager = maps:get(VillagerId, Data),
    NewVillager = Villager#villager{target = TargetId},
    NewData = maps:update(VillagerId, NewVillager, Data),

    {noreply, NewData};

handle_cast({remove_structure, StructureId}, Data) ->
    NewData = remove_shelters(StructureId, 
                              remove_assigns(StructureId,
                                             remove_storage(StructureId, Data))),

    {noreply, NewData};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({create, Level, Player, Pos}, _From, Data) ->
    lager:info("Create Villager: ~p ~p ~p", [Level, Player, Pos]),

    VillagerId = generate(Level, Player, Pos),

    Villager = #villager{id = VillagerId,                                                  
                         player = Player,
                         behavior = villager,
                         last_plan = ?MAX_INT,
                         last_run = ?MAX_INT},

    NewData = maps:put(VillagerId, Villager, Data),

    {reply, VillagerId, NewData};

handle_call({info, VillagerId}, _From, Data) ->
    Villager = maps:get(VillagerId, Data, none),
    {reply, Villager, Data};

handle_call({has_assigned, StructureId}, _From, Data) ->
    F = fun(_VillagerId, Villager, Acc) ->

            case Villager#villager.structure of
                none -> Acc;
                {AssignedId, _Pos, _Name} ->
                    case AssignedId =:= StructureId of
                        true ->
                            [Villager | Acc];
                        false ->    
                            Acc
                    end
            end
        end,

    Result = maps:fold(F, [], Data),

    {reply, Result =/= [], Data};

handle_call({has_assigned, StructureId, VillagerId}, _From, Data) ->
    Result = case maps:get(VillagerId, Data, none) of
                none -> 
                    false;
                Villager -> 
                    {AssignedId, _Pos, _Name} = Villager#villager.structure,
                    AssignedId =:= StructureId
             end,
    {reply, Result, Data};

handle_call({assign_list, Player}, _From, Data) ->
    F = fun(VillagerId, Villager, Acc) ->
            case Villager#villager.player =:= Player of
                true ->
                    VillagerObj = obj:get(VillagerId),
                    lager:info("Villager structure: ~p", [Villager#villager.structure]),
                    StructureName = case Villager#villager.structure of
                                        none -> <<"unassigned">>;
                                        {_Id, _Pos, Name} ->  
                                            Name
                                    end,

                    [#{<<"id">> => VillagerId,
                       <<"name">> => obj:name(VillagerObj),
                       <<"image">> => obj:image(VillagerObj),
                       <<"order">> => Villager#villager.order,
                       <<"structure">> => StructureName} | Acc];
                false ->
                    Acc
            end              
        end,

    AssignList = maps:fold(F, [], Data),

    {reply, AssignList, Data};

handle_call({get_by_structure, StructureId}, _From, Data) ->
    F = fun(_VillagerId, Villager) ->
            {Id, _Pos, _Name} = Villager#villager.structure,
            Id =:= StructureId
        end,

    Filtered = maps:filter(F, Data),
    Keys = maps:keys(Filtered),
    [VillagerId] = Keys,

    {reply, VillagerId, Data};

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Event}, 
                              {from, From}
                             ]),
    {noreply, Data}.

handle_info({broadcast, Message}, Data) ->
    
    case maps:get(<<"packet">>, Message) of
        <<"sound">> -> 
            WitnessId = maps:get(<<"witnessid">>, Message),
            sound:talk(WitnessId, "Did you hear that?");
        _ -> nothing
    end,

    {noreply, Data};

handle_info({event_complete, {obj_create, VillagerId}}, Data) ->
    Villager = maps:get(VillagerId, Data),
    NewVillager = Villager#villager{last_plan = 0,
                                    last_run = 0},
    NewData = maps:update(VillagerId, NewVillager, Data),

    {noreply, NewData};

handle_info({event_complete, {obj_update, _VillagerId}}, Data) ->
    %Do not want to process_complete to execute for obj_update
    {noreply, Data};

handle_info({event_complete, {Event, VillagerId}}, Data) ->
    lager:info("Event Complete: ~p ~p", [Event, VillagerId]),
    Villager = maps:get(VillagerId, Data, invalid),

    NewData = 
        case Villager =/= invalid of
            true ->
                NewVillager = process_event_complete(Villager),
                maps:update(VillagerId, NewVillager, Data);
            false ->
                Data
        end,

    {noreply, NewData};
handle_info({event_failure, {_Event, VillagerId, Error, _EventData}}, Data) ->
    _Villager = maps:get(VillagerId, Data, invalid),

    case Error of
        _ -> lager:info("Event failure: ~p", [Error])
    end,

    {noreply, Data};
handle_info(Info, Data) ->
    lager:info("~p", Info),
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
%% --------------------------------------------------------------------
%%

generate(Level, PlayerId, Pos) ->
    Name = generate_name(),
    Template = <<"Human Villager">>,
    State = none,

    lager:info("~p ~p, ~p ~p ~p", [Pos, PlayerId, Template, Name, State]),
    Id = obj:create(Pos, PlayerId, Template, Name, State),

    obj_attr:set(Id, ?STRENGTH, util:rand(10 + Level)),
    obj_attr:set(Id, ?TOUGHNESS, util:rand(10 + Level)),
    obj_attr:set(Id, ?ENDURANCE, util:rand(10 + Level)),
    obj_attr:set(Id, ?DEXTERITY, util:rand(10 + Level)),
    obj_attr:set(Id, ?INTELLECT, util:rand(10 + Level)),
    obj_attr:set(Id, ?FOCUS, util:rand(10 + Level)),
    obj_attr:set(Id, ?SPIRIT, util:rand(10 + Level)),
    obj_attr:set(Id, ?CREATIVITY, util:rand(10 + Level)),

    GSkills = skill_template:select(<<"class">>, <<"Gathering">>),
    CSkills = skill_template:select(<<"class">>, <<"Crafting">>),
    AllSkills = GSkills ++ CSkills,

    Skill1 = lists:nth(util:rand(length(AllSkills)), AllSkills),
    RemainingSkills1 = lists:delete(Skill1, AllSkills),

    Skill2 = lists:nth(util:rand(length(RemainingSkills1)), RemainingSkills1),
    RemainingSkills2 = lists:delete(Skill2, RemainingSkills1),

    Skill3 = lists:nth(util:rand(length(RemainingSkills2)), RemainingSkills2),

    lager:info("Skills1: ~p", [Skill1]),
    lager:info("Skills2: ~p", [Skill2]),
    lager:info("Skills3: ~p", [Skill3]),
    skill:update(Id, maps:get(<<"name">>, Skill1), util:rand(2000)),
    skill:update(Id, maps:get(<<"name">>, Skill2), util:rand(2000)),
    skill:update(Id, maps:get(<<"name">>, Skill3), util:rand(2000)),

    %Return ID
    Id.

create_new_plans(Tick, Data) ->
    F = fun(VillagerId, Villager, Acc) ->
            case Tick >= (Villager#villager.last_plan + (2 * ?TICKS_SEC)) of
                true ->
                    NewVillager = process_plan(Villager, Tick),
                    maps:put(VillagerId, NewVillager, Acc);
                false ->
                    maps:put(VillagerId, Villager, Acc)
            end
        end,

    maps:fold(F, #{}, Data). 

run_new_plans(Tick, Data) ->
    F = fun(VillagerId, Villager, Acc) ->
            case Tick >= (Villager#villager.last_run + (2 * ?TICKS_SEC)) of
                true ->
                    NewVillager = process_run_plan(Villager, Tick),
                    maps:put(VillagerId, NewVillager, Acc);
                false ->
                    maps:put(VillagerId, Villager, Acc)
            end
        end,

    maps:fold(F, #{}, Data).

process_plan(Villager, Tick) ->
    NewVillager = process_perception(Villager),

    CurrentPlan = NewVillager#villager.plan,
    {PlanLabel, NewPlan} = htn:plan(NewVillager#villager.behavior, 
                                    NewVillager, 
                                    villager),
    %lager:info("NewPlan: ~p CurrentPlan: ~p", [NewPlan, CurrentPlan]),
    case NewPlan =:= CurrentPlan of
        false ->
            %New plan cancel current event
            game:cancel_event(NewVillager#villager.id),

            case convert_plan_label(PlanLabel) of
                none -> nothing;
                PlanLabelSpeech -> sound:talk(NewVillager#villager.id, PlanLabelSpeech)
            end,

            %Reset Villager statistics
            NewerVillager = NewVillager#villager{plan = NewPlan,
                                                 task_state = init,
                                                 task_index = 1,
                                                 last_plan = Tick},


            NewerVillager;
        true ->
            NewVillager#villager{last_plan = Tick}
    end.

process_run_plan(Villager, Tick) ->

    lager:debug("Villager ~p ~p", [Villager, Tick]),
    case Villager#villager.plan of
        [] -> 
            Villager#villager{last_run = Tick};
        _ -> 
            NewVillager = process_task_state(Villager#villager.task_state, Villager),
            NewVillager#villager{last_run = Tick}
    end.


get_task_by_index(Villager, TaskIndex) -> 
    TaskData = lists:nth(TaskIndex, Villager#villager.plan),
    process_task_data(Villager, TaskData).

process_task_data(Villager, {TaskName, TaskArgs}) -> {TaskName, [Villager, TaskArgs]};
process_task_data(Villager, TaskName) -> {TaskName, [Villager]}.

process_task_state(init, Villager) ->
    TaskData = lists:nth(1, Villager#villager.plan),

    {TaskName, TaskArgs} = get_task_by_index(Villager, 1),

    NewVillager = erlang:apply(villager, TaskName, TaskArgs),


    NewVillager;
process_task_state(completed, Villager) ->
    TaskIndex = Villager#villager.task_index,
    PlanLength = length(Villager#villager.plan),

    NextTask = get_next_task(TaskIndex, PlanLength),
    lager:debug("V - NextTask: ~p", [NextTask]),

    NewerVillager = case NextTask of
                        {next_task, NextTaskIndex} ->
                            {TaskName, TaskArgs} = get_task_by_index(Villager, NextTaskIndex),

                            lager:debug("V - TaskName: ~p TaskArgs: ~p", [TaskName, TaskArgs]),
                            NewVillager = erlang:apply(villager, TaskName, TaskArgs),
                            NewVillager#villager{task_index = NextTaskIndex};
                        plan_completed ->
                            Villager#villager {task_state = init, task_index = 1}
                    end,


    %Return modified villager
    NewerVillager;
process_task_state(skipped, Villager) ->
    TaskIndex = Villager#villager.task_index,
    PlanLength = length(Villager#villager.plan),

    NextTask = get_next_task(TaskIndex, PlanLength),

    NewerVillager = case NextTask of
                        {next_task, NextTaskIndex} ->
                            {TaskName, TaskArgs} = get_task_by_index(Villager, NextTaskIndex),

                            NewVillager = erlang:apply(villager, TaskName, TaskArgs),
                            NewVillager#villager{task_index = NextTaskIndex};
                        plan_completed ->
                            Villager#villager {task_state = init, task_index = 1}
                    end,
    NewerVillager;
process_task_state(running, Villager) ->
    Villager.

process_event_complete(Villager) ->
    lager:info("Villager Event Complete: ~p ~p", [Villager#villager.task_index, length(Villager#villager.plan)]),

    {TaskName, TaskArgs} = get_task_by_index(Villager, Villager#villager.task_index),

    lager:info("Villager Task: ~p", [TaskName]),

    case TaskName of
        move_to -> process_move_to_complete(TaskArgs);
        move_to_pos -> process_move_complete(Villager);
        claim_shelter -> complete_task(Villager);
        harvest -> continue_task(Villager);
        explore -> continue_task(Villager);
        gather -> continue_task(Villager);
        refine -> complete_task(Villager);
        craft -> process_craft_complete(Villager);
        melee_attack -> complete_task(Villager);
        eat -> complete_task(Villager);
        sleep -> complete_task(Villager);
        _ -> Villager
    end.

process_move_to_complete([Villager, Pos]) ->
    VillagerObj = obj:get(Villager#villager.id),

    DestPos = get_pos(Villager, Pos),

    case obj:pos(VillagerObj) =:= DestPos of
        true -> 
            obj:update_state(Villager#villager.id, none),            
            Villager#villager {task_state = completed}; 
        false ->
            move_to(Villager, Pos)
    end.

process_move_complete(Villager) ->
    [VillagerObj] = db:read(obj, Villager#villager.id),
    case VillagerObj#obj.pos =:= Villager#villager.dest of
        true -> 
            obj:update_state(Villager#villager.id, none),
            Villager#villager {task_state = completed}; 
        false ->
            move_to_pos(Villager)
    end.

process_craft_complete(Villager) ->
    obj:update_state(Villager#villager.id, none),
    Villager#villager {order = none, task_state = completed}.

continue_task(Villager) ->
    Villager#villager {task_state = completed}.

complete_task(Villager) ->
    obj:update_state(Villager#villager.id, none),            
    Villager#villager {task_state = completed}.

find_enemies(_Villager, [], Enemies) ->
    Enemies;
find_enemies(VillagerObj, [PerceptionObjId | Rest], Enemies) ->
    PerceptionObj = obj:get(PerceptionObjId),
    NewEnemies = filter_objs(obj:player(VillagerObj), PerceptionObj, Enemies),
    find_enemies(VillagerObj, Rest, NewEnemies).

filter_objs(_VillagerPlayer, #obj{state = State}, Enemies) when State =:= ?DEAD -> Enemies;
filter_objs(_VillagerPlayer, #obj{class = Class}, Enemies) when Class =:= ?CORPSE -> Enemies;
filter_objs(VillagerPlayer, #obj{player = ObjPlayer}, Enemies) when VillagerPlayer =:= ObjPlayer -> Enemies;
filter_objs(_VillagerPlayer, #obj{player = ObjPlayer}, Enemies) when ObjPlayer =:= ?EMPIRE -> Enemies;  %TODO add relationship logic
filter_objs(_VillagerPlayer, Obj, Enemies) -> [Obj | Enemies].

find_structure(Villager, Type) ->       
    lager:debug("find_structure: ~p", [Type]),
    Structures  = obj:get_by_attr(Villager#villager.player, [{subclass, Type}, {state, none}]),

    ClaimedList = case Type of
                      <<"shelter">> -> get_claimed_shelters(Villager#villager.player);
                      <<"resource">> -> get_claimed_structures(Villager#villager.player);
                      <<"craft">> -> get_claimed_structures(Villager#villager.player);
                      <<"storage">> -> [] % Cannot claim storage
                  end,

    lager:debug("ClaimedList: ~p", [ClaimedList]),
    F = fun(Structure, Acc) ->
            case lists:member(Structure#obj.id, ClaimedList) of
                false -> [Structure | Acc];
                true -> Acc
            end    
        end,

    [First | _Rest] = lists:foldl(F, [], Structures),
    First.

get_storages(Player) ->
    obj:get_by_attr(Player, [{subclass, <<"storage">>}]).

get_next_task(TaskIndex, PlanLength) when TaskIndex < PlanLength ->
        NewTaskIndex = TaskIndex + 1,
        {next_task, NewTaskIndex};
get_next_task(_TaskIndex, _PlanLength) ->
        plan_completed.

move_unit(Obj = #obj {id = Id, pos = Pos}, NewPos) ->
    lager:debug("Pos: ~p NewPos: ~p", [Pos, NewPos]),

    SourcePos = Pos,
    DestPos = NewPos,
    MoveTicks = obj:movement_cost(Obj, DestPos),
    lager:debug("Move ticks: ~p", [MoveTicks]),

    %Add obj update state to change to moving state on next tick
    game:add_obj_update(self(), Id, ?STATE, ?MOVING, 1),
                
    %Add obj move event to execute in MoveTicks
    game:add_obj_move(self(), Id, SourcePos, DestPos, MoveTicks).

move_next_path(_VillagerObj, []) -> nothing;
move_next_path(VillagerObj, Path) -> move_unit(VillagerObj, lists:nth(2, Path)).

get_claimed_structures(Player) ->
    Villagers = db:index_read(villager, Player, #villager.player),

    F = fun(#villager{id = Id, structure = Structure}, Acc) when Structure =/= none -> 
                {Id, _Pos, _Name} = Structure,
                [Id | Acc];
           (_, Acc) -> 
                Acc
        end,

    lists:foldl(F, [], Villagers).

get_claimed_shelters(Player) ->
    Villagers = db:index_read(villager, Player, #villager.player),

    F = fun(#villager{id = Id, shelter = Shelter}, Acc) when Shelter =/= none -> 
                {Id, _Pos, _Name} = Shelter,
                [Id | Acc];
           (_, Acc) -> 
                Acc
        end,

    lists:foldl(F, [], Villagers).

process_perception(Villager) ->
    %TODO re-architect villager perception
    [VillagerObj] = db:read(obj, Villager#villager.id),

    Perception = perception:get_entity(VillagerObj),

    Enemies = find_enemies(VillagerObj, Perception, []),
    Villager#villager{enemies = Enemies}.

remove_shelters(StructureId, Data) ->
    F = fun(_VillagerId, Villager, Acc) ->
            case Villager#villager.shelter of
                none -> Acc;
                {ShelterId, _, _} ->
                    case ShelterId =:= StructureId of                    
                        true ->
                            NewVillager = Villager#villager {shelter = none},
                            [NewVillager | Acc];
                        false ->    
                            Acc
                    end
            end
        end,

    maps:fold(F, #{}, Data).

remove_assigns(StructureId, Data) ->
    F = fun(_VillagerId, Villager, Acc) ->                
            case Villager#villager.structure of
                none -> Acc;
                {Id, _, _} ->
                    case Id =:= StructureId of                        
                        true ->
                            NewVillager = Villager#villager {structure = none},
                            [NewVillager | Acc];
                        false ->    
                            Acc
                    end
            end
        end,

    maps:fold(F, #{}, Data).

remove_storage(StructureId, Data) ->
    F = fun(_VillagerId, Villager, Acc) ->
            case Villager#villager.storage of
                none -> Acc;
                {StorageId, _, _} ->
                    case StorageId =:= StructureId of
                        true ->
                            NewVillager = Villager#villager {storage = none},
                            [NewVillager | Acc];
                        false ->    
                            Acc
                end
            end
        end,

    maps:fold(F, #{}, Data).


generate_name() ->
    Names = [<<"Geoffry Holte">>,
             <<"Roderich Denholm">>,
             <<"Warder Folcey">>,
             <<"Andes Bardaye">>],

    lists:nth(util:rand(length(Names)), Names).

%TODO check this
find_stored_items_by_class(Villager, ItemClass) ->
    Storages = get_storages(Villager#villager.player),

    F = fun(Storage, Acc) ->
            Items = item:get_by_class(obj:id(Storage), ItemClass),
            Items ++ Acc
        end,

    lists:foldl(F, [], Storages).

convert_plan_label({villager, flee_to_hero}) -> "I need protection!";
convert_plan_label({villager, flee_randomly}) -> "Run away!";
convert_plan_label({villager, has_food}) -> "Time to eat!";
convert_plan_label({villager, has_food_storage}) -> "Time to find some food!";
convert_plan_label({villager, harvest_idle}) -> "We are out of storage for all these resources";
convert_plan_label({villager, tired_no_shelter}) -> "Sleeping on the ground, my favorite";
convert_plan_label(_) -> none.

order_speech(?ORDER_FOLLOW) -> "Yes sir, following!";
order_speech(?ORDER_EXPLORE) -> "Yes sir, exploring this area!";
order_speech(?ORDER_GATHER) -> "Yes sir, gathering resources!";
order_speech(?ORDER_HARVEST) -> "Yes sir, harvesting resources!";
order_speech(?ORDER_REFINE) -> "Yes sir, refining resources!";
order_speech(_) -> "Not sure what to say for this order".

