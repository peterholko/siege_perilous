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
-export([has_assigned/1, assign/2, remove/1, remove_structure/1, get_by_structure/1]).
-export([create_plan/0, run_plan/0]).
-export([enemy_visible/1, move_to_pos/1, move_randomly/1, hero_nearby/1]).
-export([set_pos_shelter/1, set_pos_hero/1, set_pos_structure/1]).
-export([morale_normal/1, morale_low/1, morale_very_low/1]).
-export([set_order_refine/1, set_order_craft/2, set_order_follow/1, set_order_harvest/1, set_order_guard/1, set_order_attack/1, set_target/2]).
-export([structure_needed/1, shelter_needed/1, storage_needed/1, harvest/1]).
-export([has_shelter/1, assigned_harvester/1, assigned_craft/1, has_storage/1]).
-export([free_structure/1, free_harvester/1, free_craft/1, free_shelter/1, free_storage/1]).
-export([find_shelter/1, find_harvester/1, find_craft/1, find_storage/1]).
-export([structure_not_full/1, load_resources/1, unload_resources/1]).
-export([set_pos_storage/1, set_hauling/1, set_none/1, not_hauling/1]).
-export([has_resources/1]).
-export([idle/1, refine/1, craft/1]).
-export([move_to_target/1, melee_attack/1]).
-export([has_order_follow/1, has_order_attack/1, has_order_guard/1, has_order_harvest/1, 
         has_order_refine/1, has_order_craft/1, has_order_experiment/1]).
-export([has_order/2, has_effect/2, has_not_effect/2]).
-export([generate/1]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, villager}, villager, [], []).

create_plan() ->
    gen_server:cast({global, villager}, create_plan).

run_plan() ->
    gen_server:cast({global, villager}, run_plan).

generate(Level) ->
    Id = obj:create({-9999, -9999}, -9999, unit, ?VILLAGER, <<"Human Villager">>, none),

    obj_attr:set(Id, <<"name">>, <<"Roderich Denholm">>),

    obj_attr:set(Id, ?STRENGTH, util:rand(10 + Level)),
    obj_attr:set(Id, ?TOUGHNESS, util:rand(10 + Level)),
    obj_attr:set(Id, ?ENDURANCE, util:rand(10 + Level)),
    obj_attr:set(Id, ?DEXTERITY, util:rand(10 + Level)),
    obj_attr:set(Id, ?INTELLECT, util:rand(10 + Level)),
    obj_attr:set(Id, ?FOCUS, util:rand(10 + Level)),
    obj_attr:set(Id, ?SPIRIT, util:rand(10 + Level)),
    obj_attr:set(Id, ?CREATIVITY, util:rand(10 + Level)),

    GSkills = skill_def:select(<<"class">>, <<"Gathering">>),
    CSkills = skill_def:select(<<"class">>, <<"Crafting">>),
    AllSkills = GSkills ++ CSkills,

    Skill1 = lists:nth(util:rand(length(AllSkills)), AllSkills),
    RemainingSkills1 = lists:delete(Skill1, AllSkills),

    Skill2 = lists:nth(util:rand(length(RemainingSkills1)), RemainingSkills1),
    RemainingSkills2 = lists:delete(Skill2, RemainingSkills1),

    Skill3 = lists:nth(util:rand(length(RemainingSkills2)), RemainingSkills2),

    skill:update(Id, maps:get(<<"name">>, Skill1), util:rand(26) - 1),
    skill:update(Id, maps:get(<<"name">>, Skill2), util:rand(26) - 1),
    skill:update(Id, maps:get(<<"name">>, Skill3), util:rand(26) - 1).

%%%
%%% HTN Conditions %%%
%%%

enemy_visible(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.enemies =/= [].

has_shelter(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.shelter =/= none.

assigned_harvester(Id) ->
    [Villager] = db:read(villager, Id),
    case obj:get(Villager#villager.structure) of
        invalid -> false;
        Obj -> 
            case obj:subclass(Obj) of
                <<"resource">> -> true;
                _ -> false
            end
    end.

assigned_craft(Id) ->
    [Villager] = db:read(villager, Id),
    case obj:get(Villager#villager.structure) of
        invalid -> false;
        Obj -> 
            case obj:subclass(Obj) of
                <<"craft">> -> true;
                _ -> false
            end
    end.

has_storage(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.storage =/= none.

hero_nearby(Id) ->
    true.
    %[VillagerObj] = db:read(obj, Id),
    %TODO check if villager has a hero
    %case obj:get_hero(VillagerObj#obj.player) of
    %    false -> 
    %        false;
    %    [Hero] ->
    %        map:distance(VillagerObj#obj.pos, Hero#obj.pos) =< 3
    %end.

morale_normal(Id) ->
    morale(Id, 50).
morale_low(Id) ->
    morale(Id, 25).
morale_very_low(Id) ->
    morale(Id, 0).

morale(Id, Value) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.morale >= Value.

has_order(Id, OrderType) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.order =:= OrderType.

has_effect(Id, EffectType) ->
    [VillagerObj] = db:read(obj, Id),
    effect:has_effect(VillagerObj#obj.id, EffectType).

has_not_effect(Id, EffectType) ->
    [VillagerObj] = db:read(obj, Id),
    not effect:has_effect(VillagerObj#obj.id, EffectType).

has_order_follow(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.order =:= ?ORDER_FOLLOW.

has_order_guard(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.order =:= ?ORDER_GUARD.

has_order_attack(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.order =:= ?ORDER_ATTACK.

has_order_harvest(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.order =:= ?ORDER_HARVEST.

has_order_refine(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.order =:= refine.

has_order_craft(Id) ->
    [Villager] = db:read(villager, Id),
    case Villager#villager.order of
        {craft, _RecipeName} -> true;
        _ -> false
    end.

has_order_experiment(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.order =:= experiment.

shelter_needed(Id) ->
    [Villager] = db:read(villager, Id), 
    Villager#villager.shelter =:= none.

structure_needed(Id) ->
    [Villager] = db:read(villager, Id), 
    Villager#villager.structure =:= none.

storage_needed(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.storage =:= none.

free_structure(Id) -> free_structure(Id, structure, [{subclass, [<<"resource">>, <<"craft">>]}]).
free_harvester(Id) -> free_structure(Id, structure, [{subclass, <<"resource">>}]).
free_craft(Id) -> free_structure(Id, structure, [{subclass, <<"craft">>}]).
free_shelter(Id) -> free_structure(Id, shelter, [{subclass, <<"shelter">>}]).
free_storage(Id) -> free_structure(Id, storage, [{subclass, <<"storage">>}]).
                        
free_structure(Id, StructureClass, StructureTypes) ->    
    [Villager] = db:read(villager, Id),

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

structure_not_full(Id) ->
    [Villager] = db:read(villager, Id), 
    Capacity = obj:get_capacity(Villager#villager.structure),
    
    %Assume "full" is 85% of capacity or over
    %and if 15% of capacity cannot fit, structure is full
    Capacity15 = erlang:trunc(Capacity * 0.15),

    obj:has_space(Villager#villager.structure, Capacity15).

not_hauling(Id) ->
    [Villager] = db:read(villager, Id), 
    Villager#villager.activity =/= hauling.

has_resources(Id) ->
    [Villager] = db:read(villager, Id),
    
    case Villager#villager.storage =/= none of
        true -> true;
        false -> false
    end.
    
%%% 
%%% HTN Primitive Tasks
%%%

find_shelter(Villager) ->
    Shelter = find_structure(Villager, <<"shelter">>),
    Villager#villager {shelter = Shelter#obj.id,
                       dest = Shelter#obj.pos,
                       task_state = completed}.

find_harvester(Villager) ->
    Harvester = find_structure(Villager, <<"resource">>),
    Villager#villager {structure = Harvester#obj.id,
                       dest = Harvester#obj.pos,
                       task_state = completed}.

find_craft(Villager) ->
    Craft = find_structure(Villager, <<"craft">>),
    Villager#villager {structure = Craft#obj.id,
                       dest = Craft#obj.pos,
                       task_state = completed}.

find_storage(Villager) ->
    Storage = find_structure(Villager, <<"storage">>),
    Villager#villager {storage = Storage#obj.id,
                       dest = Storage#obj.pos,
                       task_state = completed}.

set_pos_shelter(Villager) ->
    [Dwelling] = db:read(obj, Villager#villager.shelter),
    Villager#villager {dest = Dwelling#obj.pos, task_state = completed}.

set_pos_structure(Villager) ->
    [Structure] = db:read(obj, Villager#villager.structure),
    Villager#villager {dest = Structure#obj.pos, task_state = completed}.

set_pos_storage(Villager) ->
    [Storage] = db:read(obj, Villager#villager.storage),
    Villager#villager {dest = Storage#obj.pos, task_state = completed}.

set_pos_hero(Villager) ->
    [VillagerObj] = db:read(obj, Villager#villager.id),
    Hero = obj:get_hero(VillagerObj#obj.player),
    
    Villager#villager {dest = Hero#obj.pos, task_state = completed}.

set_hauling(Villager) ->
    Villager#villager {activity = hauling, task_state = completed}.

set_none(Villager) ->
    Villager#villager {activity = none, task_state = completed}.

move_to_pos(Villager) ->
    [VillagerObj] = db:read(obj, Villager#villager.id),

    Dest = Villager#villager.dest,

    %If dest is set and dest does not equal villager current pos
    NewVillager = case (Dest =/= none) and (Dest =/= VillagerObj#obj.pos) of
                      true ->
                         Path = astar:astar(VillagerObj#obj.pos, Dest, VillagerObj),

                         case Path of
                             [] -> 
                                 %No path, move task completed
                                 Villager#villager {task_state = completed};
                             _ -> 
                                 %Move to next path location
                                 move_unit(VillagerObj, lists:nth(2, Path)),
                                 Villager#villager {task_state = running, path = Path}
                         end;
                      false ->
                         lager:info("Dest: ~p Pos: ~p", [Dest, VillagerObj#obj.pos]),
                         Villager#villager {task_state = completed}
                 end,
    lager:info("Move Villager: ~p",[NewVillager]),
    NewVillager.

move_randomly(Villager) ->
    [VillagerObj] = db:read(obj, Villager#villager.id),

    NewVillager = case game:get_valid_tiles(VillagerObj#obj.pos) of
                      [] -> 
                          Villager;
                      Tiles -> 
                          Random = util:rand(length(Tiles)),
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
                                  Path = astar:astar(VillagerObj#obj.pos, TargetObj#obj.pos, VillagerObj),

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
    EventData = {Villager#villager.id, Villager#villager.structure},
    NumTicks = ?TICKS_SEC * 8,
    game:add_event(self(), sharvest, EventData, Villager#villager.id, NumTicks),

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

idle(Villager) ->
    Villager#villager {task_state = completed}.

refine(Villager) ->
    %TODO add event here 
     lager:info("Villager crafting"),

    {craft, RecipeName} = Villager#villager.order,
    
    EventData = {Villager#villager.structure, 
                 Villager#villager.id, 
                 RecipeName},

    lager:info("Craft event_data: ~p", [EventData]),
    obj:update_state(Villager#villager.id, refining),
    game:add_event(self(), craft, EventData, Villager#villager.id, ?TICKS_SEC * 10),

    Villager#villager {task_state = running}.

craft(Villager) ->
    lager:info("Villager crafting"),

    {craft, RecipeName} = Villager#villager.order,
    
    EventData = {Villager#villager.structure, 
                 Villager#villager.id, 
                 RecipeName},

    lager:info("Craft event_data: ~p", [EventData]),
    obj:update_state(Villager#villager.id, crafting),
    game:add_event(self(), craft, EventData, Villager#villager.id, ?TICKS_SEC * 10),

    Villager#villager {task_state = running}.


%%% End of HTN functions %%%
has_assigned(StructureId) ->
    db:index_read(villager, StructureId, #villager.structure) =/= [].

get_by_structure(StructureId) ->
    [Villager] = db:index_read(villager, StructureId, #villager.structure),
    Villager#villager.id.

assign(SourceId, TargetId) ->
    [Villager] = db:read(villager, SourceId),
    db:write(Villager#villager {structure = TargetId}). 

set_order_refine(SourceId) ->
    [Villager] = db:read(villager, SourceId),
    db:write(Villager#villager {order = refine}). 

set_order_craft(SourceId, RecipeName) ->
    [Villager] = db:read(villager, SourceId),
    db:write(Villager#villager {order = {craft, RecipeName}}). 

set_order_follow(SourceId) ->
    [Villager] = db:read(villager, SourceId),
    db:write(Villager#villager {order = follow}).

set_order_guard(SourceId) ->
    [Villager] = db:read(villager, SourceId),
    db:write(Villager#villager {order = guard}).

set_order_attack(SourceId) ->
    [Villager] = db:read(villager, SourceId),
    db:write(Villager#villager {order = attack}).

set_order_harvest(SourceId) ->
    [Villager] = db:read(villager, SourceId),
    db:write(Villager#villager {order = ?ORDER_HARVEST}).

set_target(SourceId, TargetId) ->
    [Villager] = db:read(villager, SourceId),
    db:write(Villager#villager {target = TargetId}).

remove(ObjId) ->
    db:delete(villager, ObjId).

remove_structure(StructureId) ->
    remove_shelters(StructureId),
    remove_assigns(StructureId),
    remove_storage(StructureId).

remove_shelters(StructureId) ->
    Villagers = db:index_read(villager, StructureId, #villager.shelter),

    F = fun(Villager) -> 
            NewVillager = Villager#villager{shelter = none},
            db:write(NewVillager)
        end,

    lists:foreach(F, Villagers).

remove_assigns(StructureId) ->
    Villagers = db:index_read(villager, StructureId, #villager.structure),

    F = fun(Villager) -> 
            NewVillager = Villager#villager{structure = none},
            db:write(NewVillager)
        end,

    lists:foreach(F, Villagers).

remove_storage(StructureId) ->
    Villagers = db:index_read(villager, StructureId, #villager.storage),

    F = fun(Villager) -> 
            NewVillager = Villager#villager{storage = none},
            db:write(NewVillager)
        end,

    lists:foreach(F, Villagers).
%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast(create_plan, Data) ->
    process_create_plan(db:first(villager)),
    {noreply, Data};

handle_cast(run_plan, Data) ->
    process_run_plan(db:first(villager)),
    {noreply, Data};

handle_cast(process, Data) ->   

    process(db:first(villager)),

    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Event}, 
                              {from, From}
                             ]),
    {noreply, Data}.

handle_info({perception, {VillagerId, Objs}}, Data) ->
    [Villager] = db:read(villager, VillagerId),
    [VillagerObj] = db:read(obj, VillagerId),

    Enemies = find_enemies(VillagerObj, Objs, []),
    NewVillager = Villager#villager{enemies = Enemies},
    db:write(NewVillager),

    {noreply, Data};

handle_info({broadcast, Message}, Data) ->
    
    case maps:get(<<"packet">>, Message) of
        <<"sound">> -> 
            WitnessId = maps:get(<<"witnessid">>, Message),
            sound:talk(WitnessId, "Did you hear that?");
        _ -> nothing
    end,

    {noreply, Data};
handle_info({event_complete, {_Event, Id}}, Data) ->
    Villager = db:read(villager, Id),
    process_event_complete(Villager),
    {noreply, Data};
handle_info({event_failure, {_Event, Id, Error, _EventData}}, Data) ->
    _Villager = db:read(villager, Id),

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

process_create_plan('$end_of_table') ->
    done;
process_create_plan(Id) ->
    [Villager] = db:read(villager, Id),

    CurrentPlan = Villager#villager.plan,
    NewPlan = htn:plan(villager, Id, villager),

    case NewPlan =:= CurrentPlan of
        false ->
            %New plan cancel current event
            game:cancel_event(Id),

            %Reset Villager statistics
            NewVillager = Villager#villager {plan = NewPlan,
                                             new_plan = true,
                                             task_state = init,
                                             task_index = 1},
            db:write(NewVillager);
        true ->
            nothing
    end,

    process_create_plan(db:next(villager, Id)).

process_run_plan('$end_of_table') ->
    done;
process_run_plan(Id) ->
    lager:debug("Villager Run plan ~p", [Id]),
    [Villager] = db:read(villager, Id),

    %Skip if plan is empty
    case Villager#villager.plan of
        [] -> nothing;
        _ -> %Process villager task state
             NewVillager = process_task_state(Villager#villager.task_state, Villager),
             db:write(NewVillager)
    end,

    process_run_plan(db:next(villager, Id)).

process_task_state(init, Villager) ->
    TaskToRun = lists:nth(1, Villager#villager.plan),
    NewVillager = erlang:apply(villager, TaskToRun, [Villager]),
    NewVillager;
process_task_state(completed, Villager) ->
    TaskIndex = Villager#villager.task_index,
    PlanLength = length(Villager#villager.plan),

    NextTask = get_next_task(TaskIndex, PlanLength),

    case NextTask of
        {next_task, NextTaskIndex} ->
            TaskToRun = lists:nth(NextTaskIndex, Villager#villager.plan),
            NewVillager = erlang:apply(villager, TaskToRun, [Villager]),
            NewVillager#villager{task_index = NextTaskIndex};
        plan_completed ->
            Villager#villager {task_state = init, task_index = 1}
    end;
process_task_state(running, Villager) ->
    Villager.

process_event_complete([Villager]) ->
    Id = Villager#villager.id,

    lager:info("Villager Event Complete: ~p ~p", [Villager#villager.task_index, length(Villager#villager.plan)]),
    Task = lists:nth(Villager#villager.task_index, Villager#villager.plan),
    obj:update_state(Id, none),

    lager:info("Villager Task: ~p", [Task]),

    case Task of
        move_to_pos -> process_move_complete(Villager);
        claim_shelter -> complete_task(Villager);
        harvest -> complete_task(Villager);
        refine -> complete_task(Villager);
        craft -> process_craft_complete(Villager);
        melee_attack -> complete_task(Villager);
        _ -> nothing
    end;
process_event_complete([]) -> nothing.

process_move_complete(Villager) ->
    [VillagerObj] = db:read(obj, Villager#villager.id),

    case VillagerObj#obj.pos =:= Villager#villager.dest of
        true -> 
            NewVillager = Villager#villager {task_state = completed},           
            db:write(NewVillager);
        false ->
            move_to_pos(Villager)
    end.

process_craft_complete(Villager) ->
    NewVillager = Villager#villager {order = none, task_state = completed},
    db:write(NewVillager).

complete_task(Villager) ->
    NewVillager = Villager#villager {task_state = completed},
    db:write(NewVillager).

find_enemies(_Villager, [], Enemies) ->
    Enemies;
find_enemies(Villager, [PerceptionObj | Rest], Enemies) ->
    NewEnemies = filter_objs(Villager#obj.player, PerceptionObj, Enemies),
    find_enemies(Villager, Rest, NewEnemies).

filter_objs(_Player, #{<<"state">> := State}, Enemies) when State =:= dead -> Enemies;
filter_objs(Player, #{<<"player">> := ObjPlayer}, Enemies) when Player =:= ObjPlayer -> Enemies;
filter_objs(_Player, Obj, Enemies) -> [Obj | Enemies].

process('$end_of_table') ->
    lager:debug("Done processing villagers");
process(Id) ->
    [Villager] = db:read(villager, Id),
    [Obj] = db:read(obj, Id),

    NewVillager = process_morale(Villager, Obj),

    %Write only if changes
    case Villager =/= NewVillager of
        true -> db:write(NewVillager);
        false -> nothing
    end,
    
    process(db:next(villager, Id)).

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

process_morale(Villager, Obj) ->
    Food = case item:get_by_subclass(Obj#obj.id, ?FOOD) of
              [] -> false;
              [_Item | _Rest] -> true
           end,

    Dwelling = case Villager#villager.shelter of
                   none -> false;
                   _ -> true
               end,

    NewVillager = case {Food, Dwelling} of
                      {true, true} -> update_morale(Villager, 1);
                      {false, true} -> update_morale(Villager, -5);
                      {true, false} -> update_morale(Villager, -1);
                      {false, false} -> update_morale(Villager, -8)
                  end,
    NewVillager.

update_morale(Villager, Value) ->
    Morale = Villager#villager.morale + Value,

    NewMorale = case Morale > 0 of 
                    true -> erlang:min(Morale, 100);
                    false -> 0
                end,

    NewVillager = Villager#villager {morale = NewMorale},
    NewVillager.

get_next_task(TaskIndex, PlanLength) when TaskIndex < PlanLength ->
        NewTaskIndex = TaskIndex + 1,
        {next_task, NewTaskIndex};
get_next_task(_TaskIndex, _PlanLength) ->
        plan_completed.

move_unit(#obj {id = Id, player = Player}, NewPos) ->
    NumTicks = ?TICKS_SEC * 8,

    %Update unit state
    obj:update_state(Id, moving),
    
    %Create event data
    EventData = {Player, Id, NewPos},

    lager:info("Villager: adding game move event"),
    game:add_event(self(), move, EventData, Id, NumTicks).

move_next_path(_VillagerObj, []) -> nothing;
move_next_path(VillagerObj, Path) -> move_unit(VillagerObj, lists:nth(2, Path)).

get_claimed_structures(Player) ->
    Villagers = db:index_read(villager, Player, #villager.player),

    F = fun(#villager{id = Id, structure = Structure}, Acc) when Structure =/= none -> [Id | Acc];
           (_, Acc) -> Acc
        end,

    lists:foldl(F, [], Villagers).

get_claimed_shelters(Player) ->
    Villagers = db:index_read(villager, Player, #villager.player),

    F = fun(#villager{id = Id, shelter = Shelter}, Acc) when Shelter =/= none -> [Id | Acc];
           (_, Acc) -> Acc
        end,

    lists:foldl(F, [], Villagers).

