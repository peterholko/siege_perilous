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
-export([remove/1, remove_structure/1]).
-export([create_plan/0, run_plan/0]).
-export([enemy_visible/1, move_to_pos/1, has_dwelling/1, has_storage/1, hero_nearby/1]).
-export([set_pos_dwelling/1, set_pos_hero/1, set_pos_structure/1]).
-export([morale_normal/1, morale_low/1, morale_very_low/1]).
-export([order_follow/1, dwelling_needed/1, claim_dwelling/1, find_dwelling/1]).
-export([structure_needed/1, find_structure/1, has_structure/1, harvest/1]).
-export([claim_structure/1]).
-export([structure_not_full/1, load_resources/1, unload_resources/1, find_storage/1]).
-export([set_pos_storage/1, set_hauling/1, set_none/1, not_hauling/1, storage_needed/1]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, villager}, villager, [], []).

create_plan() ->
    gen_server:cast({global, villager}, create_plan).

run_plan() ->
    gen_server:cast({global, villager}, run_plan).

%%%
%%% HTN Conditions %%%
%%%

enemy_visible(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.enemy =/= none.

has_dwelling(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.dwelling =/= none.

has_structure(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.structure =/= none.

has_storage(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.storage =/= none.

hero_nearby(Id) ->
    [VillagerObj] = db:read(obj, Id),
    Hero = obj:get_hero(VillagerObj#obj.player),
    map:distance(VillagerObj#obj.pos, Hero#obj.pos) =< 3.

morale_normal(Id) ->
    morale(Id, 50).
morale_low(Id) ->
    morale(Id, 25).
morale_very_low(Id) ->
    morale(Id, 0).

morale(Id, Value) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.morale >= Value.

order_follow(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.order =:= follow.

dwelling_needed(Id) ->
    [Villager] = db:read(villager, Id), 
    [VillagerObj] = db:read(obj, Id),
   
    DwellingNeeded = Villager#villager.dwelling =:= none,
    DwellingAvailable = dwelling_available(VillagerObj#obj.player),

    DwellingNeeded and DwellingAvailable.

structure_needed(Id) ->
    [Villager] = db:read(villager, Id), 
    [VillagerObj] = db:read(obj, Id),
    
    StructureNeeded = Villager#villager.structure =:= none,
    StructureAvailable = structure_available(VillagerObj#obj.player),

    StructureNeeded and StructureAvailable.

storage_needed(Id) ->
    [Villager] = db:read(villager, Id),
    [VillagerObj] = db:read(obj, Id),
    
    StorageNeeded = Villager#villager.storage =:= none,
    StorageAvailable = storage_available(VillagerObj#obj.player),

    StorageNeeded and StorageAvailable.

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

%%% 
%%% HTN Primitive Tasks
%%%
set_pos_dwelling(Villager) ->
    [Dwelling] = db:read(obj, Villager#villager.dwelling),
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
                         Path = astar:astar(VillagerObj#obj.pos, Dest),

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

find_dwelling(Villager) ->
    [VillagerObj] = db:read(obj, Villager#villager.id),

    Objs = db:index_read(obj, VillagerObj#obj.player, #obj.player),

    NewVillager = case find_dwelling(Objs, none) of
                      {found, Dwelling} -> 
                          Villager#villager {dest = Dwelling#obj.pos};
                      _ -> 
                          Villager
                  end,

    NewVillager#villager {task_state = completed}.

claim_dwelling(Villager) -> 
    [VillagerObj] = db:read(obj, Villager#villager.id),

    Objs = db:index_read(obj, VillagerObj#obj.player, #obj.player),

    NewVillager = case find_dwelling(Objs, none) of
                      {found, Dwelling} ->
                          case Dwelling#obj.pos =:= VillagerObj#obj.pos of
                              true ->
                                  Villager#villager{dwelling = Dwelling#obj.id};
                              false ->
                                  Villager
                          end;
                      _ ->
                          Villager
                  end, 
    NewVillager#villager {task_state = completed}.

find_structure(Villager) ->
    [VillagerObj] = db:read(obj, Villager#villager.id),

    Objs = db:index_read(obj, VillagerObj#obj.player, #obj.player),

    NewVillager = case find_structure(Objs, none) of
                      {found, Structure} -> 
                          Villager#villager {structure = Structure#obj.id,
                                             dest = Structure#obj.pos};
                      _ -> 
                          Villager
                  end,
    NewVillager#villager {task_state = completed}.

find_storage(Villager) ->
    [VillagerObj] = db:read(obj, Villager#villager.id),

    Objs = db:index_read(obj, VillagerObj#obj.player, #obj.player),

    NewVillager = case find_storage(Objs, none) of
                      {found, Storage} ->
                          Villager#villager {storage = Storage#obj.id,
                                             dest = Storage#obj.pos};
                      _ ->
                          Villager
                  end,
    NewVillager#villager {task_state = completed}.

claim_structure(Villager) ->
    [VillagerObj] = db:read(obj, Villager#villager.id),

    Objs = db:index_read(obj, VillagerObj#obj.player, #obj.player),

    NewVillager = case find_structure(Objs, none) of
                      {found, Structure} ->
                          case Structure#obj.pos =:= VillagerObj#obj.pos of
                              true ->
                                  Villager#villager{structure = Structure#obj.id};
                              false ->
                                  Villager
                          end;
                      _ ->
                          Villager
                  end, 
    NewVillager#villager {task_state = completed}.

harvest(Villager) ->
    EventData = {Villager#villager.id, Villager#villager.structure},
    NumTicks = ?TICKS_SEC * 8,
    game:add_event(self(), sharvest, EventData, Villager#villager.id, NumTicks),

    Villager#villager {task_state = running}.

load_resources(Villager) ->
    Capacity = obj:get_capacity(Villager#villager.id),
    Items = item:get_non_equiped(Villager#villager.structure),

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
    Capacity = obj:get_capacity(Villager#villager.storage),
    Items = item:get_non_equiped(Villager#villager.id),

    F = fun(Item) ->           
            ItemId = maps:get(<<"id">>, Item),
            ItemWeight = maps:get(<<"weight">>, Item),
            ItemQuantity = maps:get(<<"quantity">>, Item),
            ItemTotalWeight = ItemWeight * ItemQuantity,
           
            TotalWeight = item:get_total_weight(Villager#villager.storage),

            case (TotalWeight + ItemTotalWeight) =< Capacity of
                true ->
                    item:transfer(ItemId, Villager#villager.storage);
                false ->
                    Space = Capacity - TotalWeight,
                    Quantity = erlang:trunc(Space / ItemWeight),

                    case Quantity of
                        0 ->
                            nothing;
                        Quantity ->
                            item:split(ItemId, Quantity),
                            item:transfer(ItemId, Villager#villager.storage)
                    end
            end
        end,

    lists:foreach(F, Items),

    Villager#villager {task_state = completed}.

%%% End of HTN functions %%%

remove(ObjId) ->
    db:delete(villager, ObjId).

remove_structure(StructureId) ->
    remove_dwellings(StructureId),
    remove_assigns(StructureId).

remove_dwellings(StructureId) ->
    Villagers = db:index_read(villager, StructureId, #villager.dwelling),

    F = fun(Villager) -> 
            NewVillager = Villager#villager{dwelling = none},
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

    case find_enemies(VillagerObj, Objs) of
        [] -> 
            nothing;
        Enemies -> 
            NewVillager = Villager#villager{enemy = Enemies},
            db:write(NewVillager)
    end,

    {noreply, Data};
handle_info({event_complete, {_Event, Id}}, Data) ->
    Villager = db:read(villager, Id),
    process_event_complete(Villager),
    {noreply, Data};
handle_info({event_failure, {_Event, Id, Error, EventData}}, Data) ->
    Villager = db:read(villager, Id),

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
    lager:info("Villager Create plan ~p", [Id]),
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
    lager:info("Villager Run plan ~p", [Id]),
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
    lager:info("Villager NextTask: ~p", [NextTask]),

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
        claim_dwelling -> complete_task(Villager);
        harvest -> complete_task(Villager);
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

complete_task(Villager) ->
    NewVillager = Villager#villager {task_state = completed},
    db:write(NewVillager).

find_enemies(Villager, Objs) ->
    find_enemies(Villager, Objs, []).

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

dwelling_available(Player) ->
    Objs = db:index_read(obj, Player, #obj.player),
    
    case find_dwelling(Objs, none) of
        {found, _Dwelling} -> true;
        _ -> false
    end.

find_dwelling(_Objs, {found, Dwelling}) ->
    {found, Dwelling};
find_dwelling([], _) ->
    none;
find_dwelling([Obj | Rest], _Dwelling) ->
    NewDwelling = new_dwelling(Obj),
    find_dwelling(Rest, NewDwelling).

new_dwelling(Obj = #obj{subclass = Subclass, state = State}) when (Subclass =:= <<"shelter">>) and 
                                                                  (State =:= none) ->
    case db:index_read(villager, Obj#obj.id, #villager.dwelling) of
        [_Villager] -> none;
        [] -> {found, Obj}
    end;
new_dwelling(_) -> 
    none.

structure_available(Player) ->
    Objs = db:index_read(obj, Player, #obj.player),

    case find_structure(Objs, none) of
        {found, _Structure} -> true;
        _ -> false
    end.

find_structure(_Objs, {found, Structure}) ->
    {found, Structure};
find_structure([], _) ->
    none;
find_structure([Obj | Rest], _Structure) ->
    NewStructure = new_structure(Obj),
    find_structure(Rest, NewStructure).

new_structure(Obj = #obj{subclass = Subclass, state = State}) when (Subclass =:= <<"resource">>) and
                                                                   (State =:= none) ->
    lager:info("Match Obj: ~p", [Obj]),
    case db:index_read(villager, Obj#obj.id, #villager.structure) of
        [_Villager] -> none;
        [] -> {found, Obj}
    end;
new_structure(_Obj) -> 
    none.

storage_available(Player) ->
    Objs = db:index_read(obj, Player, #obj.player),

    case find_storage(Objs, none) of
        {found, _Storage} -> true;
        _ -> false
    end.

find_storage(_Objs, {found, Storage}) ->
    {found, Storage};
find_storage([], _) ->
    none;
find_storage([Obj | Rest], _Storage) ->
    NewStorage = new_storage(Obj),
    find_storage(Rest, NewStorage). 

new_storage(Obj = #obj{id = Id, subclass = Subclass, state = State}) when (Subclass =:= <<"storage">>) and
                                                                          (State =:= none) ->

    Capacity = obj:get_capacity(Id),
    Capacity10 = erlang:trunc(Capacity * 0.10),

    lager:info("Capacity: ~p ~p", [Capacity, Capacity10]),

    case obj:has_space(Id, Capacity10) of
        true -> {found, Obj};
        false -> none
    end;
new_storage(_Obj) ->
    none.
process_morale(Villager, Obj) ->
    Food = case item:get_by_subclass(Obj#obj.id, ?FOOD) of
              [] -> false;
              [_Item | _Rest] -> true
           end,

    Dwelling = case Villager#villager.dwelling of
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


