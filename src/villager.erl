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
-export([enemy_visible/1, move_to_pos/1, has_dwelling/1, hero_nearby/1]).
-export([set_pos_dwelling/1, set_pos_hero/1, set_pos_structure/1]).
-export([morale_normal/1, morale_low/1, morale_very_low/1]).
-export([order_follow/1, dwelling_needed/1, claim_dwelling/1, find_dwelling/1]).
-export([structure_needed/1, find_structure/1, has_structure/1, harvest/1]).
-export([find_structure/2, claim_structure/1]).

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

%%% 
%%% HTN Primitive Tasks
%%%
set_pos_dwelling(Id) ->
    [Villager] = db:read(villager, Id),
    [Dwelling] = db:read(obj, Villager#villager.dwelling),

    NewVillager = Villager#villager {dest = Dwelling#obj.pos, task_state = completed},
    db:write(NewVillager).

set_pos_structure(Id) ->
    [Villager] = db:read(villager, Id),
    [Structure] = db:read(obj, Villager#villager.structure),

    NewVillager = Villager#villager {dest = Structure#obj.pos, task_state = completed},
    db:write(NewVillager).

set_pos_hero(Id) ->
    [Villager] = db:read(villager, Id),
    [VillagerObj] = db:read(obj, Id),

    Hero = obj:get_hero(VillagerObj#obj.player),
    
    NewVillager = Villager#villager {dest = Hero#obj.pos, task_state = completed},
    db:write(NewVillager).

move_to_pos(Id) ->
    [Villager] = db:read(villager, Id),
    [VillagerObj] = db:read(obj, Id),

    Dest = Villager#villager.dest,

    %If dest is set and dest does not equal villager current pos
    NewVillager = case (Dest =/= none) and (Dest =/= VillagerObj#obj.pos) of
                      true ->
                         Path = astar:astar(VillagerObj#obj.pos, Dest),

                         %Add move action to next path location
                         move_next_path(VillagerObj, Path),

                         Villager#villager {task_state = inprogress, path = Path};
                      false ->
                         Villager#villager {task_state = completed}
                 end,

    db:write(NewVillager).

morale(Id, Value) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.morale >= Value.

order_follow(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.order =:= follow.

find_dwelling(Id) ->
    [Villager] = db:read(villager, Id),
    [VillagerObj] = db:read(obj, Id),

    Objs = db:index_read(obj, VillagerObj#obj.player, #obj.player),

    NewVillager = case find_dwelling(Objs, none) of
                      {found, Dwelling} -> 
                          Villager#villager {dest = Dwelling#obj.pos};
                      _ -> 
                          nothing
                  end,

    db:write(NewVillager#villager{task_state = completed}).

claim_dwelling(Id) ->
    [Villager] = db:read(villager, Id), 
    [VillagerObj] = db:read(obj, Id),

    Objs = db:index_read(obj, VillagerObj#obj.player, #obj.player),

    NewVillager = case find_dwelling(Objs, none) of
                      {found, Dwelling} ->
                          case Dwelling#obj.pos =:= VillagerObj#obj.pos of
                              true ->
                                  Villager#villager{dwelling = Dwelling#obj.id};
                              false ->
                                  nothing
                          end;
                      _ ->
                          nothing
                  end, 
    
    db:write(NewVillager#villager{task_state = completed}).

find_structure(Id) ->
    [Villager] = db:read(villager, Id), 
    [VillagerObj] = db:read(obj, Id),

    Objs = db:index_read(obj, VillagerObj#obj.player, #obj.player),

    NewVillager = case find_structure(Objs, none) of
                      {found, Structure} -> 
                          Villager#villager {structure = Structure#obj.id,
                                             dest = Structure#obj.pos};
                      _ -> 
                          nothing
                  end,

    db:write(NewVillager#villager{task_state = completed}).

claim_structure(Id) ->
    [Villager] = db:read(villager, Id), 
    [VillagerObj] = db:read(obj, Id),

    Objs = db:index_read(obj, VillagerObj#obj.player, #obj.player),

    NewVillager = case find_structure(Objs, none) of
                      {found, Structure} ->
                          case Structure#obj.pos =:= VillagerObj#obj.pos of
                              true ->
                                  Villager#villager{structure = Structure#obj.id};
                              false ->
                                  nothing
                          end;
                      _ ->
                          nothing
                  end, 
    
    db:write(NewVillager#villager{task_state = completed}).

harvest(Id) ->
    [Villager] = db:read(villager, Id),
    EventData = {Id, Villager#villager.structure},
    NumTicks = ?TICKS_SEC * 8,
    game:add_event(self(), sharvest, EventData, Id, NumTicks).

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
                                             task_state = completed,
                                             task_index = 0},
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

    case Villager#villager.task_state of
        completed ->
            TaskIndex = Villager#villager.task_index,
            PlanLength = length(Villager#villager.plan),

            lager:info("Villager: ~p ~p", [TaskIndex, PlanLength]),
            NextTask = get_next_task(TaskIndex, PlanLength),

            case NextTask of
                {next_task, NextTaskIndex} ->
                    NewVillager = Villager#villager {task_index = NextTaskIndex}, 
                    db:write(NewVillager),

                    TaskToRun = lists:nth(NextTaskIndex, Villager#villager.plan),

                    erlang:apply(villager, TaskToRun, [Id]);
                plan_completed ->
                    NewVillager = Villager#villager {task_state = completed,
                                                     task_index = 0},
                    db:write(NewVillager)
            end;
        _ ->
            nothing
    end,

    process_run_plan(db:next(villager, Id)).

process_event_complete([Villager]) ->
    Id = Villager#villager.id,

    lager:info("Villager: ~p ~p", [Villager#villager.task_index, length(Villager#villager.plan)]),
    Task = lists:nth(Villager#villager.task_index, Villager#villager.plan),
    obj:update_state(Id, none),

    case Task of
        move_to_pos -> move_to_pos(Id);
        claim_dwelling -> complete_task(Villager);
        harvest -> complete_task(Villager);
        _ -> nothing
    end;
process_event_complete([]) -> nothing.

complete_task(Villager) ->
    NewVillager = Villager#villager {task_state = completed},
    db:write(NewVillager).

find_enemies(Villager, Objs) ->
    find_enemies(Villager, Objs, []).

find_enemies(_Villager, [], Enemies) ->
    Enemies;
find_enemies(Villager, [PerceptionObj | Rest], Enemies) ->
    Player = maps:get(<<"player">>, PerceptionObj),
    NewEnemies = case Villager#obj.player =:= Player of
                     true -> Enemies;
                     false -> [PerceptionObj | Enemies]
                 end,

    find_enemies(Villager, Rest, NewEnemies).

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
new_structure(Obj) -> 
    lager:info("Obj: ~p", [Obj]),
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

move_next_path(_Obj, []) -> nothing;
move_next_path(Obj, Path) -> move_unit(Obj, lists:nth(2, Path)).

move_unit(#obj {id = Id, player = Player}, NewPos) when is_tuple(NewPos) ->
    NumTicks = ?TICKS_SEC * 8,

    %Update unit state
    obj:update_state(Id, moving),
    
    %Create event data
    EventData = {Player, Id, NewPos},

    game:add_event(self(), move, EventData, Id, NumTicks);
move_unit(_Obj, _) -> invalid_pos.

%process_task(assign, Villager, Obj) ->
%    [Structure] = db:read(obj, Villager#villager.structure),
%
%    %Check same position    
%    SamePos = Structure#obj.pos =:= Obj#obj.pos,
%    
%    %Assign action
%    Action = case SamePos of
%                true ->
%                    {harvest, Structure#obj.id};
%                false ->
%                    {move_to, Structure#obj.pos}
%             end,
%
%    process_action(Action, Villager, Obj);
%process_task(gather, Villager, Obj) ->
%    Action = case resource:survey(Obj#obj.pos) of
%                [Resource | _Rest] ->
%                    ResourceName = maps:get(<<"name">>, Resource),
%                    {harvest, ResourceName};
%                [] ->
%                    Pos = map:get_random_neighbour(Obj#obj.pos),
%                    {move_to, Pos}
%             end,
%
%    process_action(Action, Villager, Obj);
%process_task(forage, Villager, Obj) ->
%    Action = case resource:survey(Obj#obj.pos) of
%                 [Resource | _Rest] ->
%                     ResourceName = maps:get(<<"name">>, Resource),
%                     
%                     case item:is_subclass(ResourceName, ?FOOD) of
%                         true -> 
%                             {harvest, ResourceName};
%                         false ->
%                             Pos = map:get_random_neighbour(Obj#obj.pos),
%                             {move_to, Pos}
%                     end;
%                 [] ->
%                    Pos = map:get_random_neighbour(Obj#obj.pos),
%                    {move_to, Pos}
%             end,
%    process_action(Action, Villager, Obj).
%
%process_action({harvest, ResourceName}, _Villager, Obj) ->
%    obj:update_state(Obj#obj.id, harvesting),
%    EventData = {Obj#obj.id, ResourceName, Obj#obj.pos, 25, false},
%    game:add_event(self(), harvest, EventData, Obj#obj.id, 25);
%process_action(_, _, _) ->
%    nothing.

