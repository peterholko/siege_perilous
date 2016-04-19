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
-export([check_task/0, assign/2, remove/1, remove_structure/1]).
-export([create_plan/0, run_plan/0]).
-export([enemy_visible/1, move_to_dwelling/1]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, villager}, villager, [], []).

create_plan() ->
    gen_server:cast({global, villager}, create_plan).

run_plan() ->
    gen_server:cast({global, villager}, run_plan).

enemy_visible(Id) ->
    [Villager] = db:read(villager, Id),
    Villager#villager.enemy =/= none.

move_to_dwelling(Id) ->
    [Villager] = db:read(villager, Id),
    [VillagerObj] = db:read(obj, Id),

    case Villager#villager.enemy of
        none -> 
            %Enemy gone
            NewVillager = Villager#villager {task_state = completed}
            db:write(NewVillager);
        _ ->
            [Dwelling] = db:read(obj, Villager#villager.dwelling),

            Path = astar:astar(VillagerObj#obj.pos, Dwelling#obj.pos),

            NewVillager = Villager#villager {task_state = incomplete,
                                             path = Path},

            db:write(NewVillager),

            %Add move action
    end.

check_task() ->
    gen_server:cast({global, villager}, process).

assign(SourceId, TargetId) ->
    Villager = #villager {id = SourceId,
                          task = assign,
                          structure = TargetId},
    db:write(Villager).

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
    [Villager] = db:read(obj, VillagerId),

    case find_enemies(Villager, Objs) of
        [] -> nothing
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
    NewPlan = htn:plan(villager, Id),

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

    process_create_plan(mnesia:next(villager, Id)).

process_run_plan('$end_of_table') ->
    done;
process_run_plan(Id) ->
    [Villager] = db:read(villager, Id),

    case Villager#villager.task_state of
        completed ->
            TaskIndex = Villager#villager.task_index,
            PlanLength = length(Villager#villager.plan),

            NextTask = get_next_task(TaskIndex, PlanLength),

            case NextTask of
                {next_task, NextTaskIndex} ->
                    NewVillager = Villager#villager {task_index = NextTask}, 
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

    process_run_plan(mnesia:next(villager, Id)).

find_enemies(Villager, Objs) ->
    find_enemies(Villager, Objs, []).

find_enemies(_Villager, [], Enemies) ->
    Enemies;
find_enemies(Villager, [Obj | Rest], Enemies) ->
    NewEnemies = case Villager#obj.player =:= Obj#obj.player of
                     true -> Enemies;
                     false -> [Obj | Enemies]
                 end,

    find_enemies(Villager, Rest, NewEnemies).

process('$end_of_table') ->
    lager:debug("Done processing villagers");
process(Id) ->
    [Villager] = db:read(villager, Id),
    [Obj] = db:read(obj, Id),

    NewVillager0 = check_dwelling(Villager, Obj),
    NewVillager1 = process_morale(NewVillager0, Obj),
    NewVillager2 = process_state(NewVillager1, Obj),

    %Write only if changes
    case Villager =/= NewVillager2 of
        true -> db:write(NewVillager2);
        false -> nothing
    end,
    
    process(db:next(villager, Id)).

process_state(Villager, Obj = #obj{state = State}) when State =:= none ->
    NewVillager = check_morale(Villager, Obj),
    NewVillager;
process_state(Villager, _Obj) ->
    Villager.

check_morale(Villager = #villager{morale = Morale}, Obj) when Morale >= 50 ->
    %Morale high enough to process 
    process_task(Villager#villager.task, Villager, Obj),

    %Return villager
    Villager;
check_morale(Villager = #villager{morale = Morale}, Obj) when Morale >= 25 ->
    %Force task to forage due to low morale
    NewVillager = Villager#villager{task = forage},

    %Process forage
    process_task(forage, NewVillager, Obj),

    %Return NewVillager
    NewVillager;
check_morale(Villager = #villager{morale = Morale}, Obj) when Morale >= 0 ->
    %Switch player to natives player
    lager:info("Obj: ~p", [Obj]),
    [NewObj] = Obj#obj{player = ?NATIVES},
    db:write(NewObj),

    %Reset villager and return
    NewVillager = Villager#villager{task = forage,
                                    morale = 50}, 
    NewVillager.

process_task(assign, Villager, Obj) ->
    [Structure] = db:read(obj, Villager#villager.structure),

    %Check same position    
    SamePos = Structure#obj.pos =:= Obj#obj.pos,
    
    %Assign action
    Action = case SamePos of
                true ->
                    {harvest, Structure#obj.id};
                false ->
                    {move_to, Structure#obj.pos}
             end,

    process_action(Action, Villager, Obj);
process_task(gather, Villager, Obj) ->
    Action = case resource:survey(Obj#obj.pos) of
                [Resource | _Rest] ->
                    ResourceName = maps:get(<<"name">>, Resource),
                    {harvest, ResourceName};
                [] ->
                    Pos = map:get_random_neighbour(Obj#obj.pos),
                    {move_to, Pos}
             end,

    process_action(Action, Villager, Obj);
process_task(forage, Villager, Obj) ->
    Action = case resource:survey(Obj#obj.pos) of
                 [Resource | _Rest] ->
                     ResourceName = maps:get(<<"name">>, Resource),
                     
                     case item:is_subclass(ResourceName, ?FOOD) of
                         true -> 
                             {harvest, ResourceName};
                         false ->
                             Pos = map:get_random_neighbour(Obj#obj.pos),
                             {move_to, Pos}
                     end;
                 [] ->
                    Pos = map:get_random_neighbour(Obj#obj.pos),
                    {move_to, Pos}
             end,
    process_action(Action, Villager, Obj);

process_task(follow, _Villager, Obj) ->
    [Hero] = db:read(hero, Obj#obj.player),
    [HeroObj] = db:read(obj, Hero#hero.obj),
    Path = astar:astar(Obj#obj.pos, HeroObj#obj.pos),
    
    case Path of
        failure -> lager:info("~p could not find path", [Obj#obj.id]);
        PathList when length(PathList) =< 2 -> lager:info("Adjacent to following target");
        PathList ->
            NextPos = lists:nth(2, PathList),
            add_move_unit(Obj, NextPos)
    end.

process_action({move_to, Dest}, _Villager, Obj) ->
    Pathfinding = astar:astar(Obj#obj.pos, Dest),
    
    case Pathfinding of 
        failure ->
            lager:info("~p could not find path", [Obj#obj.id]);
        PathList ->
            NextPos = lists:nth(2, PathList),
            add_move_unit(Obj, NextPos)
    end;
process_action({harvest, ResourceName}, _Villager, Obj) ->
    obj:update_state(Obj#obj.id, harvesting),
    EventData = {Obj#obj.id, ResourceName, Obj#obj.pos, 25, false},
    game:add_event(self(), harvest, EventData, Obj#obj.id, 25);
process_action(_, _, _) ->
    nothing.

add_move_unit(Obj, NewPos) ->
    %Update unit state
    obj:update_state(Obj#obj.id, moving),
    
    %Create event data
    EventData = {Obj#obj.player,
                 Obj#obj.id,
                 NewPos},

    NumTicks = 16,
    lager:info("Villager add move ~p", [NewPos]),
    game:add_event(self(), move, EventData, Obj#obj.id, NumTicks).

check_dwelling(Villager = #villager{dwelling = Dwelling}, Obj) when Dwelling =:= none ->
    Objs = db:index_read(obj, Obj#obj.player, #obj.player),

    NewVillager = case find_dwelling(Objs, none) of
                      {found, ShelterId} ->
                          obj_attr:set(ShelterId, <<"claimed">>, <<"true">>),
                          Villager#villager{dwelling = ShelterId};
                      none -> 
                          Villager
                  end,
    NewVillager;
check_dwelling(Villager, _Obj) ->
    Villager.

find_dwelling(_Objs, {found, Dwelling}) ->
    {found, Dwelling};
find_dwelling([], none) ->
    none;
find_dwelling([Obj | Rest], _Dwelling) ->
    NewDwelling = new_dwelling(Obj),
    find_dwelling(Rest, NewDwelling).

new_dwelling(Obj = #obj{subclass = Subclass, state = State}) when (Subclass =:= <<"shelter">>) and 
                                                                  (State =:= none) ->
    Claimed = obj_attr:value(Obj#obj.id, <<"claimed">>, <<"false">>),

    NewDwelling = case Claimed of
                      <<"false">> -> {found, Obj#obj.id};
                      <<"true">> -> none
                  end,
    NewDwelling;
new_dwelling(_) -> 
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

