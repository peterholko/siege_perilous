%% -------------------------------------------------------------------
%% Author  : Peter Holko
%%% Description : NPC server
%%%
%%% Created : Jan 5, 2015
%%% -------------------------------------------------------------------
-module(npc).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([replan/1, run_plan/1, new_zombie/0, new_wolf/0, get_nearest/3]).
-export([target_visible/1, max_guard_dist/1]).
-export([move_random_pos/1, move_to_target/1, melee_attack/1, move_guard_pos/1]).
-export([add/1, remove/1]).
%% ====================================================================
%% External functions
%% ====================================================================

start(PlayerId) ->
    gen_server:start({global, {npc, PlayerId}}, npc, [PlayerId], []).

replan(PlayerId) ->
    gen_server:cast({global, {npc, PlayerId}}, replan).

run_plan(PlayerId) ->
    gen_server:cast({global, {npc, PlayerId}}, run_plan).

new_zombie() ->
    obj:create({18,18}, ?UNDEAD, unit, <<"npc">>, <<"Zombie">>, none).

new_wolf() ->
    obj:create({20,20}, ?UNDEAD, unit, <<"npc">>, <<"Wolf">>, none).

add(Id) ->
    NPC = #npc {id = Id},
    db:write(NPC).
remove(Id) ->
    db:delete(npc, Id).

%% ====================================================================
%% Server functions
%% ====================================================================

init([PlayerId]) ->
    %Store player id in process dict
    put(player_id, PlayerId),

    %Store npc process in connection table
    [Connection] = db:dirty_read(connection, PlayerId),
    NewConnection = Connection#connection {process = self()},
    db:dirty_write(NewConnection),

    {ok, []}.

handle_cast(create, Data) ->   
    
    {noreply, Data};

handle_cast(replan, Data) ->
    process_replan(mnesia:dirty_first(npc)),
    {noreply, Data};

handle_cast(run_plan, Data) ->
    process_run_plan(mnesia:dirty_first(npc)),
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

handle_info({map_perception_disabled, _Perception}, Data) ->
    {noreply, Data};

handle_info({perception, {NPCId, Objs}}, Data) ->
    lager:debug("Perception received."),
    [NPCObj] = db:read(obj, NPCId),
    [NPC] = db:read(npc, NPCId),
    NPCStats = obj:get(NPCObj#obj.id),

    %Remove from same player and non targetable objs
    FilteredTargets = filter_targets(Objs, []),

    %Find target
    Target = find_target(NPCObj, NPCStats, FilteredTargets),

    lager:debug("Find Target: ~p", [Target]),

    %Store target
    NewNPC = NPC#npc {target = Target},
    db:write(NewNPC),

    lager:debug("Perception processing end."),
    {noreply, Data};

handle_info({event_complete, {_EventId, Id}}, Data) ->
    [NPC] = db:read(npc, Id),
    Task = lists:nth(NPC#npc.task_index, NPC#npc.plan),

    obj:update_state(Id, none),

    case Task of
        move_random_pos ->
            NewNPC = NPC#npc {task_state = completed},
            db:write(NewNPC);
        move_to_target ->
            move_to_target(Id);
        move_guard_pos ->
            move_guard_pos(Id);
        melee_attack ->
            NewNPC = NPC#npc {task_state = completed},
            db:write(NewNPC);
        _ ->
            nothing
    end,
 
    {noreply, Data};

handle_info(_Info, Data) ->
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

terminate(_Reason, _) ->
    ok.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

filter_targets([], Targets) ->
    Targets;

filter_targets([PerceptionObj | Rest], Targets) ->
    Player = get(player_id),
    ObjPlayer = maps:get(<<"player">>, PerceptionObj),
    NewTargets = case valid_target(Player, ObjPlayer) of
                     true ->
                         Obj = get_obj(PerceptionObj),
                         [Obj | Targets];
                     false ->
                         Targets
                 end,

    filter_targets(Rest, NewTargets).

valid_target(_, -1) -> false;
valid_target(Player, ObjPlayer) when Player =:= ObjPlayer -> false;
valid_target(_, _) -> true.

get_obj(PerceptionObj) ->
    Id = maps:get(<<"id">>, PerceptionObj),
    [Obj] = db:read(obj, Id),
    Obj.

process_replan('$end_of_table') ->
    done;
process_replan(Id) ->
    [NPC] = db:read(npc, Id),

    CurrPlan = NPC#npc.plan,
    NewPlan = htn:plan(NPC#npc.orders, Id),
    case NewPlan =:= CurrPlan of
        false ->
            %New plan cancel current event
            game:cancel_event(Id),

            %Reset NPC variables
            NewNPC = NPC#npc {plan = NewPlan,
                              new_plan = true,
                              task_state = completed,
                              task_index = 0},
            db:write(NewNPC);
        true ->
            nothing
    end,

    process_replan(mnesia:dirty_next(npc, Id)).

process_run_plan('$end_of_table') ->
    done;
process_run_plan(Id) ->
    [NPC] = db:read(npc, Id),

    case NPC#npc.task_state of
        completed ->
            TaskIndex = NPC#npc.task_index,
            PlanLength = length(NPC#npc.plan),

            NextTask = get_next_task(TaskIndex, PlanLength),
            case NextTask of
                {next_task, NextTaskIndex} ->
                    NewNPC = NPC#npc {task_index = NextTaskIndex},
                    db:write(NewNPC),
                    erlang:apply(npc, lists:nth(NextTaskIndex, NPC#npc.plan) , [Id]);
                plan_completed ->
                    NewNPC = NPC#npc { task_state = completed,
                                       task_index = 0},
                    db:write(NewNPC)
            end;
        _ ->
            nothing
    end,

    process_run_plan(mnesia:dirty_next(npc, Id)).

get_next_task(TaskIndex, PlanLength) when TaskIndex < PlanLength ->
    NewTaskIndex = TaskIndex + 1,
    {next_task, NewTaskIndex};
get_next_task(_TaskIndex, _PlanLength) ->
    plan_completed.

move_next_path(_NPCObj, []) -> nothing;
move_next_path(NPCObj, Path) -> move_unit(NPCObj, lists:nth(2, Path)).

move_unit(#obj {id = Id, player = Player}, NewPos) ->
    NumTicks = ?TICKS_SEC * 8,

    %Update unit state
    obj:update_state(Id, moving),
    
    %Create event data
    EventData = {Player,
                 Id,
                 NewPos},

    game:add_event(self(), move, EventData, Id, NumTicks).

get_nearest(_NPCUnit, [], {EnemyUnit, _Distance}) ->
    EnemyUnit;
get_nearest(NPCPos, [NewEnemyUnit | EnemyUnits], {EnemyUnit, Distance}) ->
    NewEnemyUnitPos = NewEnemyUnit#obj.pos,

    CalcDistance = map:distance(NPCPos, NewEnemyUnitPos),
    {TargetEnemyUnit, NewDistance} = compare_distance(CalcDistance,
                                                      Distance,
                                                      NewEnemyUnit,
                                                      EnemyUnit),

    get_nearest(NPCPos, EnemyUnits, {TargetEnemyUnit, NewDistance}).
        
compare_distance(NewDistance, Distance, _New, Old) when NewDistance >= Distance ->
    {Old, Distance};
compare_distance(NewDistance, Distance, New, _Old) when NewDistance < Distance ->
    {New, NewDistance}.

remove_walled(ObjList) ->
    F = fun(Obj) ->
            IsWalled = obj:has_effect(Obj#obj.id, ?WALL),
            not IsWalled
        end,
    lists:filter(F, ObjList).

remove_dead(ObjList) ->
    F = fun(Obj) ->
            Obj#obj.state =/= dead
        end,
    lists:filter(F, ObjList).

remove_structures(ObjList) ->
    F = fun(Obj) ->
            Obj#obj.class =/= structure
        end,
    lists:filter(F, ObjList).

get_wander_pos(_, _, []) ->
    none;
get_wander_pos(true, RandomPos, _Neighbours) ->
    RandomPos;
get_wander_pos(false,  _, Neighbours) ->
    Random = rand:uniform(length(Neighbours)),
    RandomPos = lists:nth(Random, Neighbours),
    IsEmpty = obj:is_empty(RandomPos),

    NewNeighbours = lists:delete(RandomPos, Neighbours),

    get_wander_pos(IsEmpty, RandomPos, NewNeighbours).

check_wall(#obj{id = Id} = EnemyUnit) ->    
    Effect = db:read(effect, {Id, ?WALL}),

    Target = case Effect =/= [] of
                true ->
                    [EffectR] = Effect,
                    WallId = EffectR#effect.data,
                    [Wall] = db:read(obj, WallId),
                    Wall;
                false ->
                    EnemyUnit
             end,
    Target;

check_wall(_) ->
    none.

find_target(NPCObj, NPCStats, AllEnemyUnits) ->
    Int = maps:get(<<"int">>, NPCStats),
    Aggression = maps:get(<<"aggression">>, NPCStats),
    find_target(NPCObj, Int, Aggression, AllEnemyUnits).

find_target(_NPCObj, _, _, []) ->
    none;
find_target(NPCObj, <<"mindless">>, <<"high">>, AllEnemyUnits) ->
    EnemyUnits = remove_structures(remove_dead(AllEnemyUnits)),
    EnemyUnit = get_nearest(NPCObj#obj.pos, EnemyUnits, {none, 1000}),
    Target = check_wall(EnemyUnit),
    return_target(Target);
find_target(NPCObj, <<"animal">>, <<"high">>, AllEnemyUnits) ->
    EnemyUnits = remove_structures(remove_dead(remove_walled(AllEnemyUnits))),
    EnemyUnit = get_nearest(NPCObj#obj.pos, EnemyUnits, {none, 1000}),
    return_target(EnemyUnit). 

return_target(Target) when is_record(Target, obj) ->
    Target#obj.id;
return_target(_) ->
    none.

move_random_pos(NPCId) ->
    [NPC] = db:read(npc, NPCId),
    [NPCObj] = db:dirty_read(obj, NPCId),
    {X, Y} = NPCObj#obj.pos,

    Neighbours = map:neighbours(X, Y),
    NewPos = get_wander_pos(false, none, Neighbours),
   
    NewNPC = NPC#npc {task_state = inprogress},
    db:write(NewNPC),

    move_unit(NPCObj, NewPos).

target_visible(NPCId) ->
    [NPC] = db:read(npc, NPCId),
    NPC#npc.target =/= none.

move_to_target(NPCId) ->
    [NPC] = db:read(npc, NPCId), 
    [NPCObj] = db:read(obj, NPCId),

    case NPC#npc.target of
        none ->
            %Invalid target due to either moving out of range or dying
            NewNPC = NPC#npc {task_state = completed},
            db:write(NewNPC);
        _ -> 
            [TargetObj] = db:read(obj, NPC#npc.target),

            IsAdjacent = map:is_adjacent(NPCObj#obj.pos, TargetObj#obj.pos),

            case IsAdjacent of
                false ->
                    Path = astar:astar(NPCObj#obj.pos, TargetObj#obj.pos),
                    NewNPC = NPC#npc {task_state = inprogress,
                                      path = Path},
                    db:write(NewNPC),

                    move_next_path(NPCObj, Path);
                true ->
                    NewNPC = NPC#npc {task_state = completed},
                    db:write(NewNPC)
            end
    end.
    
melee_attack(NPCId) ->
    [NPC] = db:read(npc, NPCId),

    CurrentAttacks = NPC#npc.attacks,
    AttackType = get_attack_type(NPC),

    combat:attack(AttackType, NPCId, NPC#npc.target),

    EventData = NPCId,
    game:add_event(self(), attack, EventData, NPCId, 16),

    NewNPC = NPC#npc {task_state = inprogress,
                      attacks = store_attacks(AttackType, CurrentAttacks)},
    db:write(NewNPC).

move_guard_pos(NPCId) ->
    [NPC] = db:read(npc, NPCId),
    [NPCObj] = db:read(obj, NPCId),

    GuardPos = NPC#npc.orders_data,

    case NPCObj#obj.pos =:= GuardPos of
        false ->
            Path = astar:astar(NPCObj#obj.pos, GuardPos),
            NewNPC = NPC#npc {task_state = inprogress,
                              path = Path},
            db:write(NewNPC),

            move_next_path(NPCObj, Path);
        true ->
            NewNPC = NPC#npc {task_state = completed},
            db:write(NewNPC)
    end.

max_guard_dist(NPCId) ->
    [NPC] = db:read(npc, NPCId),
    [NPCObj] = db:read(obj, NPCId),

    GuardPos = NPC#npc.orders_data,
    NPCPos = NPCObj#obj.pos,

    map:distance(GuardPos, NPCPos) > 3. 

get_attack_type(NPC) ->
    case NPC#npc.attacks of
        [] -> ?QUICK;
        [?QUICK] -> ?QUICK;
        [?QUICK, ?QUICK] -> ?QUICK;
        [?QUICK, ?QUICK, ?QUICK] -> ?FIERCE
    end.

store_attacks(?QUICK, Attacks) -> [?QUICK | Attacks];
store_attacks(?FIERCE, Attacks) -> [].


