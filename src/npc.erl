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
-export([replan/1, run_plan/1, get_nearest/3]).
-export([hp_normal/1, hp_very_low/1, target_visible/1, target_adjacent/1, max_guard_dist/1]).
-export([set_pos_flee/1, move_random_pos/1, move_to_target/1, melee_attack/1, move_to_order_pos/1]).
-export([get_player_id/1, add/1, remove/1, set_order/3, create/2]).
%% ====================================================================
%% External functions
%% ====================================================================

start(PlayerId) ->
    gen_server:start({global, {npc, PlayerId}}, npc, [PlayerId], []).

replan(PlayerId) ->
    gen_server:cast({global, {npc, PlayerId}}, replan).

run_plan(PlayerId) ->
    gen_server:cast({global, {npc, PlayerId}}, run_plan).

get_player_id(NPCType) ->
    [NPCPlayer] = db:index_read(player, NPCType, #player.name),
    NPCPlayer#player.id.

add(Id) ->
    NPC = #npc {id = Id},
    db:write(NPC).
remove(Id) ->
    db:delete(npc, Id).

set_order(Id, Orders, OrdersData) ->
    [NPC] = db:read(npc, Id),

    NewNPC = NPC#npc {order = Orders, 
                      order_data = OrdersData},
    db:write(NewNPC).

create(Pos, Name) ->
    Type = obj_def:value(Name, <<"npc_type">>),
    PlayerId = get_player_id(Type),
    Id = obj:create(Pos, PlayerId, unit, <<"npc">>, Name, none),
    Id.

%% HTN Functions %%%

hp_normal(NPCId) ->
    NPCHp = obj_attr:value(NPCId, <<"hp">>),
    NPCBaseHp = obj_attr:value(NPCId, <<"base_hp">>),
    
    HpNormal = (NPCHp / NPCBaseHp) > 0.20,
    HpNormal.

hp_very_low(NPCId) ->
    NPCHp = obj_attr:value(NPCId, <<"hp">>),
    NPCBaseHp = obj_attr:value(NPCId, <<"base_hp">>),
    
    HpNormal = (NPCHp / NPCBaseHp) < 0.20,
    HpNormal.

set_pos_flee(NPCId) ->
    [NPC] = db:read(npc, NPCId),
    [NPCObj] = db:read(obj, NPCId),

    Radius = 5,
    RandomPos = map:random_location_from(NPCObj#obj.player, NPCObj#obj.pos, Radius),

    NewNPC = NPC#npc {order_data = RandomPos},
    db:write(NewNPC).

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

target_adjacent(NPCId) ->
    [NPC] = db:read(npc, NPCId), 
    [NPCObj] = db:read(obj, NPCId),

    case NPC#npc.target =/= none of
        true -> 
            [TargetObj] = db:read(obj, NPC#npc.target),
            map:is_adjacent(NPCObj#obj.pos, TargetObj#obj.pos);
        false -> 
            false
    end.

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
                    Path = astar:astar(NPCObj#obj.pos, TargetObj#obj.pos, NPCObj),
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
    lager:debug("Melee_attack: ~p", [NPCId]),
    [NPC] = db:read(npc, NPCId),
    [NPCObj] = db:read(obj, NPCId),

    TargetObj = combat:is_valid_target(NPC#npc.target),

    Checks = TargetObj =/= false andalso
             map:is_adjacent(NPCObj#obj.pos, TargetObj#obj.pos) andalso
             combat:is_target_alive(TargetObj) andalso
             combat:is_targetable(TargetObj),

    lager:debug("Checks: ~p", [Checks]),
    NewNPC = case Checks of
                 true ->
                     Attacks = case NPC#npc.attacks =:= NPC#npc.combo of
                                   true -> [];
                                   false -> NPC#npc.attacks
                               end,

                     Combo = case Attacks of
                                 [] -> get_combo(NPCObj);
                                 _ -> NPC#npc.combo
                             end,

                     NextAttack = get_next_attack(Attacks, Combo),
                     
                     combat:attack(NextAttack, NPCId, NPC#npc.target),

                     EventData = NPCId,
                     game:add_event(self(), attack, EventData, NPCId, util:rand(16) + 6),

                     NPC#npc {task_state = inprogress,        
                              combo = Combo,                      
                              attacks = Attacks ++ [NextAttack]};
                 false ->
                     NPC#npc {task_state = completed}
             end,

    db:write(NewNPC).

move_to_order_pos(NPCId) ->
    [NPC] = db:read(npc, NPCId),
    [NPCObj] = db:read(obj, NPCId),

    Pos = NPC#npc.order_data,

    case NPCObj#obj.pos =:= Pos of
        false ->
            Path = astar:astar(NPCObj#obj.pos, Pos, NPCObj),
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

    GuardPos = NPC#npc.order_data,
    NPCPos = NPCObj#obj.pos,

    map:distance(GuardPos, NPCPos) > 3. 


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

    perception(NPCId, Objs),

    lager:debug("Perception processing end."),
    {noreply, Data};

handle_info({event_complete, {_EventId, Id}}, Data) ->
    NPC = db:read(npc, Id),

    process_event_complete(NPC),
    {noreply, Data};
handle_info({event_cancelled, {EventId, Id}}, Data) ->
    lager:info("NPC Event cancelled ~p ~p", [EventId, Id]),
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
                         add_obj(PerceptionObj, Targets);
                     false ->
                         Targets
                 end,

    filter_targets(Rest, NewTargets).

valid_target(_, -1) -> false;
valid_target(Player, ObjPlayer) when Player =:= ObjPlayer -> false;
valid_target(_, _) -> true.


add_obj(PerceptionObj, AllObjs) ->
    Id = maps:get(<<"id">>, PerceptionObj),
    case db:read(obj, Id) of
        [Obj] -> [Obj | AllObjs];
        [] -> AllObjs
    end.

process_replan('$end_of_table') ->
    done;
process_replan(Id) ->
    [NPC] = db:read(npc, Id),

    CurrPlan = NPC#npc.plan,
    NewPlan = htn:plan(NPC#npc.order, Id, npc),
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

    lager:debug("Task State: ~p", [NPC#npc.task_state]),
    case NPC#npc.task_state of
        completed ->
            TaskIndex = NPC#npc.task_index,
            PlanLength = length(NPC#npc.plan),

            NextTask = get_next_task(TaskIndex, PlanLength),
            lager:debug("NextTask: ~p", [NextTask]),
            case NextTask of
                {next_task, NextTaskIndex} ->
                    NewNPC = NPC#npc {task_index = NextTaskIndex},
                    db:write(NewNPC),
                    TaskToRun = lists:nth(NextTaskIndex, NPC#npc.plan),
                    lager:debug("TaskToRun: ~p", [TaskToRun]),
                    erlang:apply(npc, TaskToRun, [Id]);
                plan_completed ->
                    NewNPC = NPC#npc { task_state = completed,
                                       task_index = 0},
                    db:write(NewNPC)
            end;
        _ ->
            nothing
    end,

    process_run_plan(mnesia:dirty_next(npc, Id)).

process_event_complete([NPC]) ->
    Task = lists:nth(NPC#npc.task_index, NPC#npc.plan),

    obj:update_state(NPC#npc.id, none),

    case Task of
        move_random_pos ->
            NewNPC = NPC#npc {task_state = completed},
            db:write(NewNPC);
        move_to_target ->
            move_to_target(NPC#npc.id);
        move_to_order_pos ->
            move_to_order_pos(NPC#npc.id);
        melee_attack ->
            NewNPC = NPC#npc {task_state = completed},
            db:write(NewNPC);
        _ ->
            nothing
    end;
process_event_complete(_) -> 
    nothing.

get_next_task(TaskIndex, PlanLength) when TaskIndex < PlanLength ->
    NewTaskIndex = TaskIndex + 1,
    {next_task, NewTaskIndex};
get_next_task(_TaskIndex, _PlanLength) ->
    plan_completed.

move_next_path(_NPCObj, []) -> nothing;
move_next_path(NPCObj, Path) -> move_unit(NPCObj, lists:nth(2, Path)).

move_unit(#obj {id = Id, player = Player}, NewPos) when is_tuple(NewPos) ->
    NumTicks = ?TICKS_SEC * 8,

    %Update unit state
    obj:update_state(Id, moving),
    
    %Create event data
    EventData = {Player,
                 Id,
                 NewPos},

    game:add_event(self(), move, EventData, Id, NumTicks);
move_unit(_Obj, _) -> invalid_pos.

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

remove_fortified(ObjList) ->
    F = fun(Obj) ->
            IsFortified = effect:has_effect(Obj#obj.id, ?FORTIFIED),
            not IsFortified
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

remove_poi(ObjList) ->
    F = fun(Obj) ->
            Obj#obj.class =/= poi
        end,
    lists:filter(F, ObjList).

get_wander_pos(_, _, []) ->
    none;
get_wander_pos(true, RandomPos, _Neighbours) ->
    RandomPos;
get_wander_pos(false,  _, Neighbours) ->
    Random = util:rand(length(Neighbours)),
    RandomPos = lists:nth(Random, Neighbours),
    IsEmpty = obj:is_empty(RandomPos),

    NewNeighbours = lists:delete(RandomPos, Neighbours),

    get_wander_pos(IsEmpty, RandomPos, NewNeighbours).

check_wall(#obj{id = Id} = EnemyUnit) ->    
    case effect:get_effect_data(Id, ?FORTIFIED) of
        invalid -> EnemyUnit;
        WallId -> obj:get(WallId)
    end;
check_wall(_) ->
    none.

find_target(NPCObj, AllEnemyUnits) ->
    Int = obj_attr:value(NPCObj#obj.id, <<"int">>),
    Aggression = obj_attr:value(NPCObj#obj.id, <<"aggression">>),
    find_target(NPCObj, Int, Aggression, AllEnemyUnits).

find_target(_NPCObj, _, _, []) ->
    none;
find_target(NPCObj, <<"mindless">>, <<"high">>, AllEnemyUnits) ->
    EnemyUnits = remove_poi(remove_structures(remove_dead(AllEnemyUnits))),
    EnemyUnit = get_nearest(NPCObj#obj.pos, EnemyUnits, {none, 1000}),
    Target = check_wall(EnemyUnit),
    return_target(Target);
find_target(NPCObj, <<"animal">>, <<"high">>, AllEnemyUnits) ->
    EnemyUnits = remove_poi(remove_structures(remove_dead(remove_fortified(AllEnemyUnits)))),
    EnemyUnit = get_nearest(NPCObj#obj.pos, EnemyUnits, {none, 1000}),
    return_target(EnemyUnit). 

return_target(Target) when is_record(Target, obj) ->
    Target#obj.id;
return_target(_) ->
    none.


get_combo(NPCObj) ->
    Rand = util:rand(100),
    combo(NPCObj#obj.name, Rand).

get_next_attack(Attacks, Combo) ->
    RemainingAttacks = lists:subtract(Combo, Attacks),
    [NextAttack | _] = RemainingAttacks,
    NextAttack.

combo(<<"Shadow">>, Num) when Num < 50 -> [?QUICK, ?PRECISE, ?FIERCE, ?QUICK];
combo(<<"Shadow">>, _) -> [?PRECISE, ?FIERCE, ?PRECISE, ?PRECISE];
combo(_, _) -> [?QUICK, ?QUICK, ?QUICK, ?FIERCE].

perception(NPCId, Objs) ->
    [NPCObj] = db:read(obj, NPCId),
    [NPC] = db:read(npc, NPCId),

    %Remove from same player and non targetable objs
    FilteredTargets = filter_targets(Objs, []),

    %Find target
    Target = find_target(NPCObj, FilteredTargets),

    lager:debug("Find Target: ~p", [Target]),

    %Store target
    NewNPC = NPC#npc {target = Target},
    db:write(NewNPC).
