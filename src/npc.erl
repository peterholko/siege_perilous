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
-export([replan/2, run_plan/2, get_nearest/3]).
-export([hp_normal/1, hp_very_low/1, target_visible/1, target_adjacent/1, max_guard_dist/1]).
-export([set_pos_flee/1, move_random_pos/1, move_to_target/1, melee_attack/1, move_to_order_pos/1]).
-export([get_player_id/1, add/1, remove/1, set_order/3, create/2]).
%% ====================================================================
%% External functions
%% ====================================================================

start(PlayerId) ->
    gen_server:start({global, {npc, PlayerId}}, npc, [PlayerId], []).

replan(PlayerId, Tick) ->
    gen_server:cast({global, {npc, PlayerId}}, {replan, Tick}).

run_plan(PlayerId, Tick) ->
    gen_server:cast({global, {npc, PlayerId}}, {run_plan, Tick}).

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

create(Pos, Template) ->
    Family = obj_template:value(Template, <<"family">>),
    PlayerId = get_player_id(Family),
    Id = obj:create(Pos, PlayerId, Template),
    Id.

%% HTN Conditions %%%

target_visible(Id) ->
    [NPC] = db:read(npc, Id),
    NPC#npc.target =/= none.

max_guard_dist(Id) ->
    [NPC] = db:read(npc, Id),
    [NPCObj] = db:read(obj, NPC#npc.id),

    GuardPos = NPC#npc.order_data,
    NPCPos = NPCObj#obj.pos,

    map:distance(GuardPos, NPCPos) > 3. 

target_adjacent(Id) ->
    [NPC] = db:read(npc, Id),
    [NPCObj] = db:read(obj, NPC#npc.id),

    case NPC#npc.target =/= none of
        true -> 
            [TargetObj] = db:read(obj, NPC#npc.target),
            map:is_adjacent(NPCObj#obj.pos, TargetObj#obj.pos);
        false -> 
            false
    end.

hp_normal(Id) ->
    [NPC] = db:read(npc, Id),
    NPCHp = obj_attr:value(NPC#npc.id, <<"hp">>),
    NPCBaseHp = obj_attr:value(NPC#npc.id, <<"base_hp">>),
    
    HpNormal = (NPCHp / NPCBaseHp) > 0.20,
    HpNormal.

hp_very_low(Id) ->
    [NPC] = db:read(npc, Id),
    NPCHp = obj_attr:value(NPC#npc.id, <<"hp">>),
    NPCBaseHp = obj_attr:value(NPC#npc.id, <<"base_hp">>),
    
    HpNormal = (NPCHp / NPCBaseHp) < 0.20,
    HpNormal.

%%% HTN Primitives %%%

set_pos_flee(NPC) ->
    [NPCObj] = db:read(obj, NPC#npc.id),

    Radius = 5,
    RandomPos = map:random_location_from(NPCObj#obj.player, NPCObj#obj.pos, Radius),

    NewNPC = NPC#npc {order_data = RandomPos},
    db:write(NewNPC).

move_random_pos(NPC) ->
    [NPCObj] = db:dirty_read(obj, NPC#npc.id),
    {X, Y} = NPCObj#obj.pos,

    Neighbours = map:neighbours(X, Y),
    NewPos = get_wander_pos(NPCObj, false, none, Neighbours),
   
    move_unit(NPCObj, NewPos),

    NPC#npc {task_state = running}.

move_to_target(NPC) ->
    [NPCObj] = db:read(obj, NPC#npc.id),

    case NPC#npc.target of
        none ->
            %Invalid target due to either moving out of range or dying
            NPC#npc {task_state = completed};
        _ -> 
            [TargetObj] = db:read(obj, NPC#npc.target),

            IsAdjacent = map:is_adjacent(NPCObj#obj.pos, TargetObj#obj.pos),

            case IsAdjacent of
                false ->
                    Path = astar:astar(NPCObj#obj.pos, TargetObj#obj.pos, NPCObj),
                    move_next_path(NPCObj, Path),

                    NPC#npc {task_state = running,
                             path = Path};
                true ->
                    NPC#npc {task_state = completed}
            end
    end.

melee_attack(NPC) ->
    lager:debug("Melee_attack: ~p", [NPC]),
    [NPCObj] = db:read(obj, NPC#npc.id),

    TargetObj = combat:is_valid_target(NPC#npc.target),

    Checks = TargetObj =/= false andalso
             map:is_adjacent(NPCObj#obj.pos, TargetObj#obj.pos) andalso
             combat:is_target_alive(TargetObj) andalso
             combat:is_targetable(TargetObj),

    lager:debug("Checks: ~p", [Checks]),
    case Checks of
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
             
             combat:attack(NextAttack, NPC#npc.id, NPC#npc.target),

             EventData = NPC#npc.id,
             game:add_event(self(), attack, EventData, NPC#npc.id, util:rand(16) + 6),

             NPC#npc {task_state = running,
                      combo = Combo,                      
                      attacks = Attacks ++ [NextAttack]};
         false ->
             NPC#npc {task_state = completed}
    end.
    
move_to_order_pos(NPC) ->
    [NPCObj] = db:read(obj, NPC#npc.id),

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

handle_cast({replan, Tick}, Data) ->
    NPCList = db:dirty_index_read(npc, Tick, #npc.nextplan),

    F = fun(NPC) ->
            process_replan(NPC)
        end,

    lists:foreach(F, NPCList),

    {noreply, Data};

handle_cast({run_plan, Tick}, Data) ->
    NPCList = db:dirty_index_read(npc, Tick, #npc.nextrun),
    
    F = fun(NPC) ->
            process_run_plan(NPC)
        end,

    lists:foreach(F, NPCList),
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

handle_info({perception, _NPCId}, Data) ->
    lager:debug("Perception received."),
    lager:debug("Perception processing end."),
    {noreply, Data};

handle_info({event_complete, {_Event, Id}}, Data) ->
    lager:info("NPC received event_complete"),
    NPC = db:read(npc, Id),

    %Determine next task or if NPC is dead do nothing
    process_event_complete(NPC),

    {noreply, Data};
handle_info({event_cancel, {EventId, Id}}, Data) ->
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
filter_targets(PlayerId, AllPerception) ->
    F = fun(TargetId, Targets) ->
            TargetObj = obj:get(TargetId),

            case valid_target(PlayerId, obj:player(TargetObj)) of
                true ->
                    [TargetObj | Targets];
                false ->
                    Targets
            end
        end,

    FilteredTargets = lists:foldl(F, [], AllPerception),
    FilteredTargets.

valid_target(_, -1) -> false;
valid_target(SrcPlayer, TargetPlayer) when SrcPlayer =:= TargetPlayer -> false; %Ignore same player
valid_target(_SrcPlayer, TargetPlayer) when TargetPlayer < ?NPC_ID -> false; %Ignore other npc
valid_target(_, _) -> true.

process_replan(NPC) ->
    process_perception(NPC#npc.id),

    CurrPlan = NPC#npc.plan,
    NewPlan = htn:plan(NPC#npc.order, NPC#npc.id, npc),
    case NewPlan =:= CurrPlan of
        false ->
            %New plan cancel current event
            game:cancel_event(NPC#npc.id),

            %Next plan tick 
            NextPlanTick = NPC#npc.nextplan + (2 * ?TICKS_SEC),

            %Reset NPC variables
            NewNPC = NPC#npc {plan = NewPlan,
                              new_plan = true,
                              task_state = completed,
                              task_index = 1,
                              nextplan = NextPlanTick},
            db:write(NewNPC);
        true ->
            nothing
    end.

process_run_plan(NPC) ->

    case NPC#npc.plan of
        [] -> nothing;
        _ -> %Process npc task state
            NextRunTick = NPC#npc.nextrun + (2 *?TICKS_SEC),

            NewNPC = process_task_state(NPC#npc.task_state, NPC),
            NewNPC2 = NewNPC#npc {nextrun = NextRunTick},

            db:write(NewNPC2)
    end.

process_task_state(init, NPC) ->
    TaskToRun = lists:nth(1, NPC#npc.plan),
    lager:info("NPC: ~p Running task: ~p", [NPC#npc.id, TaskToRun]),
    NewNPC = erlang:apply(npc, TaskToRun, [NPC]),
    NewNPC;
process_task_state(completed, NPC) ->
    TaskIndex = NPC#npc.task_index,
    PlanLength = length(NPC#npc.plan),

    NextTask = get_next_task(TaskIndex, PlanLength),
    lager:info("NPC: ~p NextTask: ~p ", [NPC#npc.id, NextTask]),

    case NextTask of
        {next_task, NextTaskIndex} ->
            TaskToRun = lists:nth(NextTaskIndex, NPC#npc.plan),
            NewNPC = erlang:apply(npc, TaskToRun, [NPC]),
            lager:info("NewNPC: ~p", [NewNPC]),
            NewNPC#npc{task_index = NextTaskIndex};
        plan_completed -> 
            NPC#npc{task_state = init, task_index = 1}
    end;
process_task_state(running, NPC) ->
    NPC.

process_event_complete([NPC]) ->
    Task = lists:nth(NPC#npc.task_index, NPC#npc.plan),

    obj:update_state(NPC#npc.id, none),

    case Task of
        move_random_pos ->
            complete_task(NPC);
        move_to_target ->
            process_move_to_target_complete(NPC);
        move_to_order_pos ->
            move_to_order_pos(NPC#npc.id);
        melee_attack ->
            complete_task(NPC);
        _ ->
            nothing
    end;
process_event_complete(_) -> 
    nothing.

process_move_to_target_complete(NPC) ->
    [NPCObj] = db:read(obj, NPC#npc.id),

    case NPC#npc.target of
        none -> 
            NewNPC = NPC#npc{task_state = completed},
            db:write(NewNPC);
        TargetId ->
            [TargetObj] = db:read(obj, TargetId),

            case map:is_adjacent(NPCObj#obj.pos, TargetObj#obj.pos) of
                false ->
                    move_to_target(NPC);
                true ->
                    NewNPC = NPC#npc{task_state = completed},
                    db:write(NewNPC)
            end
    end.

complete_task(NPC) ->
    NewNPC = NPC#npc{task_state = completed},
    db:write(NewNPC).

get_next_task(TaskIndex, PlanLength) when TaskIndex < PlanLength ->
    NewTaskIndex = TaskIndex + 1,
    {next_task, NewTaskIndex};
get_next_task(_TaskIndex, _PlanLength) ->
    plan_completed.

move_next_path(_NPCObj, []) -> nothing;
move_next_path(NPCObj, Path) -> move_unit(NPCObj, lists:nth(2, Path)).

move_unit(Obj = #obj {id = Id, pos = Pos}, NewPos) when is_tuple(NewPos) ->
    SourcePos = Pos,
    DestPos = NewPos,
    MoveTicks = obj:movement_cost(Obj, DestPos),

    %Add obj update state to change to moving state on next tick
    game:add_obj_update(self(), Id, ?STATE, ?MOVING, 0),
                
    %Add obj move event to execute in MoveTicks
    game:add_obj_move(self(), Id, SourcePos, DestPos, MoveTicks);    
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

get_wander_pos(_, _, _, []) ->
    none;
get_wander_pos(_NPCObj, true, RandomPos, _Neighbours) ->
    RandomPos;
get_wander_pos(NPCObj, false,  _, Neighbours) ->
    Random = util:rand(length(Neighbours)),
    RandomPos = lists:nth(Random, Neighbours),

    IsAvailable = is_pos_available(NPCObj, RandomPos),

    NewNeighbours = lists:delete(RandomPos, Neighbours),

    get_wander_pos(NPCObj, IsAvailable, RandomPos, NewNeighbours).

is_pos_available(#obj{player = Player}, RandomPos) when Player =:= ?UNDEAD ->
    IsAvailable = obj:is_empty(RandomPos) andalso
                  map:is_passable(RandomPos) andalso
                  not effect:has_effect({tile, RandomPos}, ?SANCTUARY),
    IsAvailable;
is_pos_available(_, RandomPos) ->
    obj:is_empty(RandomPos) and map:is_passable(RandomPos).

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
    combo(obj:template(NPCObj), Rand).

get_next_attack(Attacks, Combo) ->
    RemainingAttacks = lists:subtract(Combo, Attacks),
    [NextAttack | _] = RemainingAttacks,
    NextAttack.

combo({<<"Undead">>, <<"Shadow">>}, Num) when Num < 50 -> [?QUICK, ?PRECISE, ?FIERCE, ?QUICK];
combo({<<"Undead">>, <<"Shadow">>}, _) -> [?PRECISE, ?FIERCE, ?PRECISE, ?PRECISE];
combo(_, _) -> [?QUICK, ?QUICK, ?QUICK, ?FIERCE].

process_perception(NPCId) ->
    [NPCObj] = db:read(obj, NPCId),
    [NPC] = db:read(npc, NPCId),

    Perception = perception:get_entity(NPCObj),

    %Remove from same player and non targetable objs
    FilteredTargets = filter_targets(obj:player(NPCObj), Perception),
    lager:info("NPC: ~p FilteredTargets: ~p", [NPCId, FilteredTargets]),

    %Find target
    Target = find_target(NPCObj, FilteredTargets),
    lager:info("NPC: ~p Target: ~p", [NPCId, Target]),

    NewNPC = NPC#npc {target = Target},
    db:write(NewNPC).

