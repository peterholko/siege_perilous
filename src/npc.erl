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
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/2, create/3, create/4, state/1, remove/1, process/1, create_plan/1, run_plan/1,
         set_order/2, set_order/3, set_data/2, get_nearest/3]).
-export([hp_normal/1, hp_very_low/1, target_visible/1, target_adjacent/1, max_guard_dist/1, is_state/2]).
-export([is_not_structure_inspected/1]).
-export([set_pos_flee/1, set_pos_guard/1, set_pos_order/1,
         move_random_pos/1, move_to_target/1, attack/1, move_to_pos/1]).
-export([get_player_id/1]).
-export([has_mana/2, phase_id/2, corpses_nearby/1, move_in_range/1, cast_raise_dead/1, 
         cast_shadow_bolt/1, set_pos_mausoleum/1, hide/1, reveal/1, next_phase/1]).
-export([mausoleum_corpses_nearby/1, mausoleum_guardian_dead/1, has_minions/3, are_minions_dead/1, swarm_attack/1]).
-export([say_guard_text/1]).
-export([wait/2, idle/1]).
-export([find_trade_pos/1, set_pos_empire/1]).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, npc}, npc, [], []).

create(Pos, Template) ->
    gen_server:call({global, npc}, {create, Pos, Template}).

create(Pos, Player, Template) ->
    gen_server:call({global, npc}, {create, Pos, Player, Template}).

create(Pos, Player, Template, State) ->
    gen_server:call({global, npc}, {create, Pos, Player, Template, State}).

state(Id) ->
    gen_server:call({global, npc}, {state, Id}).

remove(Id) ->
    gen_server:cast({global, npc}, {remove, Id}).

process(Tick) ->
    gen_server:cast({global, npc}, {process, Tick}).

create_plan(Tick) ->
    gen_server:cast({global, npc}, {create_plan, Tick}).

run_plan(Tick) ->
    gen_server:cast({global, npc}, {run_plan, Tick}).

set_order(Id, Order) ->
    gen_server:cast({global, npc}, {set_order, Id, Order}).

set_order(Id, Order, Data) ->
    gen_server:cast({global, npc}, {set_order, Id, Order, Data}).

set_data(Id, Data) ->
    gen_server:cast({global, npc}, {set_data, Id, Data}).


get_player_id(NPCType) ->
    [NPCPlayer] = db:index_read(player, NPCType, #player.name),
    NPCPlayer#player.id.

%% HTN Conditions %%%

target_visible(NPC) ->
    NPC#npc.target =/= none.

max_guard_dist(NPC) ->
    [NPCObj] = db:read(obj, NPC#npc.id),

    GuardPos = maps:get(guard_pos, NPC#npc.data),
    NPCPos = NPCObj#obj.pos,

    map:distance(GuardPos, NPCPos) > 3. 

target_adjacent(NPC) ->
    [NPCObj] = db:read(obj, NPC#npc.id),

    case NPC#npc.target =/= none of
        true -> 
            [TargetObj] = db:read(obj, NPC#npc.target),
            map:is_adjacent(NPCObj#obj.pos, TargetObj#obj.pos);
        false -> 
            false
    end.

hp_normal(NPC) ->
    NPCHp = obj_attr:value(NPC#npc.id, <<"hp">>),
    NPCBaseHp = obj_attr:value(NPC#npc.id, <<"base_hp">>),
    
    HpNormal = (NPCHp / NPCBaseHp) > 0.20,
    HpNormal.

hp_very_low(NPC) ->
    NPCHp = obj_attr:value(NPC#npc.id, <<"hp">>),
    NPCBaseHp = obj_attr:value(NPC#npc.id, <<"base_hp">>),
    
    HpNormal = (NPCHp / NPCBaseHp) < 0.20,
    HpNormal.

has_mana(_Id, _ManaValue) ->
    true.

is_state(NPC, State) ->
    [NPCObj] = db:read(obj, NPC#npc.id),
    obj:state(NPCObj) =:= State.

%%% Event Conditions %%%
phase_id(NPC, EventPhase) ->
    NPC#npc.phase =:= EventPhase.

corpses_nearby(NPC) ->
    [NPCObj] = db:read(obj, NPC#npc.id),
    Result = obj:get_nearby_corpses(NPCObj) =/= [],
    Result.

mausoleum_corpses_nearby(NPC) ->
    MausoleumId = maps:get(mausoleum, NPC#npc.data),
    ListOfBones = item:get_by_subclass(MausoleumId, ?BONES),
    ListOfBones =/= [].

mausoleum_guardian_dead(NPC) ->
    GuardianId = maps:get(mausoleum_guard, NPC#npc.data),

    Result = case obj:get(GuardianId) of
        invalid -> true;
        GuardianObj -> obj:state(GuardianObj) =:= ?DEAD
    end,
    Result.

has_minions(NPC, Operator, Value) ->
    AllPlayerObjs = obj:get_by_player(NPC#npc.player),

    F = fun(Obj) ->
            obj:id(Obj) =/= NPC#npc.id
        end,

    Minions = lists:filter(F, AllPlayerObjs),
    NumMinions = length(Minions),

    Result = case Operator of
        less -> NumMinions < Value;
        moreorequal -> NumMinions >= Value
    end,
    Result.

are_minions_dead(NPC) ->
    AllPlayerObjs = obj:get_by_player(NPC#npc.player),

    Result = case AllPlayerObjs of
                 [] -> 
                     false;
                 _ ->
                     F = fun(Obj) ->
                            obj:state(Obj) =:= ?DEAD
                         end,

                     lists:all(F, AllPlayerObjs)
             end,
    Result.

is_not_structure_inspected(NPC) ->
    GuardingStructureId = maps:get(guarding_structure, NPC#npc.data),
    Inspected = obj_attr:value(GuardingStructureId, <<"inspected">>, false),
    not Inspected.

%%% HTN Primitives %%%

set_pos_guard(NPC) ->
    GuardPos = maps:get(guard_pos, NPC#npc.data),
    NPC#npc {dest = GuardPos, task_state = completed}.

set_pos_order(NPC) ->
    Pos = maps:get(order_pos, NPC#npc.data),
    NPC#npc {dest = Pos, task_state = completed}.

set_pos_flee(NPC) ->
    [NPCObj] = db:read(obj, NPC#npc.id),

    Radius = 5,
    RandomPos = map:random_location_from(NPCObj#obj.player, NPCObj#obj.pos, Radius),

    NewData = maps:put(<<"flee_pos">>, RandomPos, NPC#npc.data),

    NPC#npc {data = NewData, task_state = completed}.

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
                    obj:update_state(NPC#npc.id, none),
                    NPC#npc {task_state = completed}
            end
    end.

attack(NPC) ->
    [NPCObj] = db:read(obj, NPC#npc.id),

    TargetObj = combat:is_valid_target(NPC#npc.target),

    Checks = combat:is_valid_target(NPC#npc.target) =/= false andalso
             combat:is_target_alive(TargetObj) andalso
             combat:is_targetable(TargetObj) andalso
             combat:in_range(NPCObj, TargetObj),

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
             EventTicks = event_ticks(util:rand(16) + 6),

             game:add_event(self(), attack, EventData, NPC#npc.id, EventTicks),

             NPC#npc {task_state = running,
                      combo = Combo,                      
                      attacks = Attacks ++ [NextAttack]};
         false ->
             NPC#npc {task_state = completed}
    end.

move_to_pos(NPC) ->
    [NPCObj] = db:read(obj, NPC#npc.id),

    Dest = NPC#npc.dest,

    %If dest is set and dest does not equal villager current pos
    NewNPC = case (Dest =/= none) and (Dest =/= NPCObj#obj.pos) of
                  true ->
                     Path = astar:astar(NPCObj#obj.pos, Dest, NPCObj),

                     case Path of
                         [] -> 
                             %No path, move task completed
                             NPC#npc {task_state = completed};
                         _ -> 
                             %Move to next path location
                             move_unit(NPCObj, lists:nth(2, Path)),
                             NPC#npc {task_state = running, path = Path}
                     end;
                  false ->
                     obj:update_state(NPC#npc.id, none),
                     NPC#npc {task_state = completed}
            end,
    NewNPC.

move_in_range(NPC) ->
    [NPCObj] = db:read(obj, NPC#npc.id),

    case NPC#npc.target of
        none ->
            %Invalid target due to either moving out of range or dying
            NPC#npc {task_state = completed};
        _ -> 
            [TargetObj] = db:read(obj, NPC#npc.target),

            Distance = map:distance(NPCObj#obj.pos, TargetObj#obj.pos),
            Range = 2, %TODO fix hardcoded 2

            Compare = compare_dist_range(Distance, Range),

            case compare_dist_range(Distance, Range) of
                out_of_range ->
                    OptimalRing = map:ring(TargetObj#obj.pos, Range),
                        
                    case map:get_closest(NPCObj#obj.pos, OptimalRing) of
                        none ->
                            obj:update_state(NPC#npc.id, none),
                            NPC#npc {task_state = completed};
                        ClosestPos ->                          
                            Path = astar:astar(NPCObj#obj.pos, ClosestPos, NPCObj),
                            move_next_path(NPCObj, Path),
            
                            NPC#npc {task_state = running, path = Path}
                    end;
                optimal_range ->
                    obj:update_state(NPC#npc.id, none),
                    NPC#npc {task_state = completed}
            end
    end.

cast_raise_dead(NPC) ->
    [NPCObj] = db:read(obj, NPC#npc.id),
    
    EventData = case obj:get_nearby_corpses(NPCObj) of
                    [Corpse | _] ->
                        {NPC#npc.id, obj:id(Corpse), obj, ?RAISE_DEAD};
                    [] ->
                        Bones = structure:get_nearby_bones(NPCObj),
                        {NPC#npc.id, item:id(Bones), item, ?RAISE_DEAD}
                end,

    EventTicks = event_ticks(?TICKS_SEC * 10),
    
    game:add_event(self(), cast, EventData, NPC#npc.id, EventTicks),
    game:add_obj_update(self(), NPC#npc.id, ?STATE, ?CASTING, 1),

    sound:talk(NPC#npc.id, "Rise from the dead, Uus Corp!"),

    NPC#npc{task_state = running}.

cast_shadow_bolt(NPC) ->
    [_NPCObj] = db:read(obj, NPC#npc.id),

    EventData = {NPC#npc.id, NPC#npc.target, obj, ?SHADOW_BOLT},
    EventTicks = event_ticks(?TICKS_SEC * 10),

    game:add_event(self(), cast, EventData, NPC#npc.id, EventTicks),
    game:add_obj_update(self(), NPC#npc.id, ?STATE, ?CASTING, 1),

    sound:talk(NPC#npc.id, "Wis An Ben!"),

    NPC#npc{task_state = completed}.

set_pos_mausoleum(NPC) ->
    MausoleumId = maps:get(mausoleum, NPC#npc.data),
    Mausoleum = obj:get(MausoleumId),
        
    NPC#npc{dest = obj:pos(Mausoleum), task_state = completed}.

hide(NPC) ->
    game:add_obj_hide(self(), NPC#npc.id, 1),
    NPC#npc{task_state = completed}.

reveal(NPC) ->
    lager:info("NPC Reveal!"),
    game:add_obj_reveal(self(), NPC#npc.id, 1),
    NPC#npc{task_state = completed}.

next_phase(NPC) ->
    Phase = NPC#npc.phase,
    NextPhase = Phase + 1,
    NPC#npc{phase = NextPhase}.

swarm_attack(NPC) ->
    Minions = obj:get_by_player(NPC#npc.player),

    %TODO FIX this
    MonolithPos = #{order_pos => {18,35}},

    F = fun(Minion) ->
            Result = (obj:class(Minion) =:= unit) and 
                     (obj:id(Minion) =/= NPC#npc.id),

            case Result of
                true ->
                    set_order(obj:id(Minion), move_to_pos, MonolithPos);
                _ ->
                    nothing
            end
        end,

    lists:foreach(F, Minions),
    NPC#npc{task_state = completed}.

find_trade_pos(NPC) ->
    %TODO find player positions
    lager:info("Find trade pos"),
    LandingPos = {15, 37},
    NPC#npc{dest = LandingPos, task_state = completed}.

wait(NPC, Time) ->
    lager:info("Wait"),
    game:add_event(self(), wait, NPC#npc.id, NPC#npc.id, Time),
    NPC#npc{task_state = running}.

set_pos_empire(NPC) -> 
    %TODO define empire location
    NPC#npc{dest = {0, 40}, task_state = completed}.

idle(NPC) ->
    NPC.

say_guard_text(NPC) ->
    Text = maps:get(guard_text, NPC#npc.data, "Thou shall not pass!"),
    sound:talk(NPC#npc.id, Text),

    NPC#npc{task_state = completed}.

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

handle_cast({create_plan, Tick}, Data) ->
    NewData = create_new_plans(Tick, Data),
    {noreply, NewData};

handle_cast({run_plan, Tick}, Data) ->
    NewData = run_new_plans(Tick, Data),
    {noreply, NewData};

handle_cast({remove, NPCId}, Data) ->
    NewData = maps:remove(NPCId, Data),
    {noreply, NewData};

handle_cast({set_order, NPCId, Order}, Data) ->
    NPC = maps:get(NPCId, Data),
    NewNPC = NPC#npc {order = Order},
    NewData = maps:update(NPCId, NewNPC, Data),

    {noreply, NewData};

handle_cast({set_order, NPCId, Order, NPCData}, Data) ->
    NPC = maps:get(NPCId, Data),
    
    NewNPCData = maps:merge(NPC#npc.data, NPCData),

    NewNPC = NPC#npc {order = Order,
                      data = NewNPCData},

    NewData = maps:update(NPCId, NewNPC, Data),

    {noreply, NewData};

handle_cast({set_data, NPCId, NPCData}, Data) ->
    NPC = maps:get(NPCId, Data),
    
    NewNPCData = maps:merge(NPC#npc.data, NPCData),

    NewNPC = NPC#npc {data = NewNPCData},

    NewData = maps:update(NPCId, NewNPC, Data),

    {noreply, NewData};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

%TODO CLEAN UP CREATE HERE
handle_call({create, Pos, Template}, _From, Data) ->
    lager:info("Creating NPC: ~p", [Template]),
    Family = obj_template:value(Template, <<"family">>),
    PlayerId = get_player_id(Family),
    NPCId = obj:create(Pos, PlayerId, Template),

    NPC = #npc{id = NPCId,
               player = PlayerId,
               order = get_order(NPCId),
               last_plan = ?MAX_INT,
               last_run = ?MAX_INT},

    NewData = maps:put(NPCId, NPC, Data),
    lager:info("Create completed..."),

    {reply, NPCId, NewData};

handle_call({create, Pos, PlayerId, Template}, _From, Data) ->
    lager:info("Creating NPC: ~p", [Template]),
    NPCId = obj:create(Pos, PlayerId, Template),

    NPC = #npc{id = NPCId,
               player = PlayerId,
               order = get_order(NPCId),
               last_plan = ?MAX_INT,
               last_run = ?MAX_INT},

    NewData = maps:put(NPCId, NPC, Data),
    lager:info("Create completed..."),

    {reply, NPCId, NewData};

handle_call({create, Pos, PlayerId, Template, State}, _From, Data) ->
    lager:info("Creating NPC: ~p", [Template]),
    NPCId = obj:create(Pos, PlayerId, Template, State),

    NPC = #npc{id = NPCId,
               player = PlayerId,
               order = get_order(NPCId),
               last_plan = ?MAX_INT,
               last_run = ?MAX_INT},

    NewData = maps:put(NPCId, NPC, Data),

    {reply, NPCId, NewData};

handle_call({state, Id}, _From, Data) ->
    State = maps:get(Id, Data, none),
    {reply, State, Data};

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Event}, 
                              {from, From}
                             ]),
    {noreply, Data}.

handle_info({event_complete, {obj_create, NPCId}}, Data) ->

    NPC = maps:get(NPCId, Data),
    NewNPC = NPC#npc {last_plan = 0,
                      last_run = 0},
    NewData = maps:update(NPCId, NewNPC, Data),

    {noreply, NewData};

handle_info({event_complete, {obj_update, _NPCId}}, Data) ->
    %Do not want process_complete to run for obj_update, TODO rework
    {noreply, Data};

handle_info({event_complete, {_Event, Id}}, Data) ->

    NPC = maps:get(Id, Data, false),
    NewData = process_complete(NPC, Data),

    {noreply, NewData};

handle_info({event_cancel, {EventId, Id}}, Data) ->
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
create_new_plans(Tick, Data) ->
    F = fun(NPCId, NPC, Acc) ->
            case Tick >= (NPC#npc.last_plan + (2 * ?TICKS_SEC)) of
                true ->
                    NewNPC = process_plan(NPC, Tick),
                    maps:put(NPCId, NewNPC, Acc);
                false ->
                    maps:put(NPCId, NPC, Acc)
            end
        end,

    maps:fold(F, #{}, Data). 

run_new_plans(Tick, Data) ->
    F = fun(NPCId, NPC, Acc) ->
            case Tick >= (NPC#npc.last_run + (2 * ?TICKS_SEC)) of
                true ->
                    NewNPC = process_run_plan(NPC, Tick),
                    maps:put(NPCId, NewNPC, Acc);
                false ->
                    maps:put(NPCId, NPC, Acc)
            end
        end,

    maps:fold(F, #{}, Data).

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

process_plan(NPC, Tick) ->
    NewNPC = process_perception(NPC),

    CurrentPlan = NewNPC#npc.plan,    
    {_PlanLabel, NewPlan} = htn:plan(NewNPC#npc.order, NewNPC, npc),   

    case NewPlan =:= CurrentPlan of
        false ->
            %New plan cancel current event
            game:cancel_event(NewNPC#npc.id),

            %Reset NPC variables
            NewNPC#npc {plan = NewPlan,
                        task_state = init,
                        task_index = 1, 
                        last_plan = Tick};
        true ->
            NewNPC#npc {last_plan = Tick}
    end.

process_run_plan(NPC, Tick) ->
    case NPC#npc.plan of
        [] -> 
            NPC#npc{last_run = Tick};
        _ ->
            NewNPC = process_task_state(NPC#npc.task_state, NPC),
            NewNPC#npc{last_run = Tick}
    end.

get_task_by_index(NPC, TaskIndex) ->
    TaskData = lists:nth(TaskIndex, NPC#npc.plan),
    process_task_data(NPC, TaskData).

process_task_data(NPC, {TaskName, TaskArgs}) -> {TaskName, [NPC, TaskArgs]};
process_task_data(NPC, TaskName) -> {TaskName, [NPC]}.

process_task_state(init, NPC) ->
    {TaskName, TaskArgs} = get_task_by_index(NPC, 1),

    NewNPC = erlang:apply(npc, TaskName, TaskArgs),
    NewNPC;
process_task_state(completed, NPC) ->
    TaskIndex = NPC#npc.task_index,
    PlanLength = length(NPC#npc.plan),

    NextTask = get_next_task(TaskIndex, PlanLength),

    case NextTask of
        {next_task, NextTaskIndex} ->
            {TaskName, TaskArgs} = get_task_by_index(NPC, NextTaskIndex),

            NewNPC = erlang:apply(npc, TaskName, TaskArgs),
            NewNPC#npc{task_index = NextTaskIndex};
        plan_completed -> 
            NPC#npc{task_state = init, task_index = 1}
    end;
process_task_state(running, NPC) ->
    NPC.

complete_task(NPC) ->
    obj:update_state(NPC#npc.id, none),

    NPC#npc{task_state = completed}.

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
    MoveTicks = event_ticks(obj:movement_cost(Obj, DestPos)),

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

is_pos_available(Obj = #obj{player = Player}, RandomPos) when Player =:= ?UNDEAD ->
    IsAvailable = obj:is_empty(RandomPos) andalso
                  map:is_passable(RandomPos, Obj) andalso
                  not effect:has_effect({tile, RandomPos}, ?SANCTUARY),
    IsAvailable;
is_pos_available(Obj, RandomPos) ->
    obj:is_empty(RandomPos) and map:is_passable(RandomPos, Obj).

check_wall(#obj{id = Id} = EnemyUnit) ->    
    case effect:get_effect_data(Id, ?FORTIFIED) of
        invalid -> EnemyUnit;
        WallId -> obj:get(WallId)
    end;
check_wall(_) ->
    none.

find_target(NPCObj, AllEnemyUnits) ->
    Int = obj_attr:value(NPCObj#obj.id, <<"int">>, none),
    Aggression = obj_attr:value(NPCObj#obj.id, <<"aggression">>, none),
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
    return_target(EnemyUnit);
find_target(_NPCObj, _, _, _) ->
    none.

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

process_perception(NPC) ->
    [NPCObj] = db:read(obj, NPC#npc.id),

    Perception = perception:get_entity(NPCObj),

    %Remove from same player and non targetable objs
    FilteredTargets = filter_targets(NPC#npc.player, Perception),

    %Find target
    Target = find_target(NPCObj, FilteredTargets),

    %New NPC
    NPC#npc {target = Target}.

process_complete(false, Data) -> Data;
process_complete(NPC, Data) ->
    Task = lists:nth(NPC#npc.task_index, NPC#npc.plan),


    NewNPC = case Task of
                 move_random_pos -> complete_task(NPC);
                 move_to_target -> move_to_target(NPC);
                 move_to_pos -> move_to_pos(NPC);
                 move_in_range -> move_in_range(NPC);
                 attack -> complete_task(NPC);
                 cast_raise_dead -> complete_task(NPC);
                 cast_shadow_bolt -> complete_task(NPC);
                 wait -> complete_task(NPC);
                 _ -> NPC
             end,

    maps:update(NPC#npc.id, NewNPC, Data).

get_order(Id) -> 
    binary_to_atom(obj_attr:value(Id, <<"order">>, <<"wander">>), latin1).

compare_dist_range(Dist, Range) when Dist =:= Range -> optimal_range;
compare_dist_range(Dist, Range) when Dist =/= Range -> out_of_range.

event_ticks(Ticks) ->
    case ?FAST_EVENTS of
        true -> 1;
        false -> Ticks
    end.
