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
-export([move_random_pos/1]).
-export([target_visible/0, toofar/0]).
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
    local:create({2,2}, none, {2,2}, 99, unit, <<"npc">>, <<"Zombie">>, none).

new_wolf() ->
    local:create({2,2}, none, {3,3}, 99, unit, <<"npc">>, <<"Wolf">>, none).

target_visible() ->
    false.

toofar() ->
    false.

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

handle_info({local_perception, {NPCId, Objs}}, Data) ->
    lager:debug("Local perception received."),
    [NPCObj] = db:read(local_obj, NPCId),
    [NPC] = db:read(npc, NPCId),
    NPCStats = local_obj:get_stats(NPCObj#local_obj.id),

    %Remove from same player and non targetable objs
    FilteredTargets = filter_targets(Objs, []),

    %Find target
    Target = find_target(NPCObj, NPCStats, FilteredTargets),

    %Store target
    NewNPC = NPC#npc {target = Target},
    db:write(NewNPC),

    lager:debug("Local perception processing end."),
    {noreply, Data};

handle_info({event_complete, {_Event, Id}}, Data) ->
    [NPC] = db:read(npc, Id),
    Length = length(NPC#npc.plan),
    PlanIndex = NPC#npc.plan_index,
    lager:info("Length ~p PlanIndex: ~p", [Length, PlanIndex]), 
    NewNPC = case PlanIndex < Length of
                 true ->
                    NPC#npc {plan_state = next,
                             plan_index = PlanIndex + 1};
                 false ->
                    NPC#npc {plan_state = none,
                             plan_index = 1}
             end,
 
    db:write(NewNPC),
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

filter_targets([Obj | Rest], Targets) ->
    Player = get(player_id),
    ObjPlayer = maps:get(<<"player">>, Obj),

    NewTargets = case valid_target(Player, ObjPlayer) of
                     true ->
                         LocalObj = get_local_obj(Obj),
                         [LocalObj | Targets];
                     false ->
                         Targets
                 end,

    filter_targets(Rest, NewTargets).

valid_target(_, -1) -> false;
valid_target(Player, ObjPlayer) when Player =:= ObjPlayer -> false;
valid_target(_, _) -> true.

get_local_obj(Obj) ->
    Id = maps:get(<<"id">>, Obj),
    [LocalObj] = db:read(local_obj, Id),
    LocalObj.

process_replan('$end_of_table') ->
    done;
process_replan(Id) ->
    [NPC] = db:read(npc, Id),

    CurrPlan = NPC#npc.plan,
    NewPlan = htn:plan(NPC#npc.orders, Id),

    case NewPlan =:= CurrPlan of
        false ->
            NewNPC = NPC#npc {plan = NewPlan,
                              plan_state = none,
                              plan_index = 1},
            db:write(NewNPC);
        true ->
            nothing
    end.

process_run_plan('$end_of_table') ->
    done;
process_run_plan(Id) ->
    [NPC] = db:read(npc, Id),

    case NPC#npc.plan_state of
        none ->
            game:cancel_event(Id),
            [FirstTask | _Rest] = NPC#npc.plan,
            erlang:apply(npc, FirstTask, [Id]),
            NewNPC = NPC#npc {plan_state = running},
            db:write(NewNPC);
        next ->
            NextTask = lists:nth(NPC#npc.plan_index, NPC#npc.plan),
            erlang:apply(npc, NextTask, [Id]);
        running ->
            nothing
    end.

process_action({attack, Target, Path}, moving, false, NPCObj) ->
    game:cancel_event(NPCObj#local_obj.id),
    do_attack(NPCObj, Target, Path);
process_action({attack, Target, Path}, none, _, NPCObj) ->
    do_attack(NPCObj, Target, Path);
process_action(_, _, _, _) ->
    nothing.

do_attack(NPCObj, Target, Path) ->
    case process_path(Path) of
        move ->
            move_unit(NPCObj, lists:nth(2, Path));
        attack ->
            attack_unit(NPCObj, Target);
        _ ->
            nothing
    end.

process_path(Path) when length(Path) > 2 ->
    move;
process_path(Path) when length(Path) =< 2 ->
    attack.

attack_unit(Source, Target) ->
    battle:attack_unit(Source#local_obj.id, Target#local_obj.id).

move_unit(#local_obj {id = Id,
                      player = Player,
                      global_pos = GlobalPos}, NewPos) ->
    NumTicks = 40,

    %Update unit state
    local:update_state(Id, moving),
    
    %Create event data
    EventData = {GlobalPos,
                 Player,
                 Id,
                 NewPos},

    game:add_event(self(), move_local_obj, EventData, Id, NumTicks).

get_nearest(_NPCUnit, [], {EnemyUnit, _Distance}) ->
    EnemyUnit;
get_nearest(NPCPos, [NewEnemyUnit | EnemyUnits], {EnemyUnit, Distance}) ->
    NewEnemyUnitPos = NewEnemyUnit#local_obj.pos,

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
            ObjEffect = Obj#local_obj.effect,
            IsWalled = lists:member(<<"wall">>, ObjEffect),
            not IsWalled
        end,
    lists:filter(F, ObjList).

remove_dead(ObjList) ->
    F = fun(Obj) ->
            Obj#local_obj.state =/= dead
        end,
    lists:filter(F, ObjList).

remove_structures(ObjList) ->
    F = fun(Obj) ->
            Obj#local_obj.class =/= structure
        end,
    lists:filter(F, ObjList).

get_wander_pos(_, _, _, []) ->
    none;
get_wander_pos(true, _GlobalPos, RandomPos, _Neighbours) ->
    RandomPos;
get_wander_pos(false, GlobalPos, _, Neighbours) ->
    Random = random:uniform(length(Neighbours)),
    RandomPos = lists:nth(Random, Neighbours),
    IsEmpty = local:is_empty(RandomPos),
    NewNeighbours = lists:delete(RandomPos, Neighbours),

    get_wander_pos(IsEmpty, GlobalPos, RandomPos, NewNeighbours).

check_wall(#local_obj{global_pos = GPos, pos = Pos, effect = Effect} = EnemyUnit) ->    
    HasWall = lists:member(<<"wall">>, Effect),
    lager:debug("HasWall: ~p", [HasWall]),
    Target = case HasWall of
                true ->
                    local_obj:get_wall(GPos, Pos);
                false ->
                    EnemyUnit
             end,
    lager:debug("Target: ~p", [Target]),
    Target;

check_wall(_) ->
    none.

find_target(NPCObj, NPCStats, AllEnemyUnits) ->
    {Int} = bson:lookup(int, NPCStats),
    {Aggression} = bson:lookup(aggression, NPCStats),
    find_target(NPCObj, Int, Aggression, AllEnemyUnits).

find_target(NPCObj, <<"mindless">>, <<"high">>, AllEnemyUnits) ->
    EnemyUnits = remove_structures(remove_dead(AllEnemyUnits)),
    EnemyUnit = get_nearest(NPCObj#local_obj.pos, EnemyUnits, {none, 1000}),
    Target = check_wall(EnemyUnit),
    Target;
find_target(NPCObj, <<"animal">>, <<"high">>, AllEnemyUnits) ->
    EnemyUnits = remove_structures(remove_dead(remove_walled(AllEnemyUnits))),
    EnemyUnit = get_nearest(NPCObj#local_obj.pos, EnemyUnits, {none, 1000}),
    EnemyUnit. 

move_random_pos(NPCId) ->
    [NPC] = db:dirty_read(npc, NPCId),
    [NPCObj] = db:dirty_read(local_obj, NPCId),
    {X, Y} = NPCObj#local_obj.pos,
    GlobalPos = NPCObj#local_obj.global_pos,

    Neighbours = map:neighbours(X, Y, ?MAP_WIDTH, ?MAP_HEIGHT),
    NewPos = get_wander_pos(false, GlobalPos, none, Neighbours),
    lager:info("NewPos: ~p", [NewPos]),
   
    NewNPC = NPC#npc {pos = NewPos},
    db:dirty_write(NewNPC),

    move_unit(NPCObj, NewPos).
