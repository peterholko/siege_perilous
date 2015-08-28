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
-export([execute/1, new_zombie/0, new_wolf/0, get_nearest/3]).
%% ====================================================================
%% External functions
%% ====================================================================

start(PlayerId) ->
    gen_server:start({global, {npc, PlayerId}}, npc, [PlayerId], []).

execute(PlayerId) ->
    gen_server:cast({global, {npc, PlayerId}}, execute).

new_zombie() ->
    local:create({2,2}, none, {2,2}, 99, unit, <<"npc">>, <<"Zombie">>, none).

new_wolf() ->
    local:create({2,2}, none, {3,3}, 99, unit, <<"npc">>, <<"Wolf">>, none).

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

handle_cast(execute, Data) ->
    %PlayerId = get(player_id),
    %Units = db:index_read(local_obj, PlayerId, #local_obj.player),

    %F = fun(Unit) ->
    %        Perception = db:dirty_read(perception, {PlayerId, Unit#local_obj.global_pos}),
    %        process_perception(Perception)
    %    end,

    %lists:foreach(F, Units),

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

handle_info({map_perception_disabled, Perception}, Data) ->
    lager:info("Map perception: ~p", [Perception]),

    {noreply, Data};

handle_info({local_perception, {NPCId, Objs}}, Data) ->
    lager:debug("Local perception received."),
    [NPC] = db:read(npc, NPCId),
    [NPCObj] = db:read(local_obj, NPCId),

    TargetObjs = filter_targets(Objs, []),

    process_npc(NPC#npc.objective, NPCObj, TargetObjs),

    lager:debug("Local perception processing end."),
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

process_npc(_Objective, NPCObj, []) ->
    lager:debug("No enemies nearby, wandering..."),
    %Set action wander and process
    process_action(wander, NPCObj#local_obj.state, true, NPCObj);

process_npc(Objective, NPCObj, AllEnemyUnits) ->
    lager:debug("NPCObj: ~p", [NPCObj]),
    lager:debug("EnemyUnits: ~p", [AllEnemyUnits]),
    
    NPCStats = local_obj:get_stats(NPCObj#local_obj.id),
    {Int} = bson:lookup(int, NPCStats),
    {Aggression} = bson:lookup(aggression, NPCStats),

    NewObjective = determine_objective(NPCObj, Int, Aggression, AllEnemyUnits),
    SameObjective = NewObjective =:= Objective,

    process_action(NewObjective, NPCObj#local_obj.state, SameObjective, NPCObj).

determine_objective(NPCUnit, <<"mindless">>, <<"high">>, AllEnemyUnits) ->
    EnemyUnits = remove_structures(remove_dead(AllEnemyUnits)),
    EnemyUnit = get_nearest(NPCUnit#local_obj.pos, EnemyUnits, {none, 1000}),
    lager:debug("Current Target: ~p", [EnemyUnit]),
    Target = check_wall(EnemyUnit),

    check_target(NPCUnit, Target);

determine_objective(NPCUnit, <<"animal">>, <<"high">>, AllEnemyUnits) ->
    lager:debug("AllEnemyUnits: ~p", [AllEnemyUnits]),
    EnemyUnits = remove_structures(remove_dead(remove_walled(AllEnemyUnits))),
    lager:debug("EnemyUnits: ~p", [EnemyUnits]),
    EnemyUnit = get_nearest(NPCUnit#local_obj.pos, EnemyUnits, {none, 1000}),
    lager:debug("Current Target: ~p", [EnemyUnit]),
    
    check_target(NPCUnit, EnemyUnit).

check_target(_NPCUnit, none) ->
    lager:debug("No valid targets nearby, wandering..."),
    wander;
check_target(NPCUnit, Target) ->
    Result = astar:astar(NPCUnit#local_obj.pos, Target#local_obj.pos),
    Objective = case Result of
                    failure ->
                        wander;
                    Path ->
                        {attack, Target, Path}
                end,
    Objective.

process_action(wander, moving, _, _NPCObj) -> 
    nothing;
process_action(wander, none, _, NPCObj) -> 
    do_wander(NPCObj);
process_action({attack, _Target, _Path}, moving, true, _NPCObj) ->
    nothing;
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
    end,

    store_objective(NPCObj#local_obj.id, {attack, Target, Path}).

do_wander(NPCObj = #local_obj{global_pos = GlobalPos,
                              pos = {X, Y}}) ->
    Neighbours = map:neighbours(X, Y, ?MAP_WIDTH, ?MAP_HEIGHT),
    NewPos = get_wander_pos(false, GlobalPos, none, Neighbours),
    
    move_unit(NPCObj, NewPos),
    store_objective(NPCObj#local_obj.id, wander).

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
    IsEmpty = local:is_empty(GlobalPos, RandomPos),
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

store_objective(NPCId, Objective) ->
    %TODO convert to transaction
    [NPC] = db:dirty_read(npc, NPCId),
    NewNPC = NPC#npc {objective = Objective},
    db:dirty_write(NewNPC).
