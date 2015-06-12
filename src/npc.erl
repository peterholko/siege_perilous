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
-export([execute/1, new_zombie/0, get_nearest/3]).
%% ====================================================================
%% External functions
%% ====================================================================

start(PlayerId) ->
    gen_server:start({global, {npc, PlayerId}}, npc, [PlayerId], []).

execute(PlayerId) ->
    gen_server:cast({global, {npc, PlayerId}}, execute).

new_zombie() ->
    local:create({2,2}, none, {2,2}, 99, unit, <<"npc">>, <<"Zombie">>, none).

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

handle_info({local_perception, Perception}, Data) ->
    lager:info("Local perception received"),
    
    {_Explored, Objs} = new_perception(Perception),    
    {NPCObjs, EnemyObjs} = split_objs(Objs, [], []),
    
    lager:info("NPCObjs: ~p EnemyObjs: ~p", [NPCObjs, EnemyObjs]),
    F = fun(NPCObj) ->
            process_local_action(NPCObj, EnemyObjs)
    end,

    lists:foreach(F, NPCObjs),

    {noreply, Data};

handle_info(Info, Data) ->
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

new_perception([{<<"explored">>, Explored}, {<<"objs">>, Objs}]) ->
    {Explored, Objs}.

split_objs([], NPCObjs, EnemyObjs) ->
    {NPCObjs, EnemyObjs};
split_objs([Obj | Rest], NPCObjs, EnemyObjs) ->
    Player = get(player_id),
    ObjPlayer = maps:get(<<"player">>, Obj),
    ComparePlayer = Player =:= ObjPlayer,

    {NewNPCObjs, NewEnemyObjs} = add_npc_obj(Obj, NPCObjs, EnemyObjs, ComparePlayer),

    split_objs(Rest, NewNPCObjs, NewEnemyObjs).

add_npc_obj(Obj, NPCObjs, EnemyObjs, true) ->
    {[get_local_obj(Obj) | NPCObjs], EnemyObjs};
add_npc_obj(Obj, NPCObjs, EnemyObjs, false) ->
    {NPCObjs, [get_local_obj(Obj) | EnemyObjs]}.

get_local_obj(Obj) ->
    Id = maps:get(<<"id">>, Obj),
    [LocalObj] = db:read(local_obj, Id),
    LocalObj.

process_local_action(#local_obj{state = State} = NPCUnit, []) when State =:= none ->
    lager:info("No enemies nearby, wandering..."),
    process_wander(NPCUnit);

process_local_action(#local_obj{state = State, id = NPCId} = NPCUnit, AllEnemyUnits) when State =:= none ->
    lager:info("NPCUnit: ~p", [NPCUnit]),
    lager:info("EnemyUnits: ~p", [AllEnemyUnits]),
    
    NPCStats = local_obj:get_stats(NPCId),
    Int = bson:lookup(<<"int">>, NPCStats),
    Aggression = bson:lookup(<<"aggression">>, NPCStats),
    EnemyUnits = determine_targets(Int, Aggression, AllEnemyUnits),

    EnemyUnit = get_nearest(NPCUnit#local_obj.pos, EnemyUnits, {none, 1000}),
    lager:info("EnemyUnit: ~p", [EnemyUnit]), 
    Path = astar:astar(NPCUnit#local_obj.pos, EnemyUnit#local_obj.pos),
    lager:info("Path: ~p", [Path]),
    NextAction = next_action(State,
                             NPCUnit, 
                             EnemyUnit, 
                             Path),
    lager:info("Next action: ~p", [NextAction]),
    add_local_action(NextAction);

process_local_action(_NPCUnit, _AllEnemyUnits) ->
    lager:info("Action already in progress...").

determine_targets(mindless, high, AllEnemyUnits) ->
    remove_walled(remove_dead(AllEnemyUnits));
determine_targets(animal, high, AllEnemyUnits) ->
    remove_walled(remove_dead(AllEnemyUnits));
determine_targets(_, _, AllEnemyUnits) ->
    AllEnemyUnits.

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

next_action(_, _, _, failure) ->
    none; %Failure to find path 
next_action(dead, _NPCUnit, _EnemyUnit, _Path) ->
    none; %EnemyUnit is dead
next_action(_, NPCUnit, _EnemyUnit, Path) when length(Path) > 2 ->
    {move, NPCUnit, lists:nth(2,Path)};
next_action(_, NPCUnit, EnemyUnit, Path) when length(Path) =< 2 ->
    {attack, NPCUnit, EnemyUnit}.

add_local_action({attack, Source, Target}) ->
    lager:info("Adding attack: ~p ~p", [Source, Target]),
    battle:attack_unit(Source#local_obj.id, Target#local_obj.id);
add_local_action({move, Unit, NextPos}) ->
    add_move_unit(Unit#local_obj.global_pos,
                  Unit#local_obj.player,
                  Unit#local_obj.id,
                  NextPos,
                  40);
add_local_action(none) ->
    lager:info("NPC Unit doing nothing.").

add_move_unit(GlobalPos, Player, UnitId, NewPos, NumTicks) ->
    %Update unit state
    local:update_state(UnitId, moving),
    
    %Create event data
    EventData = {GlobalPos,
                 Player,
                 UnitId,
                 NewPos},

    game:add_event(self(), move_local_obj, EventData, UnitId, NumTicks).

remove_walled(ObjList) ->
    F = fun(Obj) ->
            ObjEffect = Obj#local_obj.effect,
            lists:member(<<"wall">>, ObjEffect)
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

process_wander(#local_obj{state = State,
                          global_pos = GlobalPos,
                          pos = {X, Y}} = NPCUnit) when State =:= none ->
    Neighbours = map:neighbours(X, Y, ?MAP_WIDTH, ?MAP_HEIGHT),
    Action = get_wander_pos(false, NPCUnit, GlobalPos, none, Neighbours),

    add_local_action(Action);
process_wander(_) ->
    nothing.

get_wander_pos(_, _, _, _, []) ->
    none;
get_wander_pos(true, NPCUnit, _GlobalPos, RandomPos, _Neighbours) ->
    {move, NPCUnit, RandomPos};
get_wander_pos(false, NPCUnit, GlobalPos, _, Neighbours) ->

    Random = random:uniform(length(Neighbours)),
    RandomPos = lists:nth(Random, Neighbours),
    IsEmpty = local:is_empty(GlobalPos, RandomPos),
    NewNeighbours = lists:delete(RandomPos, Neighbours),

    get_wander_pos(IsEmpty, NPCUnit, GlobalPos, RandomPos, NewNeighbours).

