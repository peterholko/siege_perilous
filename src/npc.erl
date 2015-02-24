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

%% ====================================================================
%% External functions
%% ====================================================================

start(PlayerId) ->
    gen_server:start({global, {npc, PlayerId}}, npc, [PlayerId], []).

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

handle_info({map_perception, Perception}, Data) ->
    lager:info("Map perception: ~p", [Perception]),

    {_Explored, Objs} = new_perception(Perception),
    {NPCObjs, EnemyObjs} = split_objs(Objs, [], []),
    process_action(NPCObjs, EnemyObjs),

    {noreply, Data};

handle_info({battle_perception, Perception}, _Data) ->
    lager:info("Battle perception: ~p", [Perception]),
    {Units, BattleMap} = Perception,
    {NPCUnits, EnemyUnits} = split_units(Units, [], []),
    lager:info("NPCUnits: ~p", [NPCUnits]),
    lager:info("EnemyUnits: ~p", [EnemyUnits]),

    F = fun(NPCUnitId) ->
        process_battle_action(NPCUnitId, EnemyUnits)
    end,

    lists:foreach(F, NPCUnits),

    {noreply, {NPCUnits, EnemyUnits, BattleMap}};    

handle_info({battle_move, _MoveData}, Data) ->
    %UnitId = maps:get(<<"sourceid">>, MoveData),
    {NPCUnits, EnemyUnits, _BattleMap} = Data,

    F = fun(NPCUnitId) ->
        process_battle_action(NPCUnitId, EnemyUnits)
    end,

    lists:foreach(F, NPCUnits),

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
    {[Obj | NPCObjs], EnemyObjs};
add_npc_obj(Obj, NPCObjs, EnemyObjs, false) ->
    {NPCObjs, [Obj | EnemyObjs]}.

process_action(NPCObjs, EnemyObjs) ->
    %Do nothing for now for explored

    F = fun(NPCObj) ->
            Action = none,
            NewAction = check_objs(NPCObj, EnemyObjs, Action),
            add_action(NewAction)
        end,

    lists:foreach(F, NPCObjs).

check_objs(_NPCObjs, [], Action) ->
    Action;

check_objs(NPCObj, [EnemyObj | Rest], Action) ->

    NPCId = maps:get(<<"id">>, NPCObj),
    NPCX = maps:get(<<"x">>, NPCObj),
    NPCY = maps:get(<<"y">>, NPCObj),
    NPCState = maps:get(<<"state">>, NPCObj),
    NPCPos = {NPCX, NPCY},

    Id = maps:get(<<"id">>, EnemyObj),
    X = maps:get(<<"x">>, EnemyObj),
    Y = maps:get(<<"y">>, EnemyObj),
    State = maps:get(<<"state">>, EnemyObj),
    Pos = {X, Y},

    CheckPos = NPCPos =:= Pos, 
 
    lager:info("Action: ~p CheckPos: ~p NPC: ~p Enemy: ~p", [Action, CheckPos, {NPCId, NPCPos, NPCState}, {Id, Pos, State}]),
    NewAction = determine_action(Action, 
                                 CheckPos,
                                 {NPCId, NPCPos, NPCState},
                                 {Id, Pos, State}),
    lager:info("NewAction: ~p", [NewAction]),
    check_objs(NPCObj, Rest, NewAction).

determine_action(_Action, false, {NPCId, _NPCPos, none}, {_Id, Pos, none}) ->
    lager:info("determine_action move npcid: ~p pos: ~p", [NPCId, Pos]),
    {move, {NPCId, Pos}};
determine_action(_Action, true, {NPCId, _NPCPos, none}, {Id, _Pos, none}) ->
    lager:info("determine_action attack npcid: ~p id: ~p", [NPCId, Id]),
    {attack, {NPCId, Id}};
determine_action(_Action, _Pos, NPC, _Enemy) ->
    {none, {NPC}}.

add_action({move, {NPCId, Pos}}) ->
    lager:info("npc ~p adding move", [NPCId]),
    Obj = map:get_obj(NPCId),
    NumTicks = 8,

    %Create event data 
    EventData = {Obj#map_obj.player,
                 Obj#map_obj.id,
                 Pos},
    
    game:add_event(self(), move_obj, EventData, NumTicks);

add_action({attack, {NPCId, Id}}) ->
    lager:info("npc adding attack"),

    NumTicks = 8,
    EventData = {NPCId, Id},
 
    game:add_event(self(), attack_obj, EventData, NumTicks);

add_action({none, _Data}) ->
    lager:info("NPC doing nothing");

add_action(none) ->
    lager:info("NPC doing nothing").

split_units([], NPCUnits, EnemyUnits) ->
    {NPCUnits, EnemyUnits};
split_units([Unit | Units], NPCUnits, EnemyUnits) ->
    PlayerId = get(player_id),
    UnitId = maps:get(<<"unit">>, Unit),
    ObjId = maps:get(<<"obj">>, Unit),
    [Obj] = obj:get_obj(ObjId),
    {UnitPlayerId} = bson:lookup(player, Obj),

    {NewNPCUnits, NewEnemyUnits} = add_unit(PlayerId =:= UnitPlayerId, 
                                            UnitId,
                                            NPCUnits, 
                                            EnemyUnits),

    split_units(Units, NewNPCUnits, NewEnemyUnits).

add_unit(true, Unit, NPCUnits, EnemyUnits) ->
    {[Unit | NPCUnits], EnemyUnits};
add_unit(false, Unit, NPCUnits, EnemyUnits) ->
    {NPCUnits, [Unit | EnemyUnits]}.

process_battle_action(NPCUnitId, EnemyUnits) ->
    lager:info("process battle action for ~p", [NPCUnitId]),
    [NPCUnit] = db:read(battle_unit, NPCUnitId),
    NPCPos = NPCUnit#battle_unit.pos,
    EnemyUnit = check_distance(NPCPos, EnemyUnits, {none, 1000}),
    
    Path = astar:astar(NPCUnit#battle_unit.pos, EnemyUnit#battle_unit.pos),
    lager:info("Path: ~p", [Path]),
    NextAction = next_action(NPCUnit, EnemyUnit, Path),
    lager:info("next_action: ~p", [NextAction]),    
    add_battle_action(NextAction).

check_distance(_NPCUnit, [], {EnemyUnit, _Distance}) ->
    EnemyUnit;
check_distance(NPCPos, [NewEnemyUnitId | EnemyUnits], {EnemyUnit, Distance}) ->
    [NewEnemyUnit] = db:read(battle_unit, NewEnemyUnitId),
    NewEnemyUnitPos = NewEnemyUnit#battle_unit.pos,

    {TargetEnemyUnit, NewDistance} = compare_distance(map:distance(NPCPos, NewEnemyUnitPos), 
                                                      Distance,
                                                      NewEnemyUnit,
                                                      EnemyUnit),

    check_distance(NPCPos, EnemyUnits, {TargetEnemyUnit, NewDistance}).
        
compare_distance(NewDistance, Distance, _New, Old) when NewDistance >= Distance ->
    {Old, Distance};
compare_distance(NewDistance, Distance, New, _Old) when NewDistance < Distance ->
    {New, NewDistance}.

next_action(NPCUnit, _EnemyUnit, Path) when length(Path) > 2 ->
    {move, NPCUnit#battle_unit.unit, lists:nth(2,Path)};
next_action(NPCUnit, EnemyUnit, Path) when length(Path) =< 2 ->
    {attack, NPCUnit#battle_unit.unit, EnemyUnit#battle_unit.unit}.

add_battle_action({attack, SourceId, TargetId}) ->
    battle:attack_unit(SourceId, TargetId);
add_battle_action({move, UnitId, NextPos}) ->
    battle:move_unit(UnitId, NextPos);
add_battle_action(none) ->
    lager:info("NPC Unit doing nothing.").
