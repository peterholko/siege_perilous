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

handle_info({battle_perception, Perception}, Data) ->
    lager:info("Battle perception: ~p", [Perception]),
    {Units, _BattleMap} = Perception,
    {NPCUnits, EnemyUnits} = split_units(Units, [], []),
    process_battle_action(NPCUnits, EnemyUnits),

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

    map:update_obj_state(Obj, moving),
    
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
    ObjId = maps:get(<<"obj">>, Unit),
    [Obj] = obj:get_obj(ObjId),
    {UnitPlayerId} = bson:lookup(player, Obj),

    {NewNPCUnits, NewEnemyUnits} = add_unit(PlayerId =:= UnitPlayerId, 
                                            Unit,
                                            NPCUnits, 
                                            EnemyUnits),

    split_units(Units, NewNPCUnits, NewEnemyUnits).

add_unit(true, Unit, NPCUnits, EnemyUnits) ->
    {[Unit | NPCUnits], EnemyUnits};
add_unit(false, Unit, NPCUnits, EnemyUnits) ->
    {NPCUnits, [Unit, EnemyUnits]}.

process_battle_action(NPCUnits, EnemyUnits) ->
    
    F = fun(NPCUnit) ->
            Action = none,
            NewAction = check_units(NPCUnit, EnemyUnits, Action),
            add_battle_action(NewAction)
        end,

    lists:foreach(F, NPCUnits).

check_units(_NPCUnit, [], Action) ->
    Action;
check_units(NPCUnit, [EnemyUnit | EnemyUnits], Action) ->

    %Check for target
    NewAction = determine_battle_action(NPCUnit, EnemyUnit, Action), 

    check_units(NPCUnit, EnemyUnits, NewAction).

determine_battle_action(NPCUnit, EnemyUnit, none) ->
    {SourceId} = bson:lookup('_id', NPCUnit),
    {TargetId} = bson:lookup('_id', EnemyUnit),
    {attack, SourceId, TargetId};
determine_battle_action(_NPCUnit, _EnemyUnit, Action) ->
    Action.

add_battle_action({attack, SourceId, TargetId}) ->
    battle:attack_unit(SourceId, TargetId);
add_battle_action(none) ->
    lager:info("NPC Unit doing nothing.").
