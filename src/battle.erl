%% -------------------------------------------------------------------
%% Author  : Peter Holko
%%% Description : Battle manager
%%%
%%% Created : Dec 15, 2014
%%% -------------------------------------------------------------------
-module(battle).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/2, active_turn/2, attack_unit/2]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, battle}, battle, [], []).

create(AtkId, DefId) ->
    gen_server:cast({global, battle}, {create, AtkId, DefId}).

active_turn(BattleId, UnitId) ->
    gen_server:cast({global, battle}, {active_turn, BattleId, UnitId}).

attack_unit(SourceId, TargetId) ->
    gen_server:cast({global, battle}, {attack_unit, SourceId, TargetId}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast({create, AtkId, DefId}, Data) ->   

    AtkObj = map:get_obj(AtkId), 
    DefObj = map:get_obj(DefId), 
    BattleId = create_battle(AtkObj, DefObj),

    %Set state to combat
    set_combat_state([AtkObj, DefObj]),

    lager:info("AtkId: ~p, DefId: ~p", [AtkId, DefId]),
    AtkUnits = unit:get_units_and_stats(AtkId),
    DefUnits = unit:get_units_and_stats(DefId),

    BattlePerception = AtkUnits ++ DefUnits,
    lager:info("BattlePerception: ~p", [BattlePerception]),

    send_perception([{AtkObj#map_obj.player, BattlePerception}, 
                     {DefObj#map_obj.player, BattlePerception}]),

    add_battle_units(BattleId, BattlePerception),

    {noreply, Data};

handle_cast({active_turn, BattleId, UnitId}, Data) ->
    
    Action = db:read(action, UnitId),
    process_action(BattleId, Action),

    {noreply, Data};

handle_cast({attack_unit, SourceId, TargetId}, Data) ->
    
    Action = #action {source_id = SourceId,
                      type = attack,
                      data = TargetId},
  
    db:write(Action),

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

create_battle(AtkObj, DefObj) ->

    %Id =  obj:create(0, AtkObj#map_obj.pos, battle),
    Id = 1,
    
    Battle1 = #battle {id = Id,
                       player = AtkObj#map_obj.player,
                       obj =  AtkObj#map_obj.id},
    db:write(Battle1),

    Battle2 = #battle {id = Id,
                       player = DefObj#map_obj.player,
                       obj =  DefObj#map_obj.id},
    db:write(Battle2),

    Id.

set_combat_state([]) ->
    lager:info("Done updating combat state"),
    done;
set_combat_state([Entity | Rest]) ->
    map:update_obj_state(Entity, combat),
    set_combat_state(Rest).

send_perception([]) ->
    lager:info("Done sending battle perception");

send_perception([{PlayerId, NewPerception} | Players]) ->
    [Conn] = db:dirty_read(connection, PlayerId),
    send_to_process(Conn#connection.process, battle_perception, NewPerception),
    send_perception(Players).

process_action(BattleId, [Action]) ->

    case Action#action.type of
        attack ->
            process_attack(BattleId, Action);
        _ ->
            lager:info("Unknown action type: ~p", [Action#action.type]) 
    end;

process_action(_Battle, _Action) ->
    %lager:info("No action defined.").
    none.

process_attack(BattleId, Action) ->
    SourceId = Action#action.source_id,
    TargetId = Action#action.data,

    AtkUnit = unit:get_stats(SourceId),
    DefUnit = unit:get_stats(TargetId),

    is_attack_valid(SourceId, AtkUnit),
    is_attack_valid(SourceId, DefUnit),

    Dmg = calc_attack(BattleId, AtkUnit, DefUnit),

    broadcast_dmg(BattleId, SourceId, TargetId, Dmg).

broadcast_dmg(BattleId, SourceId, TargetId, Dmg) ->
    BattleObjs = db:read(battle, BattleId), 

    F = fun(BattleObj) ->
                Message = #{<<"battle">> => BattleId, 
                            <<"sourceid">> => SourceId,
                            <<"targetid">> => TargetId,
                            <<"dmg">> => Dmg},
                [Conn] = db:dirty_read(connection, BattleObj#battle.player),
                send_to_process(Conn#connection.process, battle, Message)
        end,

    lists:foreach(F, BattleObjs).

is_attack_valid(SourceId, false) ->
    %Invalid unit, remove action
    db:delete(action, SourceId);

is_attack_valid(_SourceId, _Unit) ->
    valid.

calc_attack(_BattleId, false, _DefUnit) ->
    lager:info("Source no longer available");

calc_attack(_BattleId, _AtkUnit, false) ->
    lager:info("Target no longer avalalble");

calc_attack(BattleId, AtkUnit, DefUnit) ->
    {AtkObjId} = bson:lookup(obj_id, AtkUnit),
    {DmgBase} = bson:lookup(base_dmg, AtkUnit),
    {DmgRange} = bson:lookup(dmg_range, AtkUnit),

    {DefId} = bson:lookup('_id', DefUnit),
    {DefObjId} = bson:lookup(obj_id, DefUnit),
    {DefArmor} = bson:lookup(base_def, DefUnit),
    {DefHp} = bson:lookup(hp, DefUnit),

    DmgRoll = random:uniform(DmgRange) + DmgBase,
    
    DmgReduction = DefArmor / (DefArmor + 50),

    Dmg = round(DmgRoll * (1 - DmgReduction)),
    NewHp = DefHp - Dmg,

    %Set new hp
    set_new_hp(AtkObjId, DefId, DefObjId, NewHp),

    Dmg.

set_new_hp(_BattleId, DefId, _DefObjId, NewHp) when NewHp > 0 ->
    mdb:update(<<"unit">>, DefId, {hp, NewHp});

set_new_hp(BattleId, DefId, DefObjId, _NewHp) ->
    lager:info("Unit ~p died.", [DefId]),
    
    Items = item:get_by_owner(DefId),
    drop_items(BattleId, Items),

    %Remove battle unit and unit
    db:delete(battle_unit, DefId),
    unit:killed(DefId),

    Units = unit:get_units(DefObjId),
    set_dead_state(DefObjId, Units).

set_dead_state(DefObjId, []) ->
    lager:info("Obj contains zero units, setting state to dead"),
    map:update_obj_state(DefObjId, dead);
set_dead_state(_DefObjId, Units) ->
    lager:info("Obj contains units: ~p", [Units]).

drop_items(_BattleId, []) ->
    lager:info("Completed dropping items");
drop_items(BattleId, [Item | Rest]) ->
    item:transfer(Item, BattleId),

    drop_items(BattleId, Rest).

add_battle_units(_BattleId, []) ->
    lager:info("Done adding battle units");

add_battle_units(BattleId, [Unit | Rest]) ->
    lager:info("Adding battle_unit: ~p", [Unit]),
    {Id} = bson:lookup('_id', Unit), 
    {Speed} = bson:lookup(base_speed, Unit),

    BattleUnit = #battle_unit {unit_id = Id,
                               speed = Speed,
                               battle = BattleId},

    db:write(BattleUnit),

    add_battle_units(BattleId, Rest).

send_to_process(Process, MessageType, Message) when is_pid(Process) ->
    lager:info("Sending ~p to ~p", [Message, Process]),
    Process ! {MessageType, Message};
send_to_process(_Process, _MessageType, _Message) ->
    none.
