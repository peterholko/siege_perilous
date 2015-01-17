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
-export([create/2, active_turn/1]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, battle}, battle, [], []).

create(AtkId, DefId) ->
    gen_server:cast({global, battle}, {create, AtkId, DefId}).

active_turn(UnitId) ->
    gen_server:cast({global, battle}, {active_turn, UnitId}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast({create, AtkId, DefId}, Data) ->   

    AtkObj = map:get_obj(AtkId), 
    DefObj = map:get_obj(DefId), 

    create_battle(AtkObj, DefObj),
    set_combat_state([AtkObj, DefObj]),

    lager:info("AtkId: ~p, DefId: ~p", [AtkId, DefId]),
    AtkUnits = obj:get_units(AtkId),
    DefUnits = obj:get_units(DefId),

    BattlePerception = AtkUnits ++ DefUnits,
    lager:info("BattlePerception: ~p", [BattlePerception]),

    send_perception([{AtkObj#map_obj.player, {<<"units">>, BattlePerception}}, 
                     {DefObj#map_obj.player, {<<"units">>, BattlePerception}}]),

    add_battle_units(BattlePerception),

    {noreply, Data};

handle_cast({active_turn, UnitId}, Data) ->
    lager:info("Active Turn: ~p", [UnitId]),
    AtkUnit = unit:get_unit(UnitId),
    DefUnit = unit:get_unit(UnitId),

    calc_attack(AtkUnit, DefUnit),   

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
    Id = counter:increment(battle),
    Battle = #battle {id = Id,
                      attacker = {AtkObj#map_obj.player, AtkObj#map_obj.id},
                      defender = {DefObj#map_obj.player, DefObj#map_obj.id}},

    db:write(Battle),
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
    send_to_process(Conn#connection.process, NewPerception),
    send_perception(Players).

send_to_process(Process, NewPerception) when is_pid(Process) ->
    lager:debug("Sending ~p to ~p", [NewPerception, Process]),
    Process ! {battle_perception, [NewPerception]};
send_to_process(_Process, _NewPerception) ->
    none.

calc_attack(AtkUnit, DefUnit) ->
    DmgBase = bson:lookup(dmg_base, AtkUnit),
    DmgRange = bson:lookup(dmg_range, AtkUnit),
    DefArmor = maps:get(<<"def">>, DefUnit),
    DefHp = maps:get(<<"hp">>, DefUnit),
    DefId = maps:get(<<"_id">>, DefUnit),

    DmgRoll = random:uniform(DmgRange) + DmgBase,
    
    DmgReduction = DefArmor / (DefArmor + 50),

    Dmg = round(DmgRoll * (1 - DmgReduction)),
    NewHp = DefHp - Dmg.

add_battle_units([]) ->
    lager:info("Done adding battle units");

add_battle_units([Unit | Rest]) ->
    lager:info("Adding battle_unit: ~p", [Unit]),
    Id = maps:get(<<"_id">>, Unit), 
    Speed = maps:get(<<"speed">>, Unit),

    BattleUnit = #battle_unit {unit_id = Id,
                               speed = Speed},

    db:write(BattleUnit),

    add_battle_units(Rest).
