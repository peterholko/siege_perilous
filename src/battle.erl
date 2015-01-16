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
-export([create/2, add_event_attack/2, do_attack/2]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, battle}, battle, [], []).

create(AtkId, DefId) ->
    gen_server:cast({global, battle}, {create, AtkId, DefId}).

add_event_attack(SrcUnitId, TgtUnitId) ->
    gen_server:cast({global, battle}, {add_event_attack, SrcUnitId, TgtUnitId}).

do_attack(SrcUnitId, TgtUnitId) ->
    gen_server:cast({global, battle}, {do_attack, SrcUnitId, TgtUnitId}).

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

    AtkUnits = obj:get_units(AtkObj),
    DefUnits = obj:get_units(DefObj),

    BattlePerception = AtkUnits ++ DefUnits,

    send_perception([{AtkObj#map_obj.player, {<<"units">>, BattlePerception}}, 
                     {DefObj#map_obj.player, {<<"units">>, BattlePerception}}]),

    {noreply, Data};

handle_cast({add_event_attack, SrcUnitId, TgtUnitId}, Data) ->
    
    Unit = unit:get_unit(SrcUnitId),
    Speed = maps:get(<<"speed">>, Unit),
    NumTicks = Speed * 4,

    EventData = {SrcUnitId, TgtUnitId},

    game:add_event(self(), attack_unit, EventData, NumTicks),

    {noreply, Data};

handle_cast({do_attack, SrcUnitId, TgtUnitId}, Data) ->

    AtkUnit = unit:get_unit(SrcUnitId),
    DefUnit = unit:get_unit(TgtUnitId),

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

    DmgBase = maps:get(<<"dmg_base">>, AtkUnit),
    DmgRange = maps:get(<<"dmg_range">>, AtkUnit),
    DefArmor = maps:get(<<"def">>, DefUnit),
    DefHp = maps:get(<<"hp">>, DefUnit),
    DefId = maps:get(<<"_id">>, DefUnit),

    DmgRoll = random:uniform(DmgRange) + DmgBase,
    
    DmgReduction = DefArmor / (DefArmor + 50),

    Dmg = round(DmgRoll * (1 - DmgReduction)),
    NewHp = DefHp - Dmg,



