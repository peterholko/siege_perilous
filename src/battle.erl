% -------------------------------------------------------------------
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
-export([active_turn/1, attack_unit/2, move_unit/2]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, battle}, battle, [], []).

active_turn(UnitId) ->
    gen_server:cast({global, battle}, {active_turn, UnitId}).

attack_unit(SourceId, TargetId) ->
    gen_server:cast({global, battle}, {attack_unit, SourceId, TargetId}).

move_unit(UnitId, Pos) ->
    gen_server:cast({global, battle}, {move_unit, UnitId, Pos}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast({active_turn, UnitId}, Data) ->
    
    Action = db:read(action, UnitId),
    process_action(Action),

    {noreply, Data};

handle_cast({attack_unit, SourceId, TargetId}, Data) ->
    lager:info("Attack unit"), 
    [SourceObj] = db:read(local_obj, SourceId),
    [TargetObj] = db:read(local_obj, TargetId),

    SourceUnits = unit:get_units_and_stats(SourceObj#local_obj.global_obj_id),
    TargetUnits = unit:get_units_and_stats(TargetObj#local_obj.global_obj_id),

    add_battle_units(SourceUnits),
    add_battle_units(TargetUnits),
    
    set_combat_state(SourceUnits),
    set_combat_state(TargetUnits),

    Action = #action {source_id = SourceId,
                      type = attack,
                      data = TargetId},
    db:write(Action),
    {noreply, Data};

handle_cast({move_unit, SourceId, Pos}, Data) ->
    Action = #action {source_id = SourceId,
                      type = move,
                      data = Pos},
  
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

process_action([Action]) ->

    case Action#action.type of
        attack ->
            process_attack(Action);
        move ->
            process_move(Action);
        _ ->
            lager:info("Unknown action type: ~p", [Action#action.type]) 
    end;

process_action(_Action) ->
    %lager:info("No action defined.").
    none.

process_attack(Action) ->
    lager:info("Process attack"),
    SourceId = Action#action.source_id,
    TargetId = Action#action.data,

    
    

    %TODO fix validation
    %is_attack_valid(SourceId),  
   
     


    process_dmg(SourceId, TargetId).

process_move(_Action) ->
    none.

broadcast_dmg(SourceId, TargetId, Dmg, State) ->
    Message = #{<<"sourceid">> => SourceId,
                <<"targetid">> => TargetId,
                <<"dmg">> => Dmg,
                <<"state">> => State},

    [SourceObj] = db:read(local_obj, SourceId),
    [TargetObj] = db:read(local_obj, TargetId),

    GlobalPos = SourceObj#local_obj.global_pos,
    SourcePos = SourceObj#local_obj.pos,
    TargetPos = TargetObj#local_obj.pos,

    l_perception:broadcast(GlobalPos, SourcePos, TargetPos, Message).
       
%is_attack_valid(SourceId) ->
%    [SourceObj] = db:read(local_obj, SourceId),
%    {X, Y} = SourceObj#local_obj.pos,
%    Neighbours = map:neighbours(X, Y, ?BATTLE_WIDTH, ?BATTLE_HEIGHT), 
%    case lists:member({X, Y}, Neighbours) of
%        true ->
%            valid;
%        false ->
%            db:delete(action, SourceId)
%    end.

process_dmg(AtkId, DefId) ->
    AtkUnit = unit:get_stats(AtkId),
    DefUnit = unit:get_stats(DefId),

    {AtkObjId} = bson:lookup(obj_id, AtkUnit),
    {DmgBase} = bson:lookup(base_dmg, AtkUnit),
    {DmgRange} = bson:lookup(dmg_range, AtkUnit),

    {DefObjId} = bson:lookup(obj_id, DefUnit),
    {DefArmor} = bson:lookup(base_def, DefUnit),
    {DefHp} = bson:lookup(hp, DefUnit),

    DmgRoll = random:uniform(DmgRange) + DmgBase,
    
    DmgReduction = DefArmor / (DefArmor + 50),

    Dmg = round(DmgRoll * (1 - DmgReduction)),
    NewHp = DefHp - Dmg,

    %Check if unit is alive
    UnitState = is_unit_dead(NewHp),

    %Broadcast damage
    lager:debug("Broadcasting dmg: ~p newHp: ~p", [Dmg, NewHp]),
    broadcast_dmg(AtkId, DefId, Dmg, UnitState),

    %Check if unit is dead 
    case UnitState of
        <<"alive">> ->
            update_hp(DefId, NewHp);
        <<"dead">> ->
            process_unit_dead(AtkObjId, DefObjId, DefId)
    end.

is_unit_dead(Hp) when Hp =< 0 ->
    <<"dead">>;
is_unit_dead(_Hp) ->
    <<"alive">>.

is_army_dead([]) ->
    dead;
is_army_dead(_Units) ->
    alive.

update_hp(DefId, NewHp) ->
    mdb:update(<<"unit">>, DefId, {hp, NewHp}).

process_unit_dead(AtkObjId, DefObjId, DefId) ->
    lager:info("Unit ~p died.", [DefId]),
    
    lager:info("Updating unit state"),
    %Remove unit from collection
    local:update_state(DefId, dead),

    lager:info("Removing unit from battle_unit"),
    
    %Remove unit from battle and action
    db:delete(battle_unit, DefId),
    db:delete(action, DefId),

    DefUnits = unit:get_units(DefObjId),

    case is_army_dead(DefUnits) of
        dead ->
            %Update map obj state of attacker
            map:update_obj_state(AtkObjId, none),
            
            %Reprocess perception
            game:set_perception(true);
        alive ->
            none
    end.

add_battle_units([]) ->
    lager:info("Done adding battle units");

add_battle_units([Unit| Rest]) ->    
    set_battle_unit(Unit),
    add_battle_units(Rest).

set_battle_unit(Unit) ->
    {Id} = bson:lookup('_id', Unit),
    {Speed} = bson:lookup(base_speed, Unit),

    BattleUnit = #battle_unit {unit = Id,
                               speed = Speed},
    lager:info("BattleUnit: ~p", [BattleUnit]),
    db:write(BattleUnit).

set_combat_state([]) ->
    lager:info("Done updating combat state"),
    done;
set_combat_state([Unit | Rest]) ->
    {Id} = bson:lookup('_id', Unit),
    local:update_state(Id, combat),
    set_combat_state(Rest).


