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

    Result = is_state_not(dead, SourceObj#local_obj.state) andalso
             is_state_not(dead, TargetObj#local_obj.state),

    set_attack_unit(Result, SourceObj, TargetObj),

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

set_attack_unit(false, _, _) ->
    lager:info("set_attack_unit failed");
set_attack_unit(true, SourceObj, TargetObj) ->
    SourceObjStats = local_obj:get_stats(SourceObj#local_obj.id),
    TargetObjStats = local_obj:get_stats(TargetObj#local_obj.id),

    set_battle_unit(SourceObjStats),
    
    set_combat_state(SourceObjStats),
    set_combat_state(TargetObjStats),

    Action = #action {source_id = SourceObj#local_obj.id,
                      type = attack,
                      data = TargetObj#local_obj.id},
    db:write(Action).

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

    SourceObj = get_obj(db:read(local_obj, SourceId)), 
    TargetObj = get_obj(db:read(local_obj, TargetId)),
    
    Result = is_valid_obj(SourceObj) andalso
             is_valid_obj(TargetObj) andalso
             is_adjacent(SourceObj, TargetObj) andalso
             is_target_alive(TargetObj#local_obj.state),
    
    process_dmg(Result, SourceId, TargetId).

get_obj([]) ->
    false;
get_obj([Obj]) ->
    Obj.

is_valid_obj(false) ->
    lager:info("Invalid local obj"),
    false;
is_valid_obj(_Obj) ->
    true.

is_adjacent(SourceObj, TargetObj) ->
    {SX, SY} = SourceObj#local_obj.pos,
    TargetPos = TargetObj#local_obj.pos,
    Neighbours = map:neighbours(SX, SY, 32, 38), 
    case lists:member(TargetPos, Neighbours) of
        true ->
            true;
        false ->
            lager:info("Not adjacent"),
            false
    end.

is_target_alive(dead) -> 
    lager:info("Target not alive"),
    false;
is_target_alive(_) -> 
    true.

process_move(_Action) ->
    none.

broadcast_dmg(SourceId, TargetId, Dmg, State) ->
    %Convert id here as message is being built
    Message = #{<<"packet">> => <<"dmg">>,
                <<"sourceid">> => util:bin_to_hex(SourceId),
                <<"targetid">> => util:bin_to_hex(TargetId),
                <<"dmg">> => Dmg,
                <<"state">> => State},

    [SourceObj] = db:read(local_obj, SourceId),
    [TargetObj] = db:read(local_obj, TargetId),

    GlobalPos = SourceObj#local_obj.global_pos,
    SourcePos = SourceObj#local_obj.pos,
    TargetPos = TargetObj#local_obj.pos,

    l_perception:broadcast(GlobalPos, SourcePos, TargetPos, Message).

process_dmg(false, AtkId, _) ->
    db:delete(action, AtkId),
    lager:info("Invalid attack");      
process_dmg(true, AtkId, DefId) ->
    AtkUnit = local_obj:get_stats(AtkId),
    DefUnit = local_obj:get_stats(DefId),

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
            local_obj:update(DefId, 'hp', NewHp);
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

process_unit_dead(AtkObjId, DefObjId, DefId) ->
    lager:info("Unit ~p died.", [DefId]),
    lager:info("Updating unit state"),
    %Remove unit from collection
    local:update_state(DefId, dead),

    lager:info("Removing unit from battle_unit"),
    
    %Remove unit from battle and action
    db:delete(battle_unit, DefId),
    db:delete(action, DefId),

    DefUnits = db:index_read(local_obj, DefObjId, #local_obj.global_obj_id),

    case is_army_dead(DefUnits) of
        dead ->
            %Update map obj state of attacker
            map:update_obj_state(AtkObjId, none),
            
            %Reprocess perception
            game:set_perception(true);
        alive ->
            none
    end.

set_battle_unit(LocalObj) ->
    {Id} = bson:lookup('_id', LocalObj),
    {Speed} = bson:lookup(base_speed, LocalObj),

    BattleUnit = #battle_unit {unit = Id,
                               speed = Speed},
    lager:info("BattleUnit: ~p", [BattleUnit]),
    db:write(BattleUnit).

set_combat_state(LocalObj) ->
    {Id} = bson:lookup('_id', LocalObj),
    local:update_state(Id, combat).

is_state(ExpectedState, State) when ExpectedState =:= State -> true;
is_state(_ExpectdState, _State) -> false.

is_state_not(NotExpectedState, State) when NotExpectedState =:= State -> false;
is_state_not(_NotExpectedState, State) -> true.
