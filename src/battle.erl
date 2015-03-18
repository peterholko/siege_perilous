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

add_battle_units([]) ->
    lager:info("Done adding battle units");

add_battle_units([Unit | Rest]) ->
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
set_combat_state([Entity | Rest]) ->
    map:update_obj_state(Entity, combat),
    set_combat_state(Rest).

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

    AtkUnit = db:read(battle_unit, SourceId),
    DefUnit = db:read(battle_unit, TargetId),

    is_attack_valid(SourceId, AtkUnit, DefUnit),

    process_dmg(SourceId, TargetId).

process_move(Action) ->
    none.

broadcast_dmg(BattleId, SourceId, TargetId, Dmg, State) ->
        Message = #{<<"battle">> => BattleId, 
                    <<"sourceid">> => SourceId,
                    <<"targetid">> => TargetId,
                    <<"dmg">> => Dmg,
                    <<"state">> => State},

broadcast_move(BattleId, SourceId, Pos) ->
    BattleObjs = db:read(battle_obj, BattleId), 
    {X, Y} = Pos,

    F = fun(BattleObj) ->
                Message = #{<<"battle">> => BattleId, 
                            <<"sourceid">> => SourceId,
                            <<"x">> => X,
                            <<"y">> => Y,
                            <<"state">> => <<"moving">>},
                [Conn] = db:dirty_read(connection, BattleObj#battle_obj.player),
                send_to_process(Conn#connection.process, battle_move, Message)
        end,

    lists:foreach(F, BattleObjs).
       
is_attack_valid(SourceId, [AtkUnit], [DefUnit]) ->
    {AtkX, AtkY} = AtkUnit#battle_unit.pos,
    Neighbours = map:neighbours(AtkX, AtkY, ?BATTLE_WIDTH, ?BATTLE_HEIGHT), 
    case lists:member(DefUnit#battle_unit.pos, Neighbours) of
        true ->
            valid;
        false ->
            db:delete(action, SourceId)
    end;

is_attack_valid(SourceId, _, _) ->
    db:delete(action, SourceId).

process_dmg(false, _DefUnit) ->
    lager:info("Source no longer available");

process_dmg(_AtkUnit, false) ->
    lager:info("Target no longer avalalble");

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

process_unit_dead(BattleId, AtkObjId, DefObjId, DefId) ->
    lager:info("Unit ~p died.", [DefId]),
    
    %Transfer items to the battle
    transfer_items(BattleId, item:get_by_owner(DefId)),

    lager:info("Removing unit..."),
    %Remove unit from collection
    unit:remove(DefId),

    lager:info("Removing unit from battle..."),
    %Remove unit from battle
    db:delete(battle_unit, DefId),

    case is_army_dead(unit:get_units(DefObjId)) of
        dead ->
            %Remove battle units
            delete_battle_units(BattleId),
            
            %Clear all actions
            delete_actions(BattleId),

            %Get all items to transfer
            Items = item:get_by_owner(DefObjId) ++ item:get_by_owner(BattleId),
            
            %Send item perception
            send_item_perception(BattleId, AtkObjId, Items),

            %Transfer any defender army items to attacker and battle items
            transfer_items(AtkObjId, Items),

            %Update map obj state of attacker
            map:update_obj_state(AtkObjId, none),
            
            %Remove obj and battle
            obj:remove(DefObjId),
            obj:remove(BattleId),

            map:remove_obj(DefObjId),
            map:remove_obj(BattleId),
             
            %Reprocess perception
            game:set_perception(true),

            %Remove battle
            db:delete(battle, BattleId);
        alive ->
            none
    end.

transfer_items(_TargetId, []) ->
    lager:info("Transferring items completed.");
transfer_items(TargetId, [Item | Rest]) ->
    item:transfer(Item, TargetId),
    transfer_items(TargetId, Rest).

send_item_perception(BattleId, ObjId, ItemPerception) ->
    Battles = db:read(battle_obj, BattleId),
    Battle = lists:keyfind(ObjId, #battle_obj.obj, Battles),
    [Conn] = db:read(connection, Battle#battle_obj.player),
    PlayerPid = Conn#connection.process,

    send_to_process(PlayerPid, item_perception, ItemPerception).

delete_battle_units(BattleId) ->
    BattleUnits = db:index_read(battle_unit, BattleId, #battle_unit.battle),
    
    F = fun(BattleUnit) ->
            db:delete(battle_unit, BattleUnit#battle_unit.unit)
        end,

    lists:foreach(F, BattleUnits).

delete_actions(BattleId) ->
    Actions = db:index_read(action, BattleId, #action.battle),

    F = fun(Action) ->
            db:delete(action, Action#action.source_id)
        end,

    lists:foreach(F, Actions).

send_to_process(Process, MessageType, Message) when is_pid(Process) ->
    lager:debug("Sending ~p to ~p", [Message, Process]),
    Process ! {MessageType, Message};
send_to_process(_Process, _MessageType, _Message) ->
    none.
