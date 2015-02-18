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
-export([create/2, info/1, check_player/2, active_turn/2, attack_unit/2]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, battle}, battle, [], []).

create(AtkId, DefId) ->
    gen_server:cast({global, battle}, {create, AtkId, DefId}).

info(Battle) ->
    gen_server:call({global, battle}, {info, Battle}).

check_player(Player, Battle) ->
    gen_server:call({global, battle}, {check_player, Player, Battle}).

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
    lager:info("AtkId: ~p, DefId: ~p", [AtkId, DefId]),

    AtkObj = map:get_obj(AtkId), 
    DefObj = map:get_obj(DefId), 
    BattleId = create_battle(AtkObj, DefObj),

    %Set state to combat
    set_combat_state([AtkObj, DefObj]),

    BattlePerception = build_perception([AtkId, DefId], []),
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

handle_call({info, Battle}, _From, Data) ->
    CombatantList = get_combatants(Battle),
    BattlePerception = build_perception(CombatantList, []),

    {reply, BattlePerception, Data};

handle_call({check_player, Player, Battle}, _From, Data) ->
    BattleList = db:read(battle, Battle),
    Result = lists:keymember(Player, #battle.player, BattleList),
    lager:info("Battle check_player: ~p", [Result]), 
    {reply, Result, Data};

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

    Id =  obj:create(0, AtkObj#map_obj.pos, misc, <<"battle">>),

    Battle = #battle {id = Id,
                      tiles = []},
    db:write(Battle),

    create_battle_obj(Id, AtkObj#map_obj.player, AtkObj#map_obj.id),
    create_battle_obj(Id, DefObj#map_obj.player, DefObj#map_obj.id),

    Id.

create_battle_obj(Battle, Player, Obj) ->
    BattleObj = #battle_obj {battle = Battle,
                             player = Player,
                             obj =  Obj},
    db:write(BattleObj).

get_combatants(BattleId) ->
    BattleList = db:read(battle, BattleId),
    
    F = fun(Battle, CombatantList) ->
            [Battle#battle.obj | CombatantList]
        end,

    lists:foldl(F, [], BattleList).

build_perception([], Perception) ->
    Perception;

build_perception([ObjId | ObjList], Perception) ->    
    Units = unit:get_units_and_stats(ObjId),
    NewPerception = Perception ++ Units,
    build_perception(ObjList, NewPerception).

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

    process_dmg(BattleId, AtkUnit, DefUnit).

broadcast_dmg(BattleId, SourceId, TargetId, Dmg, State) ->
    BattleObjs = db:read(battle, BattleId), 

    F = fun(BattleObj) ->
                Message = #{<<"battle">> => BattleId, 
                            <<"sourceid">> => SourceId,
                            <<"targetid">> => TargetId,
                            <<"dmg">> => Dmg,
                            <<"state">> => State},
                [Conn] = db:dirty_read(connection, BattleObj#battle.player),
                send_to_process(Conn#connection.process, battle, Message)
        end,

    lists:foreach(F, BattleObjs).

is_attack_valid(SourceId, false) ->
    %Invalid unit, remove action
    db:delete(action, SourceId);

is_attack_valid(_SourceId, _Unit) ->
    valid.

process_dmg(_BattleId, false, _DefUnit) ->
    lager:info("Source no longer available");

process_dmg(_BattleId, _AtkUnit, false) ->
    lager:info("Target no longer avalalble");

process_dmg(BattleId, AtkUnit, DefUnit) ->
    {AtkId} = bson:lookup('_id', AtkUnit),
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

    %Check if unit is alive
    UnitState = is_unit_dead(NewHp),

    %Broadcast damage
    lager:debug("Broadcasting dmg: ~p newHp: ~p", [Dmg, NewHp]),
    broadcast_dmg(BattleId, AtkId, DefId, Dmg, UnitState),

    %Check if unit is dead 
    case UnitState of
        <<"alive">> ->
            update_hp(DefId, NewHp);
        <<"dead">> ->
            process_unit_dead(BattleId, AtkObjId, DefObjId, DefId)
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
    unit:killed(DefId),

    lager:info("Removing unit from battle..."),
    %Remove unit from battle
    db:delete(battle_unit, DefId),

    case is_army_dead(unit:get_units(DefObjId)) of
        dead ->
            %Transfer any defender army items to attacker and battle items
            transfer_items(BattleId, item:get_by_owner(DefObjId)),
            transfer_items(BattleId, item:get_by_owner(BattleId)),

            %Send item perception
            send_item_perception(BattleId, AtkObjId),

            %Update map obj state of attacker and defender
            map:update_obj_state(AtkObjId, none),
            map:update_obj_state(DefObjId, dead),

            %Reprocess perception
            game:set_perception(true),

            %Remove battle unit and battle entries
            db:delete(battle, BattleId),
            delete_battle_units(BattleId);
        alive ->
            none
    end.

transfer_items(_TargetId, []) ->
    lager:info("Transferring items completed.");
transfer_items(TargetId, [Item | Rest]) ->
    item:transfer(Item, TargetId),
    transfer_items(TargetId, Rest).

send_item_perception(BattleId, ObjId) ->
    Battles = db:read(battle, BattleId),
    Battle = lists:keyfind(ObjId, #battle.obj, Battles),
    [Conn] = db:read(connection, Battle#battle.player),
    PlayerPid = Conn#connection.process,

    send_to_process(PlayerPid, item_perception, item:get_by_owner(BattleId)).

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

delete_battle_units(BattleId) ->
    BattleUnits = db:index_read(battle_unit, BattleId, #battle_unit.battle),
    
    F = fun(BattleUnit) ->
            db:delete(battle_unit, BattleUnit#battle_unit.unit_id)
        end,

    lists:foreach(F, BattleUnits).

send_to_process(Process, MessageType, Message) when is_pid(Process) ->
    lager:debug("Sending ~p to ~p", [Message, Process]),
    Process ! {MessageType, Message};
send_to_process(_Process, _MessageType, _Message) ->
    none.
