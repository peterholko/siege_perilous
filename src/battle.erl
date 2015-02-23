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
-export([create/2, info/1, check_player/2, active_turn/2, attack_unit/2, move_unit/2]).

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

move_unit(UnitId, Pos) ->
    gen_server:cast({global, battle}, {move_unit, UnitId, Pos}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast({create, AtkId, DefId}, Data) ->   
    AtkObj = map:get_obj(AtkId), 
    DefObj = map:get_obj(DefId), 
    Pos = AtkObj#map_obj.pos,

    lager:info("Creating battle..."),
    %Create battle
    BattleId = create_battle(Pos),

    lager:info("Adding battle objs..."),

    %Add battle objs
    add_battle_obj(BattleId, AtkObj#map_obj.player, AtkObj#map_obj.id),
    add_battle_obj(BattleId, DefObj#map_obj.player, DefObj#map_obj.id),

    AtkUnits = unit:get_units_and_stats(AtkId),
    DefUnits = unit:get_units_and_stats(DefId),

    lager:info("Adding battle units..."),

    %Add battle units
    add_battle_units(BattleId, AtkUnits, true, 0),
    add_battle_units(BattleId, DefUnits, false, 0),

    %Set state to combat
    set_combat_state([AtkObj, DefObj]),

    %Get battle units
    BattleUnits = get_battle_units(BattleId),
    lager:info("BattleUnits: ~p", [BattleUnits]),

    %Build battle perception
    BattlePerception = get_perception(BattleUnits, []),
    BattleMap = get_battle_map(BattleId),

    lager:info("BattlePerception: ~p", [BattlePerception]),

    send_perception([{AtkObj#map_obj.player, {BattlePerception, BattleMap}}, 
                     {DefObj#map_obj.player, {BattlePerception, BattleMap}}]),

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

handle_cast({move_unit, UnitId, Pos}, Data) ->
    
    Action = #action {source_id = UnitId,
                      type = move,
                      data = Pos},
  
    db:write(Action),

    {noreply, Data};
 
handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({info, Battle}, _From, Data) ->
    BattleUnits = get_battle_units(Battle),
    BattleMap = get_battle_map(Battle),

    BattlePerception = get_perception(BattleUnits, []),

    {reply, {BattlePerception, BattleMap}, Data};

handle_call({check_player, Player, Battle}, _From, Data) ->
    BattleObjList = db:read(battle_obj, Battle),
    Result = lists:keymember(Player, #battle_obj.player, BattleObjList),
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

create_battle(Pos) ->
    Id =  obj:create(0, Pos, misc, <<"battle">>),
    [BattleMap] = db:read(battle_map, ?PLAINS),

    Battle = #battle {id = Id,
                      tiles = BattleMap#battle_map.tiles},
    db:write(Battle),
    Id.

add_battle_obj(Battle, Player, Obj) ->
    BattleObj = #battle_obj {battle = Battle,
                             player = Player,
                             obj =  Obj},
    db:write(BattleObj).

add_battle_units(_BattleId, [], _Attacker, _Num) ->
    lager:info("Done adding battle units");

add_battle_units(BattleId, [Unit | Rest], Attacker, UnitIndex) ->
    lager:info("Adding battle_unit"),
    set_battle_unit(BattleId, Unit, Attacker, UnitIndex),

    add_battle_units(BattleId, Rest, Attacker, UnitIndex + 1).

set_battle_unit(BattleId, Unit, Attacker, UnitIndex) ->
    {Id} = bson:lookup('_id', Unit),
    {ObjId} = bson:lookup(obj_id, Unit), 
    {Speed} = bson:lookup(base_speed, Unit),
    {Name} = bson:lookup(name, Unit),

    Pos = get_unit_pos(Attacker, UnitIndex),

    BattleUnit = #battle_unit {unit = Id,
                               obj = ObjId,
                               pos = Pos,
                               type = Name,
                               state = none,
                               speed = Speed,
                               battle = BattleId},
    lager:info("BattleUnit: ~p", [BattleUnit]),
    db:write(BattleUnit).

get_unit_pos(true, UnitIndex) ->
    {0, UnitIndex};
get_unit_pos(false, UnitIndex) ->
    {3, UnitIndex}.

get_battle_units(BattleId) ->
    db:index_read(battle_unit, BattleId, #battle_unit.battle).

get_perception([], Perception) ->
    Perception;

get_perception([BattleUnit | Rest], Perception) ->
    {X, Y} = BattleUnit#battle_unit.pos,
    NewPerception = [ #{<<"unit">> => BattleUnit#battle_unit.unit,
                        <<"obj">> => BattleUnit#battle_unit.obj,
                        <<"x">> => X,
                        <<"y">> => Y,
                        <<"type">> => BattleUnit#battle_unit.type,
                        <<"state">> => BattleUnit#battle_unit.state} | Perception],

    get_perception(Rest, NewPerception).

get_battle_map(BattleId) ->
    [Battle] = db:read(battle, BattleId),
   
    F = fun(TileData, MsgTiles) ->
            {Pos, Type} = TileData,
            {X, Y} = Pos,
            [#{<<"x">> => X,
               <<"y">> => Y,
               <<"t">> => Type} | MsgTiles]
        end,

    lists:foldl(F, [], Battle#battle.tiles). 

set_combat_state([]) ->
    lager:info("Done updating combat state"),
    done;
set_combat_state([Entity | Rest]) ->
    map:update_obj_state(Entity, combat),
    set_combat_state(Rest).

send_perception([]) ->
    lager:info("Done sending battle perception");

send_perception([{PlayerId, Perception} | Players]) ->
    [Conn] = db:dirty_read(connection, PlayerId),
    send_to_process(Conn#connection.process, battle_perception, Perception),
    send_perception(Players).

process_action(BattleId, [Action]) ->

    case Action#action.type of
        attack ->
            process_attack(BattleId, Action);
        move ->
            process_move(BattleId, Action);
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

process_move(BattleId, Action) ->
    UnitId = Action#action.source_id,
    Pos = Action#action.data,
    
    [BattleUnit] = db:read(battle_unit, UnitId),
    NewBattleUnit = BattleUnit#battle_unit{pos = Pos},
    db:write(NewBattleUnit),

    %Remove move action
    db:delete(action, UnitId),
    
    broadcast_move(BattleId, UnitId, Pos).
    
broadcast_dmg(BattleId, SourceId, TargetId, Dmg, State) ->
    BattleObjs = db:read(battle_obj, BattleId), 

    F = fun(BattleObj) ->
                Message = #{<<"battle">> => BattleId, 
                            <<"sourceid">> => SourceId,
                            <<"targetid">> => TargetId,
                            <<"dmg">> => Dmg,
                            <<"state">> => State},
                [Conn] = db:dirty_read(connection, BattleObj#battle_obj.player),
                send_to_process(Conn#connection.process, battle_dmg, Message)
        end,

    lists:foreach(F, BattleObjs).

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
    Battle = lists:keyfind(ObjId, #battle_obj.obj, Battles),
    [Conn] = db:read(connection, Battle#battle_obj.player),
    PlayerPid = Conn#connection.process,

    send_to_process(PlayerPid, item_perception, item:get_by_owner(BattleId)).

delete_battle_units(BattleId) ->
    BattleUnits = db:index_read(battle_unit, BattleId, #battle_unit.battle),
    
    F = fun(BattleUnit) ->
            db:delete(battle_unit, BattleUnit#battle_unit.unit)
        end,

    lists:foreach(F, BattleUnits).

send_to_process(Process, MessageType, Message) when is_pid(Process) ->
    lager:debug("Sending ~p to ~p", [Message, Process]),
    Process ! {MessageType, Message};
send_to_process(_Process, _MessageType, _Message) ->
    none.
