%% Author: Peter
%% Created: Dec 27, 2008
%% Description: TODO: Add description to game_loop
-module(game_loop).

%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").

%%
%% Exported Functions
%%
-export([loop/2]).

%%
%% API Functions
%%

loop(LastTime, GamePID) ->
    %StartLoopTime = util:get_time(), 
    CurrentTick = counter:increment(tick),	
    
    %Process events
    {GlobalRecalc, LocalRecalc} = process_events(CurrentTick),
    GlobalTriggered = game:get_perception(),

    %Build simple perception
    perception_recalculate(GlobalRecalc or GlobalTriggered),
   
    %Toggle off perception
    game:set_perception(false),
 
    %Update charge times
    BattleUnits = ets:tab2list(battle_unit),
    update_charge_times(BattleUnits),

    {NextTime, SleepTime} = calculate_sleep(LastTime),

    timer:sleep(SleepTime),
    loop(NextTime, GamePID).
%%
%% Local Functions
%%
%%

calculate_sleep(LastTime) ->
    CurrentTime = util:get_time(),
    CalcSleepTime = LastTime - CurrentTime + ?GAME_LOOP_TICK,

    check_sleep(CalcSleepTime, LastTime).

check_sleep(CalcSleepTime, LastTime) ->
    if
        CalcSleepTime =< 0 ->
            NextTime = LastTime + ?GAME_LOOP_TICK * 4,
            SleepTime = 1;
        true ->
            NextTime = LastTime + ?GAME_LOOP_TICK,
            SleepTime = CalcSleepTime            
    end,

    {NextTime, SleepTime}.

process_events(CurrentTick) ->
    Events = db:dirty_index_read(event, CurrentTick, #event.tick),

    check_events(Events, false, []).

check_events([], GlobalRecalc, LocalRecalc) ->
    {GlobalRecalc, LocalRecalc};
    
check_events([Event | Rest], PrevGlobalRecalc, PrevLocalRecalc) ->
    {GlobalRecalc, LocalRecalc}  = do_event(Event#event.type,
                                            Event#event.data,
                                            Event#event.player_process),
    NewGlobalRecalc = GlobalRecalc or PrevGlobalRecalc,
    NewLocalRecalc = [LocalRecalc | PrevLocalRecalc],

    check_events(Rest, NewGlobalRecalc, NewLocalRecalc).

do_event(move_obj, EventData, _PlayerPid) ->
    lager:info("Processing move_obj event: ~p", [EventData]),

    {Player, Id, {X, Y}} = EventData,
    Result = obj:move(Id, {X, Y}),

    lager:info("move_obj Result: ~p", [Result]),

    map:add_explored(Player, {X, Y}),

    true;

do_event(attack_obj, EventData, _PlayerPid) ->
    lager:info("Processing attack_obj event: ~p", [EventData]),

    {SourceId, TargetId} = EventData,

    %Create battle with list of source and target
    battle:create(SourceId, TargetId),

    true;

do_event(move_local_obj, EventData, _PlayerPid) ->
    lager:info("Processing move_local_obj event: ~p", [EventData]),

    {Global, Player, Id, {X, Y}} = EventData,

    false;

do_event(harvest, EventData, PlayerPid) ->
    lager:info("Processing harvest event: ~p", [EventData]),
    {ObjId, Resource} = EventData,

    %Update obj state
    obj:update_state(ObjId, none),

    %Create/update item
    resource:harvest(ObjId, Resource),

    %Send item perception to player pid
    send_to_process(PlayerPid, item_perception, item:get_by_owner(ObjId)), 
    
    false;

do_event(build, EventData, PlayerPid) ->
    lager:info("Processing build event: ~p", [EventData]),
    {_Id, _GlobalPos, _LocalPos, StructureId} = EventData,

    local:update_state(StructureId, none),

    %Send update state to player
    send_to_process(PlayerPid, local_state, {StructureId, none}),

    false;

do_event(_Unknown, _Data, _Pid) ->
    lager:info("Unknown event"),
    {unknown, false}.

perception_recalculate(false) ->
    nothing;

perception_recalculate(true) ->
    perception:recalculate().

update_charge_times([]) ->
    done;
update_charge_times([BattleUnit | Rest]) ->
    UnitId = BattleUnit#battle_unit.unit,
    Speed = BattleUnit#battle_unit.speed,
    Battle = BattleUnit#battle_unit.battle,
    
    NewChargeTime = charge_time:increment(UnitId, Speed),
    ActiveTurn = is_active_turn(NewChargeTime),
    process_active_turn(ActiveTurn, UnitId, Battle),

    update_charge_times(Rest).

is_active_turn(ChargeTime) when ChargeTime < 100 ->
    false;
is_active_turn(ChargeTime) when ChargeTime >= 100 ->
    true.

process_active_turn(true, UnitId, Battle) ->
    charge_time:reset(UnitId),
    %lager:info("Active turn: ~p", [UnitId]),
    battle:active_turn(Battle, UnitId);
process_active_turn(false, _UnitId, _Battle) ->
    none. 

send_to_process(Process, MessageType, Message) when is_pid(Process) ->
    lager:info("Sending ~p to ~p", [Message, Process]),
    Process ! {MessageType, Message}.
