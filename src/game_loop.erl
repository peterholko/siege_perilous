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
    RecalcPerception = process_events(CurrentTick),
    %Build simple perception
    perception_recalculate(RecalcPerception),
    
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

    check_events(Events, false).

check_events([], RecalcPerception) ->
    RecalcPerception;
    
check_events([Event | Rest], PrevRecalc) ->
    
    Recalc = do_event(Event#event.type,
                      Event#event.data,
                      Event#event.player_process),

    check_events(Rest, Recalc or PrevRecalc).

do_event(move_obj, EventData, _PlayerPid) ->
    lager:info("Processing move_obj event: ~p", [EventData]),

    {Player, Id, {X, Y}} = EventData,
    Result = map:move_obj(Id, {X, Y}),

    lager:info("move_obj Result: ~p", [Result]),

    map:add_explored(Player, {X, Y}),

    true;

do_event(attack_obj, EventData, _PlayerPid) ->
    lager:info("Processing attack_obj event: ~p", [EventData]),

    {SourceId, TargetId} = EventData,

    %Create battle with list of source and target
    battle:create([SourceId, TargetId]),

    true;

do_event(_Unknown, _Data, _Pid) ->
    lager:info("Unknown event"),
    false.

perception_recalculate(false) ->
    nothing;

perception_recalculate(true) ->
    perception_manager:recalculate().
