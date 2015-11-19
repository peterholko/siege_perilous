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
-export([loop/3]).

%%
%% API Functions
%%

loop(NumTick, LastTime, GamePID) ->
    %StartLoopTime = util:get_time(), 
    CurrentTick = counter:increment(tick),	
    
    %Process events
    EventsRecalc = process_events(CurrentTick),

    %Get triggered perception
    TriggeredRecalc = game:get_perception(),
   
    %Recalculate perception 
    recalculate(EventsRecalc or TriggeredRecalc),
  
    %Get triggered explored maps
    Explored = game:get_explored(),

    %Send out new explored maps
    process_explored(Explored),

    %Check day/night transition
    process_transition(NumTick),

    %Execute NPC actions
    execute_npc(NumTick),

    %Execute villager tasks
    execute_villager(NumTick),

    %Clean up
    clean_up(NumTick),
 
    %Toggle off perception and explored
    game:reset(),
 
    {NextTime, SleepTime} = calculate_sleep(LastTime),

    timer:sleep(SleepTime),
    loop(NumTick + 1, NextTime, GamePID).
%%
%% Local Functions
%%

calculate_sleep(LastTime) ->
    CurrentTime = util:get_time(),
    CalcSleepTime = LastTime - CurrentTime + ?GAME_LOOP_TICK,

    check_sleep(CalcSleepTime, LastTime).

check_sleep(CalcSleepTime, LastTime) ->
    Next = if
               CalcSleepTime =< 0 ->
                   NextTime = LastTime + ?GAME_LOOP_TICK * 4,
                   {NextTime, 1};
               true ->
                   NextTime = LastTime + ?GAME_LOOP_TICK,
                   {NextTime, CalcSleepTime}
           end,
    Next.

process_events(CurrentTick) ->
    Events = db:dirty_index_read(event, CurrentTick, #event.tick),
    check_events(Events, false).

check_events([], Recalc) ->
    Recalc;
check_events([Event | Rest], PrevRecalc) ->
    Recalc  = do_event(Event#event.type,
                       Event#event.data,
                       Event#event.player_process),

    NewRecalc = Recalc or PrevRecalc,
    
    db:dirty_delete(event, Event#event.id),

    check_events(Rest, NewRecalc).

do_event(action, EventData, PlayerPid) ->
    lager:info("Processing action event: ~p", [EventData]),
    Id = EventData,

    case db:read(action, Id) of
        [Action] ->
            combat:do_action(Action),
            send_to_process(PlayerPid, event_complete, {Action#action.type, Id});
        _ ->
            nothing
    end,

    false;

do_event(move_obj, EventData, PlayerPid) ->
    lager:info("Processing move_obj event: ~p", [EventData]),

    {_Player, Id, NewPos} = EventData,

    case obj:is_empty(NewPos) of
        true ->
            obj:move(Id, NewPos);
        false ->
            nothing
    end,
    
    send_to_process(PlayerPid, event_complete, {move_obj, Id}),

    true;

do_event(harvest, EventData, PlayerPid) ->
    lager:info("Processing harvest event: ~p", [EventData]),
    {ObjId, Resource, NumTicks, Repeat} = EventData,

    %Create/update item
    NewItems = resource:harvest(ObjId, Resource),
    
    case Repeat of
        true ->
            game:add_event(PlayerPid, harvest, EventData, ObjId, NumTicks);
        false ->
            %Update obj state
            obj:update_state(ObjId, none)
    end,
 
    send_update_items(ObjId, NewItems, PlayerPid),
    false; 

do_event(finish_build, EventData, _PlayerPid) ->
    lager:info("Processing build event: ~p", [EventData]),
    {ObjId, StructureId} = EventData,

    obj:update_state(ObjId, none),
    NewStructure = obj:update_state(StructureId, none), 

    obj:set_wall_effect(NewStructure), 
    
    obj:update(StructureId, <<"hp">>, 1000),

    true;

do_event(process_resource, EventData, PlayerPid) ->
    lager:info("Processing process_resource event: ~p", [EventData]),
    {StructureId, UnitId, NumTicks} = EventData,

    case structure:has_process_res(StructureId) of
        true ->
            NewItems = structure:process(StructureId),    
            send_update_items(StructureId, NewItems, PlayerPid),
            game:add_event(PlayerPid, process_resource, EventData, UnitId, NumTicks);
        false ->
            obj:update_state(UnitId, none)
    end,

    false;

do_event(craft, EventData, PlayerPid) ->
    lager:info("Processing craft event: ~p", [EventData]),
    {StructureId, UnitId, Recipe} = EventData,

    case structure:check_recipe_req(StructureId, Recipe) of
        true ->
            NewItem = structure:craft(StructureId, Recipe),
            send_update_items(StructureId, [NewItem], PlayerPid);
        false ->
            nothing
    end,

    obj:update_state(UnitId, none),

    false;

do_event(_Unknown, _Data, _Pid) ->
    lager:info("Unknown event"),
    false.

recalculate(false) ->
    done;
recalculate(true) ->
    perception:recalculate().

process_explored([]) ->
    done;
process_explored([Player | Rest]) ->
    [Conn] = db:dirty_read(connection, Player),
    ExploredTiles = map:get_explored(Player, new),
    send_to_process(Conn#connection.process, map, ExploredTiles),

    process_explored(Rest).

process_transition(0) -> nothing;
process_transition(NumTick) when (NumTick rem 600) =:= 0 ->
    Objs = ets:tab2list(obj),
    [TimeOfDay] = db:read(world, timeofday),
    NewTimeOfDay = TimeOfDay#world { value = timeofday(TimeOfDay#world.value)},
    db:write({world, timeofday, NewTimeOfDay}),

    F = fun(Obj) ->
            apply_transition(NewTimeOfDay#world.value, Obj)
        end,

    lists:foreach(F, Objs);
process_transition(_) -> nothing.

send_to_process(Process, MessageType, Message) when is_pid(Process) ->
    lager:debug("Sending ~p to ~p", [Message, Process]),
    Process ! {MessageType, Message};

send_to_process(_, _, _) ->
    none.

execute_npc(NumTick) when (NumTick rem 10) =:= 0 ->
    npc:replan(99),
    npc:run_plan(99);
execute_npc(_) ->
    nothing.

execute_villager(NumTick) when (NumTick rem 50) =:= 0 ->
    villager:check_task();
execute_villager(_) ->
    nothing.
%TODO Fix the frequency of the clean up, should be based off when the obj dies
clean_up(NumTick) when (NumTick rem 2000) =:= 0 ->
    lager:debug("Cleaning up dead objs"),
    Objs = ets:tab2list(obj),

    F = fun(Obj) ->
            remove(Obj#obj.state, Obj)
        end,


    lists:foreach(F, Objs);    

clean_up(_) ->
    nothing. 

remove(dead, Obj) ->
    obj:remove(Obj#obj.id);
remove(_, _Obj) ->
    nothing.

send_update_items(ObjId, NewItems, PlayerPid) ->
    [Obj] = db:read(obj, ObjId),
    case obj:is_nearby_hero(Obj, Obj#obj.player) of
        true ->
            %Send item perception to player pid
            send_to_process(PlayerPid, new_items, NewItems);
        false ->
            nothing
    end.

apply_transition(night, Obj = #obj {name = Name, vision = Vision}) when Name =:= <<"Zombie">> ->
    NewObj = Obj#obj {vision = Vision * 10},
    db:write(NewObj);
apply_transition(day, Obj = #obj {name = Name, vision = Vision}) when Name =:= <<"Zombie">> ->
    NewObj = Obj#obj {vision = Vision / 10},
    db:write(NewObj);
apply_transition(_, _) ->
    nothing.

timeofday(day) -> night;
timeofday(night) -> day.
