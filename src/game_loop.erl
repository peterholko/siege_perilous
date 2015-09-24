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
    {_GlobalRecalc, LocalRecalc} = process_events(CurrentTick),

    %Get triggered perception
    {_GlobalTriggered, LocalTriggered} = game:get_perception(),
   
    %Recalculate local perception 
    local_recalculate(util:unique_list(LocalRecalc ++ LocalTriggered)),
  
    %Get triggered explored maps
    Explored = game:get_explored(),

    %Send out new explored maps
    process_explored(Explored),

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
    check_events(Events, false, []).

check_events([], GlobalRecalc, LocalRecalc) ->
    {GlobalRecalc, LocalRecalc};
    
check_events([Event | Rest], PrevGlobalRecalc, PrevLocalRecalc) ->
    {GlobalRecalc, LocalRecalc}  = do_event(Event#event.type,
                                            Event#event.data,
                                            Event#event.player_process),
    NewGlobalRecalc = GlobalRecalc or PrevGlobalRecalc,
    NewLocalRecalc = add_local_recalc(PrevLocalRecalc, LocalRecalc),

    db:dirty_delete(event, Event#event.id),

    check_events(Rest, NewGlobalRecalc, NewLocalRecalc).

add_local_recalc(PrevLocalRecalc, {GlobalPos, true}) ->
    [GlobalPos | PrevLocalRecalc];
add_local_recalc(PrevLocalRecalc, _) ->
    PrevLocalRecalc.

do_event(move_obj, EventData, _PlayerPid) ->
    lager:info("Processing move_obj event: ~p", [EventData]),

    {Player, Id, GlobalPos} = EventData,

    %Move global obj and add explored
    obj:move(Id, GlobalPos),
    map:add_explored(Player, GlobalPos),

    {true, false};

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

    {false, false};

do_event(move_local_obj, EventData, PlayerPid) ->
    lager:info("Processing move_local_obj event: ~p", [EventData]),

    {GlobalPos, _Player, Id, NewPos} = EventData,

    case local:is_empty(NewPos) of
        true ->
            local:move(Id, NewPos);
        false ->
            nothing
    end,
    
    send_to_process(PlayerPid, event_complete, {move_local_obj, Id}),

    {false, {GlobalPos, true}};

do_event(exit_local, EventData, PlayerPid) ->
    lager:info("Processing exit_local event: ~p", [EventData]),
    
    {GlobalObjId, GlobalPos} = EventData,

    local:exit_map(GlobalObjId),
    obj:update_state(GlobalObjId, none),

    %Remove any local objs from local map and 
    %check if local perception update is needed
    local:exit_map(GlobalObjId),
    
    %Send exit local to player pid
    send_to_process(PlayerPid, exit_local, GlobalObjId),

    {true, {GlobalPos, true}};    

do_event(harvest, EventData, PlayerPid) ->
    lager:info("Processing harvest event: ~p", [EventData]),
    {LocalObjId, Resource, NumTicks, Repeat} = EventData,

    %Create/update item
    NewItems = resource:harvest(LocalObjId, Resource),
    
    case Repeat of
        true ->
            game:add_event(PlayerPid, harvest, EventData, LocalObjId, NumTicks);
        false ->
            %Update obj state
            local:update_state(LocalObjId, none)
    end,
 
    send_update_items(LocalObjId, NewItems, PlayerPid),
   
    {false, false};

do_event(finish_build, EventData, _PlayerPid) ->
    lager:info("Processing build event: ~p", [EventData]),
    {LocalObjId, GlobalPos, StructureId} = EventData,

    local:update_state(LocalObjId, none),
    NewStructure = local:update_state(StructureId, none), 

    local:set_wall_effect(NewStructure), 
    
    local_obj:update(StructureId, <<"hp">>, 1000),

    {false, {GlobalPos, true}};

do_event(process_resource, EventData, PlayerPid) ->
    lager:info("Processing process_resource event: ~p", [EventData]),
    {StructureId, UnitId, NumTicks} = EventData,

    case structure:has_process_res(StructureId) of
        true ->
            NewItems = structure:process(StructureId),    
            send_update_items(StructureId, NewItems, PlayerPid),
            game:add_event(PlayerPid, process_resource, EventData, UnitId, NumTicks);
        false ->
            local:update_state(UnitId, none)
    end,

    {false, false};

do_event(_Unknown, _Data, _Pid) ->
    lager:info("Unknown event"),
    false.

local_recalculate([]) ->
    done;
local_recalculate([GlobalPos | Rest]) ->
    l_perception:recalculate(GlobalPos),
    local_recalculate(Rest).

process_explored([]) ->
    done;
process_explored([{Player, GlobalPos} | Rest]) ->
    [Conn] = db:dirty_read(connection, Player),
    ExploredTiles = map:get_local_explored(Player, GlobalPos, new),
    send_to_process(Conn#connection.process, local_map, ExploredTiles),

    process_explored(Rest).

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

clean_up(NumTick) when (NumTick rem 200) =:= 0 ->
    lager:debug("Cleaning up dead local objs"),
    LocalObjs = db:index_read(local_obj, 1, #local_obj.global_pos),

    F = fun(LocalObj) ->
            remove(LocalObj#local_obj.state, LocalObj)
        end,


    lists:foreach(F, LocalObjs);    

clean_up(_) ->
    nothing. 

remove(dead, LocalObj) ->
    local_obj:remove(LocalObj#local_obj.id),
    local:remove(LocalObj).

send_update_items(LocalObjId, NewItems, PlayerPid) ->
    [LocalObj] = db:read(local_obj, LocalObjId),
    case local_obj:is_nearby_hero(LocalObj, LocalObj#local_obj.player) of
        true ->
            %Send item perception to player pid
            send_to_process(PlayerPid, new_items, NewItems);
        false ->
            nothing
    end.

