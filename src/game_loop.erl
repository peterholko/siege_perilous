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
 
    %Check day/night transition
    process_transition(NumTick),

    %Process resource upkeep
    process_upkeep(NumTick),

    %Process rest
    process_rest(NumTick),

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
    lager:debug("Processing action event: ~p", [EventData]),
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
    lager:debug("Processing move_obj event: ~p", [EventData]),

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
    lager:debug("Processing harvest event: ~p", [EventData]),
    {ObjId, Resource, Pos, NumTicks, Repeat} = EventData,

    %Check if resource still exists
    case resource:is_valid(Pos) of
        true ->
            lager:debug("Creating/update item.."),
            %Create/update item
            NewItems = resource:harvest(ObjId, Resource, Pos),
            
            case Repeat of
                true ->
                    lager:debug("Repeating harvest event"),
                    game:add_event(PlayerPid, harvest, EventData, ObjId, NumTicks);
                false ->
                    %Update obj state
                    lager:debug("Updating obj state to none"),
                    obj:update_state(ObjId, none)
            end,
         
            lager:debug("Sending new items to player"),
            send_update_items(ObjId, NewItems, PlayerPid),
            send_to_process(PlayerPid, event_complete, {harvest, ObjId});
        false ->
            send_to_process(PlayerPid, event_failure, {harvest, invalid_resource})
    end,

    false; 

do_event(finish_build, EventData, _PlayerPid) ->
    lager:debug("Processing build event: ~p", [EventData]),
    {ObjId, StructureId} = EventData,

    obj:update_state(ObjId, none),
    NewStructure = obj:update_state(StructureId, none), 

    obj:set_wall_effect(NewStructure), 
    
    obj:update(StructureId, <<"hp">>, 1000),

    true;

do_event(process_resource, EventData, PlayerPid) ->
    lager:debug("Processing process_resource event: ~p", [EventData]),
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
    lager:debug("Processing craft event: ~p", [EventData]),
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
    lager:debug("Unknown event"),
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
process_transition(NumTick) when (NumTick rem (?TICKS_SEC * 60 )) =:= 0 ->
    [TimeOfDay] = db:read(world, timeofday),
    NewValue = timeofday(TimeOfDay#world.value),
    NewTimeOfDay = TimeOfDay#world { value = NewValue},
    db:write(NewTimeOfDay),
    lager:debug("Processing ~p transition", [NewValue]),

    process_spawn_mana(NewValue),

    Objs = ets:tab2list(obj),

    F = fun(Obj) ->
            apply_transition(NewTimeOfDay#world.value, Obj)
        end,

    lists:foreach(F, Objs);
process_transition(_) -> nothing.

process_upkeep(NumTick) when ((NumTick rem (?TICKS_SEC * 30)) =:= 0) and (NumTick > 0) ->
    process_mana_upkeep(),
    process_food_upkeep();
process_upkeep(_) -> nothing.

process_rest(NumTick) when ((NumTick rem (?TICKS_SEC * 30)) =:= 0) and (NumTick > 0) ->
    process_rest_state();
process_rest(_) -> nothing.

send_to_process(Process, MessageType, Message) when is_pid(Process) ->
    lager:debug("Sending ~p to ~p", [Message, Process]),
    Process ! {MessageType, Message};

send_to_process(_, _, _) ->
    none.

execute_npc(NumTick) when (NumTick rem 10) =:= 0 ->
    npc:replan(?UNDEAD),
    npc:run_plan(?UNDEAD);
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
    case obj:is_hero_nearby(Obj, Obj#obj.player) of
        true ->
            %Send item perception to player pid
            send_to_process(PlayerPid, new_items, NewItems);
        false ->
            nothing
    end.

apply_transition(night, Obj = #obj {id = Id, name = Name, vision = Vision}) when Name =:= <<"Zombie">> ->
    %Apply night effect 
    obj:add_effect(Id, <<"night_undead">>, none),

    ObjM = obj:get(Id),
    Hp = maps:get(<<"hp">>, ObjM),
    obj:update(Id, <<"hp">>, Hp * 10),

    %Increase vision x 10
    NewObj = Obj#obj {vision = erlang:trunc(Vision * 10)},
    db:write(NewObj);
apply_transition(day, Obj = #obj {id = Id, name = Name, vision = Vision}) when Name =:= <<"Zombie">> ->
    %Check if night undead is applied 
    case obj:has_effect(Id, <<"night_undead">>) of
        true ->
            obj:remove_effect(Id, <<"night_undead">>),

            %Decrease hp x 10
            ObjM = obj:get(Id),
            Hp = maps:get(<<"hp">>, ObjM),
            obj:update(Id, <<"hp">>, Hp / 10),

            %Decrease vision / 10
            NewObj = Obj#obj {vision = erlang:trunc(Vision / 10)},
            db:write(NewObj);
        false ->
            nothing
    end;
apply_transition(_, _) ->
    nothing.

process_spawn_mana(night) ->
    Monoliths = db:index_read(obj, ?MONOLITH, #obj.subclass),

    F = fun(Monolith) ->
            NearbyList = map:filter_pos(map:range(Monolith#obj.pos, 4)),
            spawn_mana(0, NearbyList)
        end,

    lists:foreach(F, Monoliths);
process_spawn_mana(day) -> 
    Manas = db:index_read(resource, <<"Mana">>, #resource.name),

    F = fun(Mana) ->
            lager:debug("Removing Obj: ~p", [Mana#resource.obj]),
            obj:remove(Mana#resource.obj),
            db:delete(resource, Mana#resource.index)
        end,

    lists:foreach(F, Manas).

spawn_mana(5, _NearbyList) -> nothing;
spawn_mana(N, NearbyList) -> 
    NumPos = length(NearbyList),
    RandomIndex = rand:uniform(NumPos),   
    RandomPos = lists:nth(RandomIndex, NearbyList),

    resource:create(<<"Mana">>, 5, RandomPos, true),
    NewNearbyList = lists:delete(RandomPos, NearbyList),
    spawn_mana(N + 1, NewNearbyList).

process_mana_upkeep() ->
    Monoliths = db:index_read(obj, ?MONOLITH, #obj.subclass),

    F = fun(Monolith) ->
            Mana = item:find_one({<<"owner">>, Monolith#obj.id, <<"name">>, <<"Mana">>}),
            update_mana(Monolith, Mana)
        end,

    lists:foreach(F, Monoliths).

update_mana(Monolith, #{}) -> 
    obj:update_state(Monolith#obj.id, disabled);
update_mana(Monolith, Mana) ->
    Id = maps:get(<<"id">>, Mana),
    Quantity = maps:get(<<"quantity">>, Mana),
    NewQuantity = Quantity - 1,

    item:update(Id, NewQuantity),

    case NewQuantity > 0 of
        false ->
            obj:update_state(Monolith#obj.id, disabled);
        true ->
            nothing
    end.

process_food_upkeep() ->
    Units = db:index_read(obj, unit, #obj.class),

    F = fun(Unit = #obj{player = Player}) when Player =/= ?UNDEAD ->
            case item:get_by_subclass(Unit#obj.id, <<"food">>) of
                [] ->
                    obj:add_effect(Unit#obj.id, <<"starving">>, none),

                    ObjM = obj:get(Unit#obj.id),
                    Hp = maps:get(<<"hp">>, ObjM),
                    NewHp = Hp - 1,
                    obj:update(Unit#obj.id, <<"hp">>, NewHp);
                [Item | _Rest] ->

                    ItemId = maps:get(<<"_id">>, Item),
                    NewQuantity = maps:get(<<"quantity">>, Item) - 1,
                    item:update(ItemId, NewQuantity)
            end;
            (_) -> nothing
        end,

    lists:foreach(F, Units).

process_rest_state() ->
    Objs = db:index_read(obj, rest, #obj.state),

    F = fun(Obj) ->
            case obj:has_effect(Obj#obj.id, <<"starving">>) of
                false ->
                    ObjM = obj:get(Obj#obj.id),
                    Hp = maps:get(<<"hp">>, ObjM),
                    BaseHp = maps:get(<<"base_hp">>, ObjM),

                    NewHp = update_hp(Hp, BaseHp),
                    obj:update(Obj#obj.id, <<"hp">>, NewHp);
                true ->
                    nothing
            end
        end,

    lists:foreach(F, Objs).

update_hp(Hp, BaseHp) when Hp < BaseHp -> Hp + 1;
update_hp(_Hp, _BaseHp) -> nothing.

timeofday(day) -> night;
timeofday(night) -> day.
