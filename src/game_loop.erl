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

    %NPC generate plan and run plan
    npc_create_plan(NumTick),
    npc_run_plan(NumTick),

    %Villager generate plan and run plan
    villager_create_plan(NumTick),
    villager_run_plan(NumTick),

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
                       Event#event.pid),

    NewRecalc = Recalc or PrevRecalc,
    
    db:dirty_delete(event, Event#event.id),

    check_events(Rest, NewRecalc).

do_event(attack, EventData, PlayerPid) ->
    lager:debug("Processing action event: ~p", [EventData]),
    message:send_to_process(PlayerPid, event_complete, {attack, EventData}),
    false;

do_event(defend, EventData, PlayerPid) ->
    lager:debug("Processing defend event: ~p", [EventData]),
    
    {ObjId, DefendType} = EventData,

    effect:remove(ObjId, DefendType),

    message:send_to_process(PlayerPid, event_complete, {defend, EventData}),
    false;

do_event(move, EventData, PlayerPid) ->
    lager:debug("Processing move_obj event: ~p", [EventData]),
    {_Player, Id, NewPos} = EventData,

    case obj:is_empty(Id, NewPos) of
        true ->
            obj:move(Id, NewPos);
        false ->
            obj:update_state(Id, none)
    end,

    message:send_to_process(PlayerPid, event_complete, {move, Id}),
    true;

do_event(ford, EventData, PlayerPid) ->
    lager:debug("Processing ford event: ~p", [EventData]),
    {_Player, Id, Pos, NewPos} = EventData,

    case map:get_ford_pos(Pos, NewPos) of
        none -> nothing;
        NextPos -> obj:move(Id, NextPos)
    end,

    message:send_to_process(PlayerPid, event_complete, {move, Id}),
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
            game:send_update_items(ObjId, NewItems, PlayerPid),
            message:send_to_process(PlayerPid, event_complete, {harvest, ObjId});
        false ->
            message:send_to_process(PlayerPid, event_failure, {harvest, invalid_resource})
    end,

    false; 

do_event(sharvest, EventData, _Pid) ->
    {VillagerId, StructureId} = EventData,
    Villager = db:read(obj, VillagerId),
    Structure = db:read(obj, StructureId),

    case structure:harvest(Villager, Structure) of
        success ->
            message:send_to_process(global:whereis_name(villager), event_complete, {harvest, VillagerId});
        {error, ErrMsg} ->
            lager:debug("sharvest error: ~p", [ErrMsg]),
            FailureData = StructureId,
            message:send_to_process(global:whereis_name(villager), event_failure, {harvest, VillagerId, ErrMsg, FailureData})
    end,

    false;

do_event(finish_build, EventData, _PlayerPid) ->
    lager:debug("Processing build event: ~p", [EventData]),
    {ObjId, StructureId} = EventData,

    %Set unit builder state to none
    obj:update_state(ObjId, none),

    %Set structure state to none
    obj:update_state(StructureId, none), 

    %Set structure hp to max
    BaseHp = obj_attr:value(StructureId, <<"base_hp">>),
    obj_attr:set(StructureId, <<"hp">>, BaseHp),

    true;

do_event(process_resource, EventData, PlayerPid) ->
    lager:debug("Processing process_resource event: ~p", [EventData]),
    {StructureId, UnitId, NumTicks} = EventData,

    case structure:has_process_res(StructureId) of
        true ->
            NewItems = structure:process(StructureId),    
            game:send_update_items(StructureId, NewItems, PlayerPid),
            game:add_event(PlayerPid, process_resource, EventData, UnitId, NumTicks);
        false ->
            obj:update_state(UnitId, none)
    end,

    false;

do_event(craft, EventData, PlayerPid) ->
    lager:debug("Processing craft event: ~p", [EventData]),
    {StructureId, UnitId, Recipe} = EventData,
    VillagerId = villager:get_by_structure(StructureId),

    case structure:check_recipe_req(StructureId, Recipe) of
        true ->
            %Craft items
            NewItems = structure:craft(StructureId, Recipe),
            
            %Send update to player
            game:send_update_items(StructureId, NewItems, PlayerPid),
            
            %Send event_complete to villager
            message:send_to_process(global:whereis_name(villager), event_complete, {craft, VillagerId});
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
    message:send_to_process(Conn#connection.process, map, ExploredTiles),

    process_explored(Rest).

process_transition(0) -> nothing;
process_transition(NumTick) when ((NumTick + ?TICKS_MIN) rem (?TICKS_MIN * 4)) =:= 0 ->
    %Transition from day to blood moon
    transition(bloodmoon),
    bloodmoon(),
    send_world_update(time, bloodmoon); 
process_transition(NumTick) when ((NumTick + ?TICKS_MIN) rem (?TICKS_MIN * 2)) =:= 0 ->
    %Transition from day to night
    transition(night),
    send_world_update(time, night); 
process_transition(NumTick) when (NumTick rem (?TICKS_MIN * 2)) =:= 0 ->
    %Transition from night/bloodmoon to day 
    transition(day),
    send_world_update(time, day);
process_transition(_) -> nothing.

process_upkeep(NumTick) when ((NumTick rem (?TICKS_SEC * 30)) =:= 0) and (NumTick > 0) ->
    mana_upkeep(),
    food_upkeep(),
    structure_upkeep();
process_upkeep(_) -> nothing.

process_rest(NumTick) when ((NumTick rem (?TICKS_SEC * 10)) =:= 0) and (NumTick > 0) ->
    process_rest_state(NumTick);
process_rest(_) -> nothing.

npc_create_plan(NumTick) when (NumTick rem (?TICKS_SEC * 2)) =:= 0 ->
    npc:replan(?UNDEAD);
npc_create_plan(_) ->
    nothing.

npc_run_plan(NumTick) when ((NumTick + (?TICKS_SEC)) rem (?TICKS_SEC * 2)) =:= 0 ->
   npc:run_plan(?UNDEAD);
npc_run_plan(_) ->
    nothing.

villager_create_plan(NumTick) when (NumTick rem (?TICKS_SEC * 5)) =:= 0 ->
    villager:create_plan();
villager_create_plan(_) ->
    nothing.

villager_run_plan(NumTick) when ((NumTick + (?TICKS_SEC)) rem (?TICKS_SEC * 5)) =:= 0 ->
    villager:run_plan();
villager_run_plan(_) ->
    nothing.

clean_up(NumTick) when (NumTick rem (?TICKS_MIN * 3)) =:= 0 ->
    lager:debug("Cleaning up dead objs"),
    Objs = ets:tab2list(obj),

    F = fun(Obj) ->
            case (NumTick - Obj#obj.modtick) > (?TICKS_MIN * 3) of
                true ->
                    case Obj#obj.subclass =:= <<"hero">> of
                        false ->
                            case Obj#obj.state of
                                dead -> obj:remove(Obj#obj.id);
                                founded -> obj:remove(Obj#obj.id);
                                _ -> nothing
                            end;
                        true ->
                            nothing
                    end;
                false ->
                    nothing
            end
        end,

    lists:foreach(F, Objs);    

clean_up(_) ->
    nothing. 

transition(Time) ->
    NewTimeOfDay = #world {attr = time,
                           value = Time},
    db:write(NewTimeOfDay),

    Objs = ets:tab2list(obj),

    F = fun(Obj) ->
            apply_transition(Time, Obj)
        end,

    lists:foreach(F, Objs).

apply_transition(bloodmoon, Obj = #obj {id = Id, name = Name, vision = Vision}) when Name =:= <<"Zombie">> ->
    %Apply night effect 
    effect:add(Id, ?BLOODMOON, none),

    %Increase vision x 5
    NewObj = Obj#obj {vision = erlang:trunc(Vision * 5)},
    db:write(NewObj);
apply_transition(day, Obj = #obj {id = Id, name = Name, vision = Vision}) when Name =:= <<"Zombie">> ->
    %Check if night undead is applied 
    case effect:has_effect(Id, ?BLOODMOON) of
        true ->
            effect:remove(Id, ?BLOODMOON),

            %Decrease vision / 10
            NewObj = Obj#obj {vision = erlang:trunc(Vision / 5)},
            db:write(NewObj);
        false ->
            nothing
    end;
apply_transition(bloodmoon, Obj = #obj{player = Player, 
                                       class = Class,
                                       vision = Vision}) when (Player > ?NPC_ID) and 
                                                              (Class =:= unit) and 
                                                              (Vision > 0) ->
    NewObj = Obj#obj {vision = 1},
    db:write(NewObj);
apply_transition(night, Obj = #obj{player = Player, 
                                   class = Class,
                                   vision = Vision}) when (Player > ?NPC_ID) and 
                                                          (Class =:= unit) and 
                                                          (Vision > 0) ->
    NewObj = Obj#obj {vision = 1},
    db:write(NewObj);
apply_transition(day, Obj = #obj{player = Player, 
                                 class = Class,
                                 vision = Vision}) when (Player > ?NPC_ID) and 
                                                        (Class =:= unit) and 
                                                        (Vision > 0) ->
    NewObj = Obj#obj {vision = 2},
    db:write(NewObj);
apply_transition(_, _) ->
    nothing.

mana_upkeep() ->
    Monoliths = db:index_read(obj, ?MONOLITH, #obj.subclass),

    F = fun(Monolith) ->
            case item:get_by_subclass(Monolith#obj.id, ?MANA) of
               [] ->
                    obj:update_state(Monolith#obj.id, disabled);
               [Mana | _Rest] -> 
                    Id = maps:get(<<"id">>, Mana),
                    NewQuantity = maps:get(<<"quantity">>, Mana) - 1,
                    item:update(Id, NewQuantity)
            end
        end,

    lists:foreach(F, Monoliths).

food_upkeep() ->
    Units = db:index_read(obj, unit, #obj.class),

    F = fun(Unit = #obj{player = Player}) when Player =/= ?UNDEAD ->
            case item:get_by_subclass(Unit#obj.id, ?FOOD) of
                [] ->
                    effect:add(Unit#obj.id, ?STARVING, none),
                    obj:update_hp(Unit#obj.id, -1),

                    game:send_update_stats(Player, Unit#obj.id); 
                [Item | _Rest] ->
                    effect:remove(Unit#obj.id, ?STARVING),
                    ItemId = maps:get(<<"id">>, Item),
                    NewQuantity = maps:get(<<"quantity">>, Item) - 1,
                    item:update(ItemId, NewQuantity)
            end;
            (_) -> nothing
        end,

    lists:foreach(F, Units).

structure_upkeep() ->
    Structures = obj:get_by_attr([{class, structure}, {state, none}]),

    F = fun(Structure) ->
            structure:process_upkeep(Structure)
        end,

    lists:foreach(F, Structures).

process_rest_state(NumTick) ->
    Objs = db:index_read(obj, resting, #obj.state),

    F = fun(Obj) ->
            case effect:has_effect(Obj#obj.id, <<"Starving">>) of
                false -> obj:update_hp(Obj#obj.id, 1);
                true -> nothing
            end,

            check_random_event(NumTick, Obj)
        end,

    lists:foreach(F, Objs).

send_world_update(Attr, Value) ->
    Connections = ets:tab2list(connection),
    BinAttr = atom_to_binary(Attr, latin1),
    BinValue = atom_to_binary(Value, latin1),
    Message = #{BinAttr => BinValue},

    F = fun(Connection) ->
            message:send_to_process(Connection#connection.process, world, Message)
        end,

    lists:foreach(F, Connections).

bloodmoon() ->
    %Power up current zombies
    Objs = ets:tab2list(obj),

    F = fun(Obj, Acc) ->
            zombie_powerup(Obj) + Acc
        end,

    NumZombies = lists:foldl(F, 0, Objs),

    %Spawn more zombies if less than max zombies
    case NumZombies < ?MAX_ZOMBIES of
        true ->
            Num = counter:increment(bloodmoon),	
            Rand = util:rand(5 + Num),
            npc_mgr:spawn_zombies(Rand);
        false ->
            nothing
    end.
 
zombie_powerup(#obj{id = Id, name = Name}) when Name =:= <<"Zombie">> ->
    Hp = obj_attr:value(Id, <<"hp">>),
    obj_attr:set(Id, <<"hp">>, Hp + 10),
    1; %Return counted 1 zombie
zombie_powerup(_) -> 0.

check_random_event(NumTick, Obj = #obj{subclass = Subclass}) when Subclass =:= <<"hero">> ->
    [State] = db:read(state, Obj#obj.id),

    TickDiff = NumTick - State#state.modtick,

    case TickDiff > (?TICKS_SEC * 10) of
        true -> 
            REvent = revent:create(),
            REventMap = revent:to_map(REvent),
            
            obj:update_state(Obj, revent, REvent#revent.id),

            game:send_revent(Obj#obj.player, REventMap);
        false -> nothing
    end;
check_random_event(_, _) -> nothing.

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
    RandomIndex = util:rand(NumPos),   
    RandomPos = lists:nth(RandomIndex, NearbyList),

    resource:create(<<"Mana">>, 5, RandomPos, true),
    NewNearbyList = lists:delete(RandomPos, NearbyList),
    spawn_mana(N + 1, NewNearbyList).

