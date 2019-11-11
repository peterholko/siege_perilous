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
-export([npc_process/1]).

%%
%% API Functions
%%

loop(NumTick, LastTime, GamePID) ->
    %StartLoopTime = util:get_time(), 
    CurrentTick = counter:increment(tick),	

    obj:process_obj_stats(),

    %Process dead and deleting objs
    process_deleting_objs(NumTick),
    process_dead_objs(NumTick),

    %Check day/night transition
    %process_transition(NumTick),

    %Process resource upkeep
    %process_upkeep(NumTick),

    %Process rest
    process_rest(NumTick),


    %Process Obj events
    ObservedEvents = process_obj_events(CurrentTick),

    case ObservedEvents =/= [] of
        true ->
            lager:debug("ObservedEvents: ~p",[ObservedEvents]),
            perception:process_observed_events(ObservedEvents);
        false ->
            nothing
    end,

    %Process effects
    process_effects(CurrentTick),

    %Process events
    process_events(CurrentTick),

    %Get triggered explored maps
    Explored = game:get_explored(),

    %Send out new explored maps
    process_explored(Explored),

    %Villager create plan and run it
    villager_process(NumTick),

    %NPC create plan and run it
    npc_process(NumTick),

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

process_obj_events(CurrentTick) ->    
    ObjEvents = db:dirty_index_read(obj_event, CurrentTick, #obj_event.tick),

    AllObjs = ets:tab2list(obj),
    Observers = lists:filter(fun(Obj) -> obj:has_vision(Obj) end, AllObjs),

    check_obj_events(ObjEvents, Observers, []).

check_obj_events([], _Observers, PerceptionEvents) ->
    PerceptionEvents;
check_obj_events([ObjEvent | Rest], Observers, All) ->
    lager:debug("*** Processing ObjEvent: ~p ", [ObjEvent]),
    ProcessedEvent  = do_obj_event(ObjEvent#obj_event.event,
                                   ObjEvent#obj_event.pid,
                                   ObjEvent#obj_event.data),

    %Check if new observer created
    NewObservers = check_new_observer(ProcessedEvent, Observers),

    %Update and send perceptions
    ObservedEvents = perception:check_event_visible(ProcessedEvent, NewObservers),

    %Send completion event to calling process
    ObjId = obj_id(ObjEvent#obj_event.event, ObjEvent#obj_event.data),

    message:send_to_process(ObjEvent#obj_event.pid,
                            event_complete, 
                            {ObjEvent#obj_event.event, ObjId}),


    check_obj_events(Rest, NewObservers, ObservedEvents ++ All).

check_new_observer(#obj_create{obj = Obj}, Observers) -> 
    case Obj#obj.vision > 0 of
        true ->
            [Obj | Observers];
        false ->
            Observers
    end;
check_new_observer(_, Observers) -> 
    Observers.

obj_id(obj_create, Id) -> Id;
obj_id(obj_update, {Id, _Attr, _Value}) -> Id;
obj_id(obj_move, {Id, _SourcePos, _DestPos}) -> Id;
obj_id(obj_delete, Id) -> Id;
obj_id(obj_hide, Id) -> Id;
obj_id(obj_reveal, Id) -> Id.

do_obj_event(obj_create, _Process, Id) ->
    lager:debug("obj_create: ~p", [Id]),

    NewObj = obj:process_create(Id),

    ObjCreate = #obj_create {obj = NewObj,
                             source_pos = NewObj#obj.pos},

    ObjCreate;

do_obj_event(obj_update, _Process, {ObjId, _Attr, Value}) ->
    lager:debug("obj_update: ~p ~p", [ObjId, Value]),

    NewObj = obj:process_update_state(ObjId, Value),

    ObjUpdate = #obj_update {obj = NewObj, 
                             source_pos = NewObj#obj.pos, 
                             attr = <<"state">>,
                             value = Value},
    ObjUpdate;

do_obj_event(obj_move, _Process, {ObjId, SourcePos, DestPos}) ->
    lager:info("obj_move: ~p ~p ~p", [ObjId, SourcePos, DestPos]),

    NewObj = obj:process_move(ObjId, DestPos),

    %Check if obj was actually able to move
    ObjMove = case obj:pos(NewObj) =:= SourcePos of
                  true -> 
                      #obj_update {obj = NewObj,
                                   source_pos = NewObj#obj.pos,
                                   attr = <<"state">>,
                                   value = none};
                  false -> 
                      %Recalculate perception for obj that moved
                      %perception:recalculate(NewObj),

                      lager:debug("Moved Obj: ~p",[NewObj]),
                      #obj_move {obj = NewObj,
                                 source_pos = SourcePos,
                                 dest_pos = NewObj#obj.pos}

              end,

    %Send event complete to source of event

    ObjMove;

do_obj_event(obj_delete, _Process, Id) ->
    Obj = obj:get(Id),
    
    %Set the state of the object to deleting
    obj:process_deleting(Obj),

    ObjDelete = #obj_delete{obj = Obj,
                            source_pos = obj:pos(Obj)},
    
    ObjDelete;

do_obj_event(obj_hide, _Process, Id) ->
    NewObj = obj:process_update_state(Id, ?HIDING),
    ObjHide = #obj_hide{obj = NewObj},

    ObjHide;

do_obj_event(obj_reveal, _Process, Id) ->
    lager:info("Processing REVEAL!"),
    NewObj = obj:process_update_state(Id, ?NONE),
    ObjReveal = #obj_reveal{obj = NewObj},

    ObjReveal.

process_events(CurrentTick) ->
    Events = db:dirty_index_read(event, CurrentTick, #event.tick),

    F = fun(Event) ->
            do_event(Event#event.type,
                     Event#event.data,
                     Event#event.pid),

            db:delete(event, Event#event.id)
        end,

    lists:foreach(F, Events).

do_event(attack, EventData, PlayerPid) ->
    lager:debug("Processing action event: ~p", [EventData]),
    ObjId = EventData,
    message:send_to_process(PlayerPid, event_complete, {attack, ObjId}),
    false;

do_event(defend, EventData, PlayerPid) ->
    lager:debug("Processing defend event: ~p", [EventData]),
    
    {ObjId, DefendType} = EventData,

    effect:remove(ObjId, DefendType),    

    message:send_to_process(PlayerPid, event_complete, {defend, ObjId}),
    false;

do_event(cast, EventData, PlayerPid) ->
    lager:info("Processing cast event: ~p", [EventData]),

    {SourceId, TargetId, TargetType, Spell} = EventData,
    
    Source = obj:get(SourceId),

    Target = case TargetType of
                 obj -> obj:get(TargetId);
                 item -> item:get_rec(TargetId)
             end,

    magic:cast(Source, Target, TargetType, Spell),

    message:send_to_process(PlayerPid, event_complete, {cast, SourceId}),
    false;

do_event(ford, EventData, PlayerPid) ->
    lager:debug("Processing ford event: ~p", [EventData]),
    {_Player, Id, Pos, NewPos} = EventData,

    case map:get_ford_pos(Pos, NewPos) of
        none -> nothing;
        NextPos -> obj:move(Id, NextPos)
    end,

    message:send_to_process(PlayerPid, event_complete, {move, Id}),
    true;

do_event(explore, EventData, PlayerPid) ->
    lager:info("Processing explore event: ~p", [EventData]),
    ObjId = EventData,
    Obj = obj:get(ObjId),

    Result = resource:explore(ObjId, obj:pos(Obj)),
    lager:info("Explore Result: ~p", [Result]),

    obj:update_state(ObjId, none),

    message:send_to_process(PlayerPid, survey, Result),
    message:send_to_process(PlayerPid, event_complete, {explore, ObjId});

do_event(gather, EventData, PlayerPid) ->
    lager:debug("Processing gather event: ~p", [EventData]),

    {ObjId, ResourceType, Pos} = EventData,

    case resource:is_valid_type(ResourceType, Pos) of
        true ->
            lager:info("Gathering resource by type"),
            
            resource:gather_by_type(ObjId, ResourceType, Pos),
            message:send_to_process(PlayerPid, event_complete, {gather, ObjId}),
            obj:update_state(ObjId, none);
        false ->
            nothing
    end;

do_event(harvest, EventData, PlayerPid) ->
    lager:debug("Processing harvest event: ~p", [EventData]),
    {ObjId, Resource, Pos, NumTicks, Repeat} = EventData,

    %Check if resource still exists
    case resource:is_valid(Resource, Pos) of
        true ->
            lager:debug("Creating/update item.."),

            %Create/update item
            case resource:harvest(ObjId, Resource, Pos) of
                {error, ErrMsg} ->
                    message:send_to_process(PlayerPid, event_failure, ErrMsg);
                NewItems ->
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
                    message:send_to_process(PlayerPid, event_complete, {harvest, ObjId})
            end;
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
    end;

do_event(finish_build, EventData, _PlayerPid) ->
    lager:info("Processing build event: ~p", [EventData]),
    {ObjId, StructureId} = EventData,

    %Set unit builder state to none
    game:add_obj_update(self(), ObjId, ?STATE, ?NONE, 1),

    %Set structure state to none
    game:add_obj_update(self(), StructureId, ?STATE, ?NONE, 1),

    %Set structure hp to max
    BaseHp = obj_attr:value(StructureId, <<"base_hp">>),
    obj_attr:set(StructureId, <<"hp">>, BaseHp);

do_event(refine, EventData, PlayerPid) ->
    lager:debug("Processing refine event: ~p", [EventData]),
    {StructureId, UnitId, NumTicks} = EventData,

    case structure:has_refine_resources(StructureId) of
        true ->
            NewItems = structure:refine(StructureId),    
            game:send_update_items(StructureId, NewItems, PlayerPid),
            game:add_event(PlayerPid, refine, EventData, UnitId, NumTicks);
        false ->
            obj:update_state(UnitId, none)
    end;

do_event(craft, EventData, PlayerPid) ->
    lager:debug("Processing craft event: ~p", [EventData]),
    {StructureId, UnitId, Recipe} = EventData,
    VillagerId = villager:get_by_structure(StructureId),

    case structure:check_recipe_req(StructureId, Recipe) of
        true ->
            %Craft items
            NewItems = recipe:craft(StructureId, Recipe),
            
            %Send update to player
            game:send_update_items(StructureId, NewItems, PlayerPid),
            
            %Send event_complete to villager
            message:send_to_process(global:whereis_name(villager), event_complete, {craft, VillagerId});
        false ->
            nothing
    end,

    obj:update_state(UnitId, none);

do_event(?DRINKING, EventData, _PlayerPid) ->
    lager:info("Processing drink event: ~p", [EventData]),
    VillagerId = EventData,

    %TODO add different values for different foods
    obj:update_thirst(VillagerId, -480),

    effect:remove(VillagerId, ?THIRSTY),

    message:send_to_process(global:whereis_name(villager), event_complete, {drink, VillagerId}),

    obj:update_state(VillagerId, none);


do_event(?EATING, EventData, _PlayerPid) ->
    lager:info("Processing eat event: ~p", [EventData]),
    VillagerId = EventData,

    %TODO add different values for different foods
    obj:update_hunger(VillagerId, -480),

    effect:remove(VillagerId, ?HUNGRY),

    message:send_to_process(global:whereis_name(villager), event_complete, {eat, VillagerId}),

    obj:update_state(VillagerId, none);

do_event(?SLEEPING, EventData, PlayerPid) ->
    lager:info("Processing sleeping event: ~p", [EventData]),

    VillagerId = EventData,

    case obj:process_sleep(VillagerId) of
        more_sleep ->
            game:add_event(PlayerPid, ?SLEEPING, VillagerId, VillagerId, ?TICKS_SEC * 30);
        rested ->
            message:send_to_process(global:whereis_name(villager), event_complete, {?SLEEPING, VillagerId})
    end;
    
do_event(login, EventData, Pid) ->
    lager:info("Processing login event: ~p", [EventData]),
    PlayerId = EventData,

    Objs = perception:get_by_player(PlayerId),
    Map = map:get_explored(PlayerId, all),    

    Perception = #{<<"map">> => Map,
                   <<"objs">> => Objs},

    %TODO move this code
    case db:read(connection, PlayerId) of
        [Connection] ->
            NewConnection = Connection#connection{status = online},
            db:write(NewConnection);
        _ -> 
            nothing
    end,

    message:send_to_process(Pid, perception, Perception);

do_event(event, EventData, _Pid) ->
    EventData(),
    false; 

do_event(_Unknown, _Data, _Pid) ->
    lager:debug("Unknown event"),
    false.

process_explored([]) ->
    done;
process_explored([Player | Rest]) ->
    case player:is_player(Player) of
        true ->        
            lager:debug("Processed Explored: ~p", [Player]),
            [Conn] = db:dirty_read(connection, Player),
            ExploredTiles = map:get_explored(Player, new),
            message:send_to_process(Conn#connection.process, map, ExploredTiles);
        false ->
            nothing
    end,

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

process_dead_objs(NumTick) when (NumTick rem (?TICKS_MIN * 1)) =:= 0 ->
    game:process_dead_objs(NumTick);
process_dead_objs(_) ->
    nothing. 

process_deleting_objs(NumTick) when (NumTick rem (?TICKS_MIN * 1)) =:= 0 ->
    game:process_deleting_objs(NumTick);
process_deleting_objs(_) ->
    nothing.

villager_process(0) -> nothing;
villager_process(NumTick) ->
    villager:process(NumTick).

npc_process(0) -> nothing;
npc_process(NumTick) -> 
    npc:process(NumTick).

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
            case item:get_by_class(Unit#obj.id, ?FOOD) of
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
            Rand = util:rand(5 + Num);
            %npc_mgr:spawn_zombies(Rand);
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

process_effects(CurrentTick) ->
    %Process effects with intervals

    process_interval_effects(CurrentTick),
    process_expiry_effects(CurrentTick).

process_interval_effects(CurrentTick) ->
    IntervalEffects = db:dirty_index_read(effect, CurrentTick, #effect.next_tick),

    F = fun(IntervalEffect) ->
            interval_effect(IntervalEffect)            
        end,

    lists:foreach(F, IntervalEffects).

process_expiry_effects(CurrentTick) ->
    Effects = db:dirty_index_read(effect, CurrentTick, #effect.expiry),
    
    F = fun(Effect) ->
            lager:info("Effect ~p expiring", [Effect#effect.type]),
            expire_effect(Effect) 
        end,

    lists:foreach(F, Effects).

interval_effect(_) -> nothing.

expire_effect(#effect{id = Id, type = ?HOLY_LIGHT}) ->
    effect:remove(Id, ?HOLY_LIGHT);
expire_effect(_) ->
    lager:info("No matching expiry effect").

