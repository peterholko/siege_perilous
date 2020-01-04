%% Author: Peter
%% Created: Dec 04, 2014
%% Description: Game process to handle game state
-module(game).
-behaviour(gen_server).

%%
%% Include files
%%
-include("schema.hrl").
-include("common.hrl").
%%
%% Exported Functions
%%
-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add_obj_create/3, add_obj_delete/3, add_obj_update/4, add_obj_update/5, add_obj_move/5,
         add_obj_hide/3, add_obj_reveal/3]).
-export([add_event/5, has_pre_events/1, has_post_events/1, cancel_event/1]).
-export([process_dead_objs/1, process_deleting_objs/1, process_remove_player/1]).
-export([trigger_explored/1]).
-export([get_tick/0, get_perception/0, get_explored/0, reset/0]).
-export([get_info_tile/1, get_valid_tiles/2]).
-export([hero_dead/2]).
-export([spawn_shadow/1, spawn_wolf/0]).
-export([create_new_player/1, login/1]).
-export([send_update_items/3, 
         send_update_stats/2,
         send_update_experiment/2, 
         send_villager_change/1,
         send_tile_update/3,
         send_item_update/3,
         send_item_transfer/5,
         send_effect_change/3, send_effect_change/4,
         send_revent/2]).


%% Common functions
%%
send_update_items(ObjId, NewItems, PlayerPid) ->
    lager:debug("Send update items: ~p ~p ~p", [ObjId, NewItems]),
    [Obj] = db:read(obj, ObjId),
    case obj:is_hero_nearby(Obj, Obj#obj.player) of
        true ->
            %Send item perception to player pid
            message:send_to_process(PlayerPid, new_items, NewItems);
        false ->
            nothing
    end.

send_update_stats(PlayerId, ObjId) when PlayerId > ?NPC_ID ->
    lager:info("PlayerId: ~p ObjId ~p", [PlayerId, ObjId]),
    Stats = obj:get_stats(ObjId),
    [Conn] = db:read(connection, PlayerId),
    message:send_to_process(Conn#connection.process, stats, Stats);
send_update_stats(_, _) -> 
    nothing.

send_update_experiment(StructureId, InfoExperiment) ->
    [StructureObj] = db:read(obj, StructureId),

    Index = {obj:player(StructureObj), experiment, StructureId},

    case db:read(active_info, Index) =/= [] of
        true ->
            [Conn] = db:read(connection, obj:player(StructureObj)),
            message:send_to_process(Conn#connection.process, info_experiment, InfoExperiment);
        false ->
            nothing
    end.

send_villager_change(Villager) ->
    %Check if being villager is observed
    Index = {villager:player(Villager), obj, villager:id(Villager)},
    
    case db:read(active_info, Index) =/= [] of
        true -> 
            Message = #{<<"id">> => villager:id(Villager),
                        <<"order">> => villager:order(Villager),
                        <<"action">> => villager:action(Villager),
                        <<"morale">> => villager:morale(Villager),
                        <<"shelter">> => obj:get_name_by_id(villager:shelter(Villager)),
                        <<"structure">> => obj:get_name_by_id(villager:structure(Villager))},

            lager:info("Sending villager_change: ~p", [Message]),

            [Conn] = db:read(connection, villager:player(Villager)),
            message:send_to_process(Conn#connection.process, villager_change, Message);
        false ->
            nothing
    end.

send_tile_update(Pos, Attr, Value) ->    
    case db:index_read(active_info, Pos, #active_info.id) of
        [] -> nothing;
        ActiveInfoList ->
            {X, Y} = Pos,

            F = fun(ActiveInfo) ->
                    
                    Message = #{<<"x">> => X,
                                <<"y">> => Y,
                                <<"attr">> => Attr,
                                <<"value">> => Value},            
                    lager:info("Message: ~p", [Message]),
                    [Conn] = db:read(connection, ActiveInfo#active_info.player),
                    message:send_to_process(Conn#connection.process, 
                                            info_tile_update, 
                                            Message)                       
                end,
           
            lists:foreach(F, ActiveInfoList)
    end.

send_item_update(ObjId, Item, Merged) ->
    Obj = obj:get(ObjId),

    IndexObjItems = {obj:player(Obj), obj_items, ObjId},
    ActiveInfoObjCheck = db:read(active_info, IndexObjItems) =/= [],

    IndexItem = {obj:player(Obj), item, item:id(Item)},
    ActiveInfoItemCheck = db:read(active_info, IndexItem) =/= [],
    lager:info("Sending item update, ~p ~p", [ActiveInfoObjCheck, ActiveInfoItemCheck]),

    case ActiveInfoObjCheck or ActiveInfoItemCheck of
        true ->
            Message = #{<<"id">> => ObjId,
                        <<"item">> => Item,
                        <<"merged">> => Merged},
            lager:info("info_item_update: ~p", [Message]),
            [Conn] = db:read(connection, obj:player(Obj)),
            message:send_to_process(Conn#connection.process, info_item_update, Message);
        false ->
            nothing
    end.

send_item_transfer(SourceOwnerId, DestOwnerId, SourceItemId, Item, Merged) ->
    SourceOwnerObj = obj:get(SourceOwnerId),
    DestOwnerObj = obj:get(DestOwnerId),

    SourceIndex = {obj:player(SourceOwnerObj), obj, SourceOwnerId},
    DestIndex = {obj:player(DestOwnerObj), obj, DestOwnerId},

    Result = db:read(active_info, SourceIndex) =/= [] orelse db:read(active_info, DestIndex) =/= [],
    
    case Result of
        true ->
            Message = #{<<"sourceid">> => SourceOwnerId,
                        <<"destid">> => DestOwnerId,
                        <<"sourceitemid">> => SourceItemId,
                        <<"item">> => Item,
                        <<"merged">> => Merged},

            %TODO item transfer right now can only be between same player objs 
            [Conn] = db:read(connection, obj:player(DestOwnerObj)),
            message:send_to_process(Conn#connection.process, item_transfer_result, Message);
        false ->
            nothing
    end.

send_effect_change(ObjId, Effect, EffectOp) ->
    send_effect_change(ObjId, Effect, EffectOp, none).

send_effect_change({tile, _}, _, _, _) -> nothing; %TODO send only to observers of tile
send_effect_change(ObjId, Effect, EffectOp, EffectData) ->
    Obj = obj:get(ObjId),

    %TODO Expand to non-player owned objs
    case player:is_online(obj:player(Obj)) of
        false -> nothing;
        Conn ->
            EffectOpBin = atom_to_binary(EffectOp, latin1),

            Message = #{<<"id">> => obj:id(Obj),
                        <<"effect">> => Effect,
                        <<"op">> => EffectOpBin},

            FinalMessage = case EffectData of
                               none -> maps:put(<<"data">>, EffectData, Message);
                               _ -> Message
                           end,
                        
            message:send_to_process(Conn#connection.process, info_effect_update, FinalMessage)
    end.

send_revent(PlayerId, REvent) ->
    [Conn] = db:read(connection, PlayerId),
    message:send_to_process(Conn#connection.process, revent, REvent).

get_info_tile(Pos) ->
    {X, Y} = Pos,
    [Tile] = map:get_tile(Pos),

    TileName = map:tile_name(Tile#map.tile),
    MovementCost = map:movement_cost(TileName),
    DefenseBonus = map:defense_bonus(TileName),
    Passable = map:is_passable(TileName),
    WildnessLevel = encounter:get_wildness(Pos), 
    HasSanctuary = map:has_sanctuary(Pos),

    NumUnrevealed = resource:get_num_unrevealed(Pos),
    Resources = resource:survey(Pos),

    Info0 = maps:put(<<"x">>, X , #{}),
    Info1 = maps:put(<<"y">>, Y , Info0),
    Info2 = maps:put(<<"name">>, TileName, Info1),
    Info3 = maps:put(<<"mc">>, MovementCost, Info2),
    Info4 = maps:put(<<"def">>, DefenseBonus, Info3),
    Info4a = maps:put(<<"unrevealed">>, NumUnrevealed, Info4),
    Info5 = maps:put(<<"sanctuary">>, HasSanctuary, Info4a),
    Info6 = maps:put(<<"passable">>, Passable, Info5),
    Info7 = maps:put(<<"wildness">>, WildnessLevel, Info6),
    Info8 = maps:put(<<"resources">>, Resources, Info7),

    Info8.

get_valid_tiles({X, Y}, Obj) ->
    Neighbours = map:neighbours(X, Y),

    F = fun(Pos) ->
        map:is_passable(Pos, Obj) and obj:is_empty(Pos)
    end,

    lists:filter(F, Neighbours).

create_new_player(PlayerId) ->
    lager:info("Spawning new player: ~p", [PlayerId]),

    PlayerStartPosList = player:get_open_start_pos(),

    %TODO cancel creation if all start locations are full

    RandStartIndex = util:rand(length(PlayerStartPosList)),
    RandStartPos = lists:nth(RandStartIndex, PlayerStartPosList),
    
    StartPosName = list_to_binary("startpos" ++ integer_to_list(RandStartPos)),
    StartPosData = db:all(start, StartPosName),
    lager:info("StartPosData: ~p", [StartPosData]),
    
    HeroPos = util:pos_bin_to_tuple(maps:get(<<"hero_pos">>, StartPosData)),
    VillagerPos = util:pos_bin_to_tuple(maps:get(<<"villager_pos">>, StartPosData)),
    MonolithPos = util:pos_bin_to_tuple(maps:get(<<"monolith_pos">>, StartPosData)),
    ShipwreckPos = util:pos_bin_to_tuple(maps:get(<<"shipwreck_pos">>, StartPosData)),
    Corpse1Pos = util:pos_bin_to_tuple(maps:get(<<"corpse1_pos">>, StartPosData)),
    Corpse2Pos = util:pos_bin_to_tuple(maps:get(<<"corpse2_pos">>, StartPosData)),
    NecromancerPos = util:pos_bin_to_tuple(maps:get(<<"necromancer_pos">>, StartPosData)),

    [Player] = db:read(player, PlayerId),

    HeroId = obj:create(HeroPos, PlayerId, Player#player.class),   
    MonolithId = obj:create(MonolithPos, PlayerId, <<"Monolith">>),
    ShipwreckId = obj:create(ShipwreckPos, PlayerId, <<"Shipwreck">>),

    NewPlayer = Player#player {hero = HeroId,
                               start_pos = RandStartPos,
                               data = #{monolith_pos => MonolithPos}},
    db:write(NewPlayer),
    
    %Create 2 corpses
    obj:create(Corpse1Pos, ?UNDEAD, <<"Human Corpse">>, ?DEAD),
    obj:create(Corpse2Pos, ?UNDEAD, <<"Human Corpse">>, ?DEAD),

    VillagerId = villager:create(0, PlayerId, VillagerPos),
    %VillagerId2 = villager:create(0, PlayerId, Villager2Pos),

    item:create(HeroId, <<"Honeybell Berries">>, 25),
    item:create(HeroId, <<"Spring Water">>, 25),
    item:create(HeroId, <<"Gold Coins">>, 25),
    item:create(HeroId, <<"Valleyrun Copper Ingot">>, 200),
    item:create(HeroId, <<"Cragroot Maple Timber">>, 200),
    item:create(HeroId, <<"Copper Training Axe">>, 1),
    item:create(HeroId, <<"Health Potion">>, 5),
    item:create(MonolithId, <<"Mana">>, 2500),
    item:create(HeroId, <<"Cragroot Maple Wood">>, 100),
    item:create(ShipwreckId, <<"Cragroot Maple Timber">>, 25),
    item:create(ShipwreckId, <<"Valleyrun Copper Ore">>, 100),

    map:add_explored(PlayerId, HeroPos, 2),

    %Log player in
    game:add_event(self(), login, PlayerId, none, 2),
   
    % Equip food so it isn't dumped
    item:create(VillagerId, <<"Honeybell Berries">>, 50, <<"true">>),
    item:create(VillagerId, <<"Spring Water">>, 50, <<"true">>),
    item:create(VillagerId, <<"Pick Axe">>, 2, <<"true">>),

    %item:create(VillagerId2, <<"Honeybell Berries">>, 50, <<"true">>),

    % Recipe initial
    recipe:create(PlayerId, <<"Copper Training Axe">>),
    recipe:create(PlayerId, <<"Copper Training Sword">>),
    recipe:create(PlayerId, <<"Copper Training Club">>),
    recipe:create(PlayerId, <<"Training Pick Axe">>),
    recipe:create(PlayerId, <<"Training Bow">>),

    F1 = fun() ->
            MausoleumPos = {16,32},

            MausoleumId = obj:create(MausoleumPos, ?UNDEAD, <<"Mausoleum">>),
            NPCId = npc:create(NecromancerPos, ?UNDEAD, <<"Necromancer">>),
            GuardianId = npc:create({17,32}, ?GUARDIANS, <<"Wose">>, ?HIDING),

            GuardianData = #{guard_pos => MausoleumPos,
                             guarding_structure => MausoleumId,
                             guard_text => "You fool! Do you know what you have unleashed!?"},
            npc:set_order(GuardianId, guard_structure, GuardianData),

            item:create(MausoleumId, ?BONES, 10),

            NPCData = #{mausoleum => MausoleumId,
                        mausoleum_guard => GuardianId,
                        order_pos => {17,31}},

            npc:set_data(NPCId, NPCData),

            LinkedNPC = [MausoleumId, NPCId, GuardianId],

            NewPlayerData = maps:put(linkednpc, LinkedNPC, NewPlayer#player.data),
            NewPlayer2 = NewPlayer#player {data = NewPlayerData},
            db:write(NewPlayer2)
         end,

    F2 = fun() ->
            MeagerMerchantId = npc:create({0, 40}, ?EMPIRE, <<"Meager Merchant">>),
            obj:add_group(MeagerMerchantId, ?MERCHANT),
            ItemMap = item:create(MeagerMerchantId, <<"Pick Axe">>, 2),
            item_attr:set(item:id(ItemMap), <<"price">>, 5),


            VillagerForHireId = villager:create(0, ?EMPIRE, {-50, -50}),
            obj_attr:set(VillagerForHireId, <<"wage">>, 24),

            obj_attr:set(MeagerMerchantId, <<"hauling">>, [VillagerForHireId]),
            obj:update_state(VillagerForHireId, ?ABOARD)

         end,

    F3 = fun() ->
            TaxCollectorShipId = npc:create({0, 40}, ?EMPIRE, <<"Tax Ship">>),
            TaxCollectorId = npc:create({-100, -50}, ?EMPIRE, <<"Tax Collector">>),
            
            obj_attr:set(TaxCollectorShipId, <<"hauling">>, [TaxCollectorId]),
            obj:update_state(TaxCollectorId, ?ABOARD),

            Data1 = #{tax_collector => TaxCollectorId,
                      target_player => PlayerId},

            Data2 = #{tax_collector_ship => TaxCollectorShipId,
                      target_player => PlayerId},

            npc:set_data(TaxCollectorShipId, Data1),
            npc:set_data(TaxCollectorId, Data2),

            obj:add_group(TaxCollectorId, ?TAX_COLLECTOR),

            %Set player is_tax_collected to false TODO move to another module
            [EmpirePlayer] = db:read(player, ?EMPIRE),
            NewEmpirePlayerData1 = maps:put({PlayerId, is_tax_collected}, false, EmpirePlayer#player.data),
            NewEmpirePlayerData2 = maps:put({PlayerId, tax_amount_due}, 10, NewEmpirePlayerData1),
            NewEmpirePlayerData3 = maps:put({PlayerId, landing_pos}, RandStartPos, NewEmpirePlayerData2),
            NewEmpirePlayer = EmpirePlayer#player {data = NewEmpirePlayerData3},
            db:write(NewEmpirePlayer)
         end,

    F4 = fun() ->
            sound:talk(VillagerId, "The dead rise up!  We must flee!")
         end,

    game:add_event(none, event, F1, none, ?TICKS_SEC * 10),
    %game:add_event(none, event, F2, none, 15),
    %game:add_event(none, event, F3, none, 15),
    %game:add_event(none, event, F4, none, 40),

    lager:info("Game end.").

login(PlayerId) ->
    %Log player in
    game:add_event(self(), login, PlayerId, none, 2).

spawn_shadow(MonolithPos) ->
    ListOfPos = map:random_location_from(?UNDEAD, MonolithPos, 5),
    RandomIndex = util:rand(length(ListOfPos)),
    NPCPos = lists:nth(RandomIndex, ListOfPos),

    NPCId = npc:create(NPCPos, <<"Shadow">>),

    npc:set_order(NPCId, move_to_pos, MonolithPos).

spawn_wolf() ->
    NPCPos = map:random_location(),
    NPCId = npc:create(NPCPos, <<"Wolf">>),

    npc:set_order(NPCId, wander_flee, none).

hero_dead(PlayerId, HeroId) ->
    %TODO revise all hero death flow
    lager:info("Hero killed"),
    [Player] = db:read(player, PlayerId),
    Objs = db:index_read(obj, PlayerId, #obj.player),
    LinkedNPCs = maps:get(linkednpc, Player#player.data),

    F = fun(Obj) ->
            case obj:id(Obj) =/= HeroId of
                true -> obj:update_deleting(Obj);
                false -> nothing
            end
        end,

    lists:foreach(F, Objs),

    G = fun(NPCId) ->
            Minions = obj_attr:value(NPCId, <<"minions">>, []),

            H = fun(MinionId) ->
                    MinionObj = obj:get(MinionId),
                    obj:update_deleting(MinionObj)
                end,

            lists:foreach(H, Minions),

            NPCObj = obj:get(NPCId),
            obj:update_deleting(NPCObj)
        end,
    
    lists:foreach(G, LinkedNPCs),

    case db:read(connection, PlayerId) of
        [Conn] -> 
            message:send_to_process(Conn#connection.process, hero_dead, #{});
        _ -> 
            nothing
    end,

    game:add_event(self(), hero_dead, PlayerId, none, ?TICKS_MIN * 2).

process_remove_player(PlayerId) ->
    db:delete(player, PlayerId). 

process_dead_objs(CurrentTick) ->
    DeadObjs = db:index_read(obj, ?DEAD, #obj.state),
    lager:info("Dead Objs: ~p", [DeadObjs]),

    F = fun(Obj) ->
            case (CurrentTick - obj:modtick(Obj)) > (?TICKS_MIN * 1) of
                true ->
                    lager:info("Update deleting ~p", [Obj]),
                    obj:update_deleting(Obj);
                false ->
                    nothing
            end                
        end,

    lists:foreach(F, DeadObjs).

process_deleting_objs(CurrentTick) ->
    DeletingObjs = db:index_read(obj, ?DELETING, #obj.class),
    lager:info("Deleting Objs: ~p", [DeletingObjs]),
    
    F = fun(Obj) ->
             case (CurrentTick - obj:modtick(Obj)) > ?TICKS_MIN of
                true ->
                    obj:remove(obj:id(Obj));
                false ->
                    nothing
            end                
        end,

    lists:foreach(F, DeletingObjs).

get_tick() ->
    [{counter, tick, CurrentTick}] = db:dirty_read(counter, tick),
    CurrentTick.

%%
%% API Functions
%%

start() ->
    gen_server:start({global, game_pid}, game, [], []).

add_obj_create(Process, Obj, EventTick) ->
    [{counter, tick, CurrentTick}] = db:dirty_read(counter, tick),

    Data = Obj,

    ObjEvent = #obj_event{id = counter:increment(obj_event),
                          pid = Process,
                          event = obj_create,
                          data = Data,
                          tick = CurrentTick + event_ticks(EventTick)},

    db:write(ObjEvent).

add_obj_delete(Process, Obj, EventTick) ->
    [{counter, tick, CurrentTick}] = db:dirty_read(counter, tick),

    Data = Obj,

    ObjEvent = #obj_event{id = counter:increment(obj_event),
                          pid = Process,
                          event = obj_delete,
                          data = Data,
                          tick = CurrentTick + event_ticks(EventTick)},

    db:write(ObjEvent).

add_obj_update(Process, ObjId, Attr, Value) -> 
    add_obj_update(Process, ObjId, Attr, Value, 1).

add_obj_update(Process, ObjId, Attr, Value, EventTick) ->
    [{counter, tick, CurrentTick}] = db:dirty_read(counter, tick),
    lager:debug("add_obj_update - ~p ~p ~p ~p ~p", [ObjId, Attr, Value, EventTick, CurrentTick]),

    Data = {ObjId, Attr, Value},

    ObjEvent = #obj_event{id = counter:increment(obj_event),
                          pid = Process,
                          event = obj_update,
                          data = Data,
                          tick = CurrentTick + event_ticks(EventTick)},

    db:write(ObjEvent).

add_obj_move(Process, ObjId, SourcePos, DestPos, EventTick) ->
    [{counter, tick, CurrentTick}] = db:dirty_read(counter, tick),

    Data = {ObjId, SourcePos, DestPos},

    ObjEvent = #obj_event{id = counter:increment(obj_event),
                          pid = Process,
                          event = obj_move,
                          data = Data,
                          tick = CurrentTick + event_ticks(EventTick)},

    db:write(ObjEvent).

add_obj_hide(Process, ObjId, EventTick) ->
    [{counter, tick, CurrentTick}] = db:dirty_read(counter, tick),

    ObjEvent = #obj_event{id = counter:increment(obj_event),
                          pid = Process,
                          event = obj_hide,
                          data = ObjId,
                          tick = CurrentTick + event_ticks(EventTick)},

    db:write(ObjEvent).

add_obj_reveal(Process, ObjId, EventTick) ->
    [{counter, tick, CurrentTick}] = db:dirty_read(counter, tick),

    ObjEvent = #obj_event{id = counter:increment(obj_event),
                          pid = Process,
                          event = obj_reveal,
                          data = ObjId,
                          tick = CurrentTick + event_ticks(EventTick)},

    db:write(ObjEvent).

add_event(Process, EventType, EventData, EventSource, EventTick) ->
    [{counter, tick, CurrentTick}] = db:dirty_read(counter, tick),

    Event = #event { id = counter:increment(event),
                     pid = Process,
                     type = EventType,
                     data = EventData,
                     source = EventSource,
                     tick = CurrentTick + event_ticks(EventTick),
                     class = event_class(EventType)},

    db:write(Event).

has_pre_events(EventSource) ->
    Events = db:index_read(event, EventSource, #event.source),
    
    F = fun(Event) ->
            Event#event.class =:= pre
        end,

    lists:any(F, Events).

has_post_events(EventSource) ->
    Events = db:index_read(event, EventSource, #event.source),
    
    F = fun(Event) ->
            Event#event.class =:= pos
        end,

    lists:any(F, Events).

cancel_event(EventSource) ->
    case db:index_read(event, EventSource, #event.source) of
        [Event] ->
            process_cancel(Event#event.type, Event),

            lager:debug("Cancel_event - Deleting event: ~p", [Event]),
            db:delete(event, Event#event.id),

            message:send_to_process(Event#event.pid, event_cancel, {Event#event.type, Event#event.data});
        _ ->
            lager:debug("Cancel_event - none found from ~p", [EventSource]),
            nothing
    end.

trigger_explored(Player) ->
    gen_server:cast({global, game_pid}, {trigger_explored, Player}).

get_perception() ->
    gen_server:call({global, game_pid}, get_perception).

get_explored() ->
    gen_server:call({global, game_pid}, get_explored).

reset() ->
    gen_server:cast({global, game_pid}, reset_perception).

%% ====================================================================
%% %% Server functions
%% ====================================================================


init([]) -> 
    Perception = false,
    Explored = [],

    Data = #game {perception = Perception,
                  explored = Explored},


    {ok, Data}.

terminate(_Reason, _) ->
    ok.

handle_cast({trigger_explored, Player}, #game{perception = Perception,
                                              explored = Explored} ) ->
    NewExplored = [Player | Explored],
    NewData = #game {perception = Perception,
                     explored = NewExplored},
    {noreply, NewData};

handle_cast(reset_perception, _Data) ->
    NewData = #game{perception = false,
                    explored = []},
    {noreply, NewData};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call(get_perception, _From, Data = #game{perception = Perception,
                                                explored = _Explored}) ->
    {reply, Perception, Data};

handle_call(get_explored, _From, Data = #game{perception = _Perception,
                                              explored = Explored}) ->
    {reply, Explored, Data};

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Event}, 
                              {from, From}
                             ]),
    {noreply, Data}.

handle_info({'EXIT', _Pid, _Reason}, Data) ->
    %% child exit?
    {noreply, Data};

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

%%
%% Local Functions
%%

event_class(attack) -> pre;
event_class(defend) -> pre;
event_class(move) -> pre;
event_class(ford) -> pre;
event_class(_) -> post.

event_ticks(Ticks) ->
    case ?FAST_EVENTS of
        true -> 1;
        false -> Ticks
    end.

process_cancel(build, Event) ->
    EventData = Event#event.data,
    {_BuilderId, StructureId} = EventData,

    BuildTime = obj_attr:value(StructureId, <<"build_time">>),
    EndTime = obj_attr:value(StructureId, <<"end_time">>),
    CurrentTime = game:get_tick(),
    lager:info("BuildTime: ~p", [BuildTime]),
    lager:info("EndTime: ~p", [EndTime]),
    lager:info("CurrentTime: ~p", [CurrentTime]),


    Progress = util:round3(1 - ((EndTime - CurrentTime) / BuildTime)),
    lager:info("Cancelled Event - progress: ~p", [Progress]),

    obj_attr:set(StructureId, <<"progress">>, Progress),

    game:add_obj_update(self(), StructureId, ?STATE, ?STALLED, 1);

process_cancel(_, _Event) -> nothing.