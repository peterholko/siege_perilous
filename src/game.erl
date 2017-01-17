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
-export([add_event/5, has_pre_events/1, has_post_events/1, cancel_event/1]).
-export([trigger_perception/0, trigger_explored/1]).
-export([get_perception/0, get_explored/0, reset/0]).
-export([send_update_items/3, send_update_stats/2, send_revent/2]).
-export([get_info_tile/1, get_valid_tiles/1]).
-export([spawn_hero/1, hero_dead/2]).
-export([spawn_shadow/1, spawn_wolf/0]).


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

    Info0 = maps:put(<<"x">>, X , #{}),
    Info1 = maps:put(<<"y">>, Y , Info0),
    Info2 = maps:put(<<"name">>, TileName, Info1),
    Info3 = maps:put(<<"mc">>, MovementCost, Info2),
    Info4 = maps:put(<<"def">>, DefenseBonus, Info3),
    Info5 = maps:put(<<"passable">>, Passable, Info4),
    Info6 = maps:put(<<"wildness">>, WildnessLevel, Info5),

    Info6.

get_valid_tiles({X, Y}) ->
    Neighbours = map:neighbours(X, Y),

    F = fun(Pos) ->
        map:is_passable(Pos) and obj:is_empty(Pos)
    end,

    lists:filter(F, Neighbours).

spawn_hero(PlayerId) ->
    %Pos = map:random_location(),
    %AdjPos = map:get_random_neighbour(Pos),
    Pos = {17,35},
    AdjPos = {16,35},
    AdjPos2 = {18,35},
 
    HeroId = obj:create(Pos, PlayerId, unit, ?HERO, <<"Hero Mage">>, none),
    VillagerId = obj:create(AdjPos, PlayerId, unit, ?VILLAGER, <<"Human Villager">>, none),
    MonolithId = obj:create(AdjPos2, PlayerId, poi, ?MONOLITH, <<"Monolith">>, none),

    item:create(HeroId, <<"Crimson Root">>, 100),
    item:create(HeroId, <<"Cragroot Popular">>, 10),
    item:create(MonolithId, <<"Mana">>, 2500),
   
    % Equip food so it isn't dumped
    ItemMap = item:create(VillagerId, <<"Crimson Root">>, 100),
    ItemId = maps:get(<<"id">>, ItemMap),
    item:equip(ItemId),

    map:add_explored(PlayerId, Pos, 2).

hero_dead(PlayerId, HeroId) ->
    lager:info("Hero killed").

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
   
%%
%% API Functions
%%

start() ->
    gen_server:start({global, game_pid}, game, [], []).

add_event(Process, EventType, EventData, EventSource, EventTick) ->
    [{counter, tick, CurrentTick}] = db:dirty_read(counter, tick),

    Event = #event { id = counter:increment(event),
                     pid = Process,
                     type = EventType,
                     data = EventData,
                     source = EventSource,
                     tick = CurrentTick + EventTick,
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
            lager:debug("Cancel_event - Deleting event: ~p", [Event]),
            db:delete(event, Event#event.id);
        _ ->
            lager:debug("Cancel_event - none found from ~p", [EventSource]),
            nothing
    end.

trigger_perception() ->
    gen_server:cast({global, game_pid}, trigger_perception).

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

handle_cast(trigger_perception, #game{explored = Explored}) ->
    NewData = #game {perception = true,
                     explored = Explored},
    {noreply, NewData};

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

