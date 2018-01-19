%% -------------------------------------------------------------------
%% Author  : Peter Holko
%%% Description : Calculates perception data
%%%
%%% Created : Dec 15, 2014
%%% -------------------------------------------------------------------
-module(perception).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/1, update/2, remove/2, recalculate/1, calculate_player/1, calculate_entity/1, broadcast/3, broadcast/4]).
-export([check_event_visible/2, process_observed_events/1, send_observed_changes/1, is_visible/2]).
-export([get_by_player/1]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, perception_pid}, perception, [], []).

create(Obj) ->
    gen_server:cast({global, perception_pid}, {create, Obj}).

recalculate(Obj) ->
    gen_server:cast({global, perception_pid}, {recalculate, Obj}).

update(ObserverObj, UpdatedObj) ->
    gen_server:cast({global, perception_pid}, {update, ObserverObj, UpdatedObj}).

remove(ObserverObj, RemovedObj) ->
    gen_server:cast({global, perception_pid}, {remove, ObserverObj, RemovedObj}).

broadcast(SourcePos, Range, MessageData) ->
    gen_server:cast({global, perception_pid}, {broadcast, SourcePos, Range, MessageData}).

broadcast(SourcePos, TargetPos, Range, MessageData) ->
    gen_server:cast({global, perception_pid}, {broadcast, SourcePos, TargetPos, Range, MessageData}).

get_by_player(PlayerId) ->
    AllPerception = db:index_read(perception, PlayerId, #perception.player),

    F = fun(Perception, All) ->
            maps:keys(Perception#perception.data) ++ All
        end,

    ObjList = util:unique_list(lists:foldl(F, [], AllPerception)),

    G = fun(ObjId, AllObjs) ->
            Obj = obj:get(ObjId),
            ObjMap = obj:rec_to_map(Obj),

            [ObjMap | AllObjs]
        end,


    lists:foldl(G, [], ObjList).

process_observed_events(AllEvents) ->
    
    EventsByPlayerMap = events_by_player(AllEvents),   
    EventsByPlayerList = maps:to_list(EventsByPlayerMap),

    F = fun(EventsByPlayer) ->
            {PlayerId, Events} = EventsByPlayer,
            
            case player:is_online(PlayerId) of
                false -> nothing;
                Conn ->
                    UniqueEvents = util:unique_list(Events),
                    lager:info("Sending list of events: ~p", [UniqueEvents]),
                    ConvertedEvents = convert_events(UniqueEvents),
                    lager:info("Converted Events: ~p", [ConvertedEvents]),

                    send_events(Conn, ConvertedEvents)
            end
        end,

    lists:foreach(F, EventsByPlayerList).
  
events_by_player(AllEvents) ->
    DefaultVault = [],

    F = fun({Observer, Event}, EventsByPlayer) ->
            PlayerEvents = maps:get(obj:player(Observer), EventsByPlayer, DefaultVault),
            NewPlayerEvents = [Event | PlayerEvents],

            maps:put(obj:player(Observer), NewPlayerEvents, EventsByPlayer)
        end,

    lists:foldl(F, #{}, AllEvents).

send_events(Conn, Events) ->
    Packet = #{<<"packet">> => <<"changes">>,
               <<"events">> => Events},

    Conn#connection.process ! {changes, Packet}.

convert_events(Events) ->

    F = fun(Event, AccEvent) ->
            [convert_event(Event) | AccEvent]
        end,

    lists:foldl(F, [], Events).

convert_event(#obj_create{obj = Obj}) ->
    ObjMap = obj:rec_to_map(Obj),
    #{<<"event">> => <<"obj_create">>,
      <<"obj">> => ObjMap};
convert_event(#obj_update{obj = Obj, attr = Attr, value = Value}) ->
    #{<<"event">> => <<"obj_update">>,
      <<"obj_id">> => obj:id(Obj),
      <<"attr">> => Attr,
      <<"value">> => Value};
convert_event(#obj_move{obj = Obj, source_pos = {X, Y}}) ->
    ObjMap = obj:rec_to_map(Obj),
    #{<<"event">> => <<"obj_move">>,
      <<"obj">> => ObjMap,
      <<"src_x">> => X,
      <<"src_y">> => Y}.

send_observed_changes(ChangesMap) ->
    ChangesList = maps:to_list(ChangesMap),

    F = fun(Change) ->                
            lager:debug("Change: ~p", [Change]),
            {PlayerId, {AddedList, RemovedList, UpdatedList}} = Change,
            
            %Check if added exists in the update list, meaning it isn't a newly visible object
            ProcessedAddedList = check_added(AddedList, UpdatedList),

            AddedMapList = added_to_map(ProcessedAddedList),
            UpdatedMapList = updated_to_map(UpdatedList),

            Changes = #{<<"packet">> => <<"changes">>,
                        <<"added">> => AddedMapList,
                        <<"removed">> => RemovedList,
                        <<"updated">> => UpdatedMapList},
      
            lager:debug("~p", [Changes]),

            case player:is_online(PlayerId) of
                false -> nothing;
                Conn ->
                    Conn#connection.process ! {changes, Changes}
            end
        end,

    lists:foreach(F, ChangesList).

check_added(AddedList, UpdatedList) ->
    lager:debug("Check added: ~p ~p", [AddedList, UpdatedList]),
    F = fun(Added) -> not lists:keymember(Added, 1, UpdatedList) end,

    lists:filter(F, AddedList).

added_to_map(ObjList) ->

    F = fun(ObjId, AllObjs) ->
            Obj = obj:get(ObjId),
            ObjMap = obj:rec_to_map(Obj),

            [ObjMap | AllObjs]
        end,

    lists:foldl(F, [], ObjList).

updated_to_map(Updated) -> 

    F = fun({ObjIdBin, Attr}, UpdatedMapList) ->
            Obj = obj:get(ObjIdBin),
            ObjMap = obj:rec_to_map(Obj),

            AttrValue = maps:get(Attr, ObjMap),

            AttrsMap = #{<<"id">> => ObjIdBin},
            NewAttrsMap = maps:put(Attr, AttrValue, AttrsMap),

            [NewAttrsMap | UpdatedMapList]
        end,

    lists:foldl(F, [], Updated).

check_event_visible(Event, Observers) ->
    F = fun(Observer, AllEvents) ->
            NewAllEvents = add_observed_event(Observer, Event, AllEvents),
            NewAllEvents
        end,

    lists:foldl(F, [], Observers).

add_observed_event(Observer, Event = #obj_create{obj = Obj, source_pos = SourcePos}, AllEvents) ->
    case check_distance(Observer, SourcePos) of
        true ->
            %Update observer's perception of obj after create event
            perception:update(Observer, Obj), 

            NewPerceptionEvent = {Observer, Event},

            [NewPerceptionEvent | AllEvents];
        false ->
            AllEvents
    end;
add_observed_event(Observer, Event = #obj_update{obj = Obj, source_pos = SourcePos}, AllEvents) ->
    case check_distance(Observer, SourcePos) of
        true ->
            %Update observer's perception of obj after update event
            perception:update(Observer, Obj), 

            NewPerceptionEvent = {Observer, Event},

            [NewPerceptionEvent | AllEvents];
        false ->
            AllEvents
    end;
add_observed_event(Observer, ObjMove = #obj_move{obj = Obj, source_pos = SourcePos, 
                                                 dest_pos = DestPos}, AllEvents) ->
    Result = {check_distance(Observer, SourcePos),
              check_distance(Observer, DestPos)},

    case Result of
        {true, true} -> % Moving obj is completely in observer's LOS

            %Update observer's perception of obj after move event
            perception:update(Observer, Obj),

            NewPerceptionEvent = {Observer, ObjMove},

            [NewPerceptionEvent | AllEvents];
        {true, false} -> % Moving obj has moved out of observer's LOS 

            %Remove obj from observer's perception 
            perception:remove(Observer, Obj),

            NewPerceptionEvent = {Observer, ObjMove},

            [NewPerceptionEvent | AllEvents];

        {false, true} -> % Moving obj has moved into observer's LOS

            %Add obj to observer's perception
            perception:update(Observer, Obj),

            NewPerceptionEvent = {Observer, ObjMove},

            [NewPerceptionEvent | AllEvents];
        {false, false} -> % Move event dot visible to observer
            AllEvents
            
    end.

check_distance(Observer, Pos) ->
    lager:debug("check_distance observer: ~p pos: ~p", [Observer, Pos]),
    Distance = map:distance(Observer#obj.pos, Pos),
    Distance =< Observer#obj.vision.

calculate_player(Player) ->
    AllObj = ets:tab2list(obj),
    PlayerEntities = player_entities(AllObj, Player),

    F = fun(Entity, AllPerception) -> 
            Perception = visible_objs(AllObj, Entity),
            
            maps:merge(Perception, AllPerception)
        end,

    AllPerceptionMap = lists:foldl(F, #{}, PlayerEntities),
    AllPerceptionList = maps:to_list(AllPerceptionMap),

    G = fun({_ObjIdBin, Obj}, AllObjs) ->            
            ObjMap = obj:rec_to_map(Obj),
            [ ObjMap | AllObjs]
        end,

    PacketPerception = lists:foldl(G, [], AllPerceptionList),
    PacketPerception.

calculate_entity(Entity) ->
    AllObj = ets:tab2list(obj),

    Perception = visible_objs(AllObj, Entity),
    Perception.

is_visible(SourceId, TargetId) ->
    [Perception] = db:read(perception, SourceId),

    F = fun(MapObj) ->
           MapObjId = maps:get(<<"id">>, MapObj),
           TargetId =:= MapObjId
        end,

    lists:any(F, Perception#perception.data).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast({create, Obj}, Data) ->   
    PerceptionData = calculate_entity(Obj),

    Perception = #perception{entity = obj:id(Obj),
                             player = obj:player(Obj),
                             data = PerceptionData},

    db:write(Perception),
    lager:info("Inserted new perception: ~p", [Perception]),

    {noreply, Data};

handle_cast({update, ObserverObj, UpdatedObj}, Data) ->  
    [Perception] = db:read(perception, obj:id(ObserverObj)),

    PrevDiff = Perception#perception.diff,
    
    NewPerceptionData = maps:put(obj:id(UpdatedObj), UpdatedObj, Perception#perception.data),
    NewPerception = Perception#perception{data = NewPerceptionData,
                                          diff = PrevDiff ++ [obj:id(UpdatedObj)]},

    db:write(NewPerception),

    {noreply, Data};

handle_cast({remove, ObserverObj, RemovedObj}, Data) ->   
    [Perception] = db:read(perception, obj:id(ObserverObj)),
     
    PrevDiff = Perception#perception.diff,

    NewPerceptionData = maps:remove(obj:id(RemovedObj), Perception#perception.data),
    NewPerception = Perception#perception{data = NewPerceptionData,
                                          diff = PrevDiff ++ [obj:id(RemovedObj) * -1]},

    db:write(NewPerception),

    {noreply, Data};

handle_cast({recalculate, ObserverObj}, Data) ->
    [Perception] = db:read(perception, obj:id(ObserverObj)),

    OldPerceptionData = Perception#perception.data,
    NewPerceptionData = calculate_entity(ObserverObj),

    PrevDiff = Perception#perception.diff,

    DiffObjs = maps:keys(OldPerceptionData) -- maps:keys(NewPerceptionData),

    NewPerception = Perception#perception{data = NewPerceptionData,
                                          diff = PrevDiff ++ DiffObjs},

    db:write(NewPerception),
    {noreply, Data};

handle_cast({broadcast, SourcePos, Range, MessageData}, Data) ->
    VisionObjs = get_nearby_objs(SourcePos, Range),
    lager:info("VisionObjs: ~p", [VisionObjs]),
    broadcast_to_objs(VisionObjs, MessageData),

    {noreply, Data};    

handle_cast({broadcast, SourcePos, TargetPos, Range, MessageData}, Data) ->
    SourceObjs = get_nearby_objs(SourcePos, Range),
    TargetObjs = get_nearby_objs(TargetPos, Range),
    VisionObjs = util:unique_list(SourceObjs ++ TargetObjs),

    lager:info("VisionObjs: ~p", [VisionObjs]),
    broadcast_to_objs(VisionObjs, MessageData),

    {noreply, Data};    

handle_cast(stop, Data) ->
    {stop, normal, Data}.

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


player_entities(AllObj, Player) ->
    F = fun(Obj) -> (Obj#obj.vision > 0) and (Obj#obj.player =:= Player) end,
    lists:filter(F, AllObj).

do_recalculate() ->
    lager:debug("Perception recalculate"),
    %Erase process dict
    erase(),

    %Get all entities
    AllObj = ets:tab2list(obj),
    Entities = filter_objs(AllObj),

    lager:debug("Calculate perceptions ~p", [Entities]),
    %Calculate each player entity perception
    entity_perception(Entities, AllObj).

filter_objs(AllObj) ->
    F = fun(Obj) -> Obj#obj.vision > 0 end,
    lists:filter(F, AllObj).

entity_perception([], _AllObj) ->
    done;

entity_perception([Entity | Rest], AllObj) ->
    lager:debug("Entity perception: ~p", [Entity]),    
    NearbyObjs = visible_objs(AllObj, Entity),
    lager:debug("NearbyObjs: ~p", [NearbyObjs]), 

    %Compare previous perception to new perception
    PreviousObjs = db:dirty_read(perception, Entity#obj.id),

    %Compare old perception to new
    Result = compare_perception(NearbyObjs, PreviousObjs),

    store_perception(Result, Entity, NearbyObjs),

    %Check if new perception must be sent
    send_perception(Result, Entity, NearbyObjs),

    entity_perception(Rest, AllObj).

compare_perception(_New, []) -> false;
compare_perception(New, [Old]) -> New =:= Old#perception.data.

store_perception(false, Entity, NewPerception) ->
    db:dirty_write(#perception {entity=Entity#obj.id, player=Entity#obj.player, data=NewPerception});
store_perception(_Result, _EntityId, _NewPerception) ->
    nothing.

send_perception(true, _, _) -> nothing;
send_perception(false, Entity, NewPerception) ->
    case Entity#obj.subclass of
        ?VILLAGER -> 
            Process = global:whereis_name(villager),
            send_to_process(Process, {Entity#obj.id, NewPerception});
        _ -> 
            nothing
    end,

    [Conn] = db:read(connection, Entity#obj.player),
    send_to_process(Conn#connection.process, {Entity#obj.id, NewPerception}).

send_to_process(Process, NewPerception) when is_pid(Process) ->
    lager:debug("Sending ~p to ~p", [NewPerception, Process]),
    Process ! {perception, NewPerception};
send_to_process(_Process, _NewPerception) ->
    none.

broadcast_to_objs(Objs, Message) ->
    lager:info("broadcast Message: ~p", [Message]),
    F = fun(Obj) ->
            case Obj#obj.player > ?NPC_ID of
                true ->
                    NewMessage = maps:put(<<"witnessid">>, Obj#obj.id, Message),
                    case Obj#obj.subclass of
                        ?VILLAGER -> 
                            Process = global:whereis_name(villager),
                            Process ! {broadcast, NewMessage};
                        ?HERO -> 
                            [Conn] = db:read(connection, Obj#obj.player),
                            lager:info("Broadcasting message: ~p", [NewMessage]),
                            Conn#connection.process ! {broadcast, NewMessage};
                        _ -> 
                            %No other subclasses require broadcasts
                            nothing
                    end;
                false ->
                    %NPC don't need broadcast at this time
                    nothing
            end
        end,
    
    lists:foreach(F, Objs).    

% Undead units cannot see objects with SANCTUARY
%visible_objs(AllObjs, #obj {pos = Pos, player = Player, vision = Vision}) when Player =:= ?UNDEAD ->
%    F = fun(Target, PerceptionData) ->
%            Result = Target#obj.state =/= hiding andalso
%                     map:distance(Pos, Target#obj.pos) =< Vision andalso
%                     not effect:has_effect(Target#obj.id, ?SANCTUARY),            
%            case Result of
%                true ->
%                    maps:put(Target#obj.id, Target, PerceptionData);
%                false -> 
%                    PerceptionData
%            end
%        end,
%
%    lists:foldl(F, [], AllObjs);

% Animal units cannot see objects with FORTIFIED
%visible_objs(AllObjs, #obj {pos = Pos, player = Player, vision = Vision}) when Player =:= ?ANIMAL ->
%    F = fun(Target, Visible) ->
%            Result = Target#obj.state =/= hiding andalso
%                     map:distance(Pos, Target#obj.pos) =< Vision andalso
%                     not effect:has_effect(Target#obj.id, ?FORTIFIED),
%            
%            case Result of
%                true -> [build_message(Target) | Visible];
%                false -> Visible
%            end
%        end,

 
% lists:foldl(F, [], AllObjs);


visible_objs(AllObjs, #obj {id = Id, pos = Pos, vision = Vision}) ->
    F = fun(Target, PerceptionData) when Target#obj.id =:= Id ->
                maps:put(Target#obj.id, Target, PerceptionData); %Include self
           (Target, PerceptionData) ->
            Result = Target#obj.state =/= hiding andalso
                     map:distance(Pos, Target#obj.pos) =< Vision,

            case Result of
                true -> 
                    maps:put(Target#obj.id, Target, PerceptionData);
                false -> 
                    PerceptionData
            end
        end,

    lists:foldl(F, #{}, AllObjs).


get_nearby_objs(SourcePos, Range) ->
    AllObj = ets:tab2list(obj),

    F = fun(Obj) ->
            case Obj#obj.vision > 0 of
                true -> map:distance(SourcePos, Obj#obj.pos) =< Range;
                false -> false                
            end
        end,

    lists:filter(F, AllObj).


get_observers(PlayerId) ->
    Objs = db:dirty_index_read(obj, PlayerId, #obj.player),

    F = fun(Obj) -> Obj#obj.vision =:= 0 end,
    lists:filter(F, Objs).
