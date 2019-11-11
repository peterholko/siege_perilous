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
-export([create/1, get_entity/1, get_player/1, get_diff/1, update/2, remove/2, recalculate/1, clear_diff/1,
         calculate_player/1, calculate_entity/1, broadcast/3, broadcast/4]).
-export([check_event_visible/2, process_observed_events/1, is_visible/2]).
-export([get_by_player/1]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, perception_pid}, perception, [], []).

create(Obj) ->
    gen_server:cast({global, perception_pid}, {create, Obj}).

get_entity(Obj) ->
    gen_server:call({global, perception_pid}, {get_entity, Obj}).

get_player(PlayerId) ->
    gen_server:call({global, perception_pid}, {get_player, PlayerId}).

get_diff(Obj) ->
    gen_server:call({global, perception_pid}, {get_diff, Obj}).

recalculate(Obj) ->
    gen_server:cast({global, perception_pid}, {recalculate, Obj}).

update(ObserverObj, UpdatedObj) ->
    gen_server:cast({global, perception_pid}, {update, ObserverObj, UpdatedObj}).

remove(ObserverObj, RemovedObj) ->
    gen_server:cast({global, perception_pid}, {remove, ObserverObj, RemovedObj}).

clear_diff(Obj) ->
    gen_server:cast({global, perception_pid}, {clear_diff, Obj}).

broadcast(SourcePos, Range, MessageData) ->
    gen_server:cast({global, perception_pid}, {broadcast, SourcePos, Range, MessageData}).

broadcast(SourcePos, TargetPos, Range, MessageData) ->
    gen_server:cast({global, perception_pid}, {broadcast, SourcePos, TargetPos, Range, MessageData}).

get_by_player(PlayerId) ->
    ObjList = perception:get_player(PlayerId),

    F = fun(ObjId, AllObjs) ->
            Obj = obj:get(ObjId),
            ObjMap = obj:rec_to_map(Obj),

            [ObjMap | AllObjs]
        end,


    lists:foldl(F, [], ObjList).

process_observed_events(AllEvents) ->
    
    EventsByPlayerMap = events_by_player(AllEvents),   
    EventsByPlayerList = maps:to_list(EventsByPlayerMap),

    F = fun(EventsByPlayer) ->
            {PlayerId, Events} = EventsByPlayer,
            
            case player:is_online(PlayerId) of
                false -> nothing;
                Conn ->
                    UniqueEvents = util:unique_list(Events),
                    ConvertedEvents = convert_events(UniqueEvents),

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
convert_event(#obj_reveal{obj = Obj}) ->
    ObjMap = obj:rec_to_map(Obj),
    #{<<"event">> => <<"obj_create">>,
      <<"obj">> => ObjMap};
convert_event(#obj_delete{obj = Obj}) ->
    #{<<"event">> => <<"obj_delete">>,
      <<"obj_id">> => obj:id(Obj)};
convert_event(#obj_hide{obj = Obj}) ->
    #{<<"event">> => <<"obj_delete">>,
      <<"obj_id">> => obj:id(Obj)};
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

check_event_visible(Event, Observers) ->
    F = fun(Observer, AllEvents) ->
            
            NewAllEvents = add_observed_event(Observer, Event, AllEvents),
            NewAllEvents
        end,

    lists:foldl(F, [], Observers).

add_observed_event(Observer = #obj{id = ObserverId}, 
                   Event = #obj_move{obj = #obj{id = ObjId}}, 
                   AllEvents) when ObserverId =:= ObjId ->
   
    PlayerPerception = perception:get_player(obj:player(Observer)), 

    perception:recalculate(ObserverId),

    EntityDiff = perception:get_diff(Observer),

    F = fun(DiffObjId, Acc) ->
            case lists:member(DiffObjId, PlayerPerception) of
                false ->
                    DiffObj = obj:get(DiffObjId),
                    NewObj = #obj_create{obj = DiffObj,
                                         source_pos = {1,1}},
                    [{Observer, NewObj} | Acc];
                true ->
                    Acc
            end
        end,

    DiffEvents = lists:foldl(F, [], EntityDiff),
    
    perception:clear_diff(Observer), 

    NewEvents = [{Observer, Event} | AllEvents],

    NewEvents ++ DiffEvents;
add_observed_event(Observer, Event = #obj_create{obj = Obj, source_pos = SourcePos}, AllEvents) ->
    Result = {is_visible_by_observer(Observer, Obj),
              check_distance(Observer, SourcePos)},

    case Result of
        {_, false} ->
            AllEvents; %Obj create is not seen by observer
        {false, _} ->
            AllEvents; %Obj state or effect is not visible by observer
        {true, true} ->
            %Update observer's perception of obj after create event
            perception:update(Observer, Obj), 

            NewPerceptionEvent = {Observer, Event},

            [NewPerceptionEvent | AllEvents]
    end;
add_observed_event(Observer, Event = #obj_reveal{obj = Obj}, AllEvents) ->
    % Is visible by observer is not needed as the object initiated the hide
    
    case check_distance(Observer, obj:pos(Obj)) of
        true ->
            perception:update(Observer, Obj),

            NewPerceptionEvent = {Observer, Event},

            [NewPerceptionEvent | AllEvents];
        false ->
            AllEvents
    end;

add_observed_event(Observer, Event = #obj_delete{obj = Obj, source_pos = SourcePos}, AllEvents) ->
    Result = {is_visible_by_observer(Observer, Obj),
              check_distance(Observer, SourcePos)},
    lager:info("ObjDelete: ~p", [Result]),
    case Result of
        {_, false} ->
            AllEvents; %Obj deleted is not seen by observer
        {false, _} ->
            AllEvents; %Obj state or effect is not visible by observer
        {true, true} ->
            %Update observer's perception of obj after delete event
            perception:remove(Observer, Obj), 

            NewPerceptionEvent = {Observer, Event},

            [NewPerceptionEvent | AllEvents]
    end;

add_observed_event(Observer, Event = #obj_hide{obj = Obj}, AllEvents) ->
    % Is visible by observer is not needed as the object initiated the hide
    
    case check_distance(Observer, obj:pos(Obj)) of
        true ->
            perception:remove(Observer, Obj),

            NewPerceptionEvent = {Observer, Event},

            [NewPerceptionEvent | AllEvents];
        false ->
            AllEvents
    end;

add_observed_event(Observer, Event = #obj_update{obj = Obj, source_pos = SourcePos}, AllEvents) ->
    Result = {is_visible_by_observer(Observer, Obj),
              check_distance(Observer, SourcePos)},

    case Result of
        {_, false} ->
            AllEvents; %Obj update is not seen by observer
        {false, _} ->
            %Remove obj from observer's perception 
            perception:remove(Observer, Obj),

            NewPerceptionEvent = {Observer, Event},

            [NewPerceptionEvent | AllEvents];
        {true, true} ->
            %Update observer's perception of obj after update event
            perception:update(Observer, Obj), 

            NewPerceptionEvent = {Observer, Event},

            [NewPerceptionEvent | AllEvents]
    end;
add_observed_event(Observer, ObjMove = #obj_move{obj = Obj, source_pos = SourcePos, 
                                                 dest_pos = DestPos}, AllEvents) ->
    Result = {is_visible_by_observer(Observer, Obj),
              check_distance(Observer, SourcePos),
              check_distance(Observer, DestPos)},

    case Result of
        {_, false, false} -> % If moving obj is not within distance, event not seen
            AllEvents;
        {false, _, _} -> % Moving obj is no longer visible due to an effect or state 
            %Remove obj from observer's perception 
            perception:remove(Observer, Obj),

            NewPerceptionEvent = {Observer, ObjMove},

            [NewPerceptionEvent | AllEvents];
        {true, true, true} -> % Moving obj is completely in observer's LOS

            %Update observer's perception of obj after move event
            perception:update(Observer, Obj),

            NewPerceptionEvent = {Observer, ObjMove},

            [NewPerceptionEvent | AllEvents];
        {true, true, false} -> % Moving obj has moved out of observer's LOS 
            %Remove obj from observer's perception 
            perception:remove(Observer, Obj),

            NewPerceptionEvent = {Observer, ObjMove},

            [NewPerceptionEvent | AllEvents];

        {true, false, true} -> % Moving obj has moved into observer's LOS

            %Add obj to observer's perception
            perception:update(Observer, Obj),

            NewPerceptionEvent = {Observer, ObjMove},

            [NewPerceptionEvent | AllEvents]
    end.

check_distance(Observer, Pos) ->
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

is_visible_by_observer(_Obs = #obj{player = Player}, Obj) when Player =:= ?UNDEAD ->
    not effect:has_effect(obj:id(Obj), ?SANCTUARY);
is_visible_by_observer(_Obs = #obj{player = Player}, Obj) when Player =:= ?ANIMAL ->
    not effect:has_effect(obj:id(Obj), ?FORTIFIED);
is_visible_by_observer(_, _) ->
    true.

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast({create, Obj}, Data) ->   
    PerceptionData = maps:keys(calculate_entity(Obj)),

    Perception = #perception{entity = obj:id(Obj),
                             player = obj:player(Obj),
                             data = PerceptionData},

    db:write(Perception),

    {noreply, Data};

handle_cast({update, ObserverObj, UpdatedObj}, Data) ->  
    [Perception] = db:read(perception, obj:id(ObserverObj)),

    IsMember = lists:member(obj:id(UpdatedObj), Perception#perception.data),

    NewPerceptionData = case IsMember of
                            true -> 
                                Perception#perception.data;
                            false ->
                                [obj:id(UpdatedObj) | Perception#perception.data]
                        end,

    NewPerception = Perception#perception{data = NewPerceptionData},
    db:write(NewPerception),

    {noreply, Data};

handle_cast({remove, ObserverObj, RemovedObj}, Data) ->   
    [Perception] = db:read(perception, obj:id(ObserverObj)),
    
    NewPerceptionData = lists:delete(obj:id(RemovedObj), Perception#perception.data), 
    NewPerception = Perception#perception{data = NewPerceptionData},

    db:write(NewPerception),
    
    {noreply, Data};

handle_cast({recalculate, ObserverId}, Data) ->
    ObserverObj = obj:get(ObserverId),
    [Perception] = db:read(perception, obj:id(ObserverObj)),

    OldPerceptionData = Perception#perception.data,
    NewPerceptionData = maps:keys(calculate_entity(ObserverObj)),

    PrevDiff = Perception#perception.diff,

    DiffObjs = NewPerceptionData -- OldPerceptionData,

    NewPerception = Perception#perception{data = NewPerceptionData,
                                          diff = PrevDiff ++ DiffObjs},

    db:write(NewPerception),


    {noreply, Data};

handle_cast({clear_diff, ObserverObj}, Data) ->
    [Perception] = db:read(perception, obj:id(ObserverObj)),

    NewPerception = Perception#perception {diff = []},

    db:write(NewPerception),
    {noreply, Data};

handle_cast({broadcast, SourcePos, Range, MessageData}, Data) ->
    VisionObjs = get_nearby_objs(SourcePos, Range),
    broadcast_to_objs(VisionObjs, MessageData),

    {noreply, Data};    

handle_cast({broadcast, SourcePos, TargetPos, Range, MessageData}, Data) ->
    SourceObjs = get_nearby_objs(SourcePos, Range),
    TargetObjs = get_nearby_objs(TargetPos, Range),
    VisionObjs = util:unique_list(SourceObjs ++ TargetObjs),

    broadcast_to_objs(VisionObjs, MessageData),

    {noreply, Data};    

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({get_entity, Obj}, _From, Data) ->
    [Perception] = db:read(perception, obj:id(Obj)),

    {reply, Perception#perception.data, Data};

handle_call({get_player, PlayerId}, _From, Data) ->
    AllPerception = db:index_read(perception, PlayerId, #perception.player),

    F = fun(Perception, Acc) ->
            Perception#perception.data ++ Acc
        end,

    PlayerPerception = util:unique_list(lists:foldl(F, [], AllPerception)),

    {reply, PlayerPerception, Data};

handle_call({get_diff, Obj}, _From, Data) ->
    [Perception] = db:read(perception, obj:id(Obj)),

    {reply, Perception#perception.diff, Data};

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

broadcast_to_objs(Objs, Message) ->
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
visible_objs(AllObjs, #obj {id = Id, player = Player, pos = Pos, vision = Vision}) when Player =:= ?UNDEAD ->
    F = fun(Target, PerceptionData) when Target#obj.id =:= Id ->
                maps:put(Target#obj.id, Target, PerceptionData); %Include self
           (Target, PerceptionData) ->
            Result = Target#obj.state =/= hiding andalso %Not hiding
                     Target#obj.class =/= ?DELETING andalso %Not being deleted
                     Target#obj.vision >= 0 andalso %Obj being created
                     not effect:has_effect(Target#obj.id, ?SANCTUARY) andalso %Cannot see Sanctuary
                     map:distance(Pos, Target#obj.pos) =< Vision,

            case Result of
                true -> 
                    maps:put(Target#obj.id, Target, PerceptionData);
                false -> 
                    PerceptionData
            end
        end,

    lists:foldl(F, #{}, AllObjs);

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
            Result = Target#obj.state =/= hiding andalso %Not hiding
                     Target#obj.class =/= ?DELETING andalso %Not being deleted
                     Target#obj.vision >= 0 andalso %Obj being created
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

