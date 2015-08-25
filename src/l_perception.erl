%% -------------------------------------------------------------------
%% Author  : Peter Holko
%%% Description : Calculates local perception data
%%%
%%% Created : Dec 15, 2014
%%% -------------------------------------------------------------------
-module(l_perception).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([recalculate/1, broadcast/4]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, l_perception_pid}, l_perception, [], []).

recalculate(GlobalPos) ->
    gen_server:cast({global, l_perception_pid}, {recalculate, GlobalPos}).

broadcast(GlobalPos, SourcePos, TargetPos, MessageData) ->
    gen_server:cast({global, l_perception_pid}, {broadcast, GlobalPos, SourcePos, TargetPos, MessageData}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast({recalculate, GlobalPos}, Data) ->   
    do_recalculate(GlobalPos),
    {noreply, Data};

handle_cast({broadcast, GlobalPos, SourcePos, TargetPos, MessageData}, Data) ->
    SourceObjs = map:get_nearby_objs(SourcePos, {local_map, GlobalPos}, 2),
    TargetObjs = map:get_nearby_objs(TargetPos, {local_map, GlobalPos}, 2),

    broadcast_to_objs(SourceObjs ++ TargetObjs, MessageData),

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

do_recalculate(GlobalPos) ->
    lager:info("Local perception recalculate ~p", [GlobalPos]),
    %Erase process dict
    erase(),

    %Get all entities
    AllObj = db:index_read(local_obj, GlobalPos, #local_obj.global_pos),
    Entities = filter_objs(AllObj),

    lager:debug("Calculate perceptions ~p", [Entities]),
    %Calculate each player entity perception and store to process dict
    entity_perception(Entities, GlobalPos),

    %Get all entity perceptions from process dict
    EntityPerceptions = get(),

    %Compare new to previous perception
    EntitiesToUpdate = compare_perception(EntityPerceptions, []),

    lager:debug("Entities to update: ~p", [EntitiesToUpdate]),
    send_perception(EntitiesToUpdate).

filter_objs(AllObj) ->
    F = fun(Obj) -> Obj#local_obj.vision end,
    lists:filter(F, AllObj).

entity_perception([], _GlobalPos) ->
    done;

entity_perception([Entity | Rest], GlobalPos) ->
    lager:debug("Entity perception: ~p", [Entity]),    
    NearbyObjs = map:get_nearby_objs(Entity#local_obj.pos, {local_map,GlobalPos}, ?LOS),
    lager:debug("NearbyObjs: ~p", [NearbyObjs]), 
    put(Entity#local_obj.id, NearbyObjs),

    entity_perception(Rest, GlobalPos).

compare_perception([], EntitiesUpdated) ->
    EntitiesUpdated;

compare_perception([{random_seed, _} | Rest], EntitiesUpdated) ->
    compare_perception(Rest, EntitiesUpdated);

compare_perception([{EntityId, NewPerception} | Rest], EntitiesUpdated) ->
    OldPerception = db:dirty_read(perception, EntityId),

    Result = perception_equal(NewPerception, OldPerception),

    NewEntitiesUpdated = store_perception(EntitiesUpdated, EntityId, NewPerception, Result),

    compare_perception(Rest, NewEntitiesUpdated).

perception_equal(_NewData, []) ->
    false;

perception_equal(New, [Old]) ->
    New =:= Old#perception.data.

store_perception(Entities, EntityId, NewPerception, false) ->
    db:dirty_write(#perception {entity=EntityId, data=NewPerception}),
    [{EntityId, NewPerception} | Entities];

store_perception(Entities, _EntityId, _NewPerception, _Result) ->
    Entities.

send_perception([]) ->
    lager:debug("Sent all perception updates");

send_perception([{EntityId, NewPerception} | Players]) ->
    [Entity] = db:dirty_read(local_obj, EntityId),
    [Conn] = db:dirty_read(connection, Entity#local_obj.player),
    send_to_process(Conn#connection.process, {EntityId, NewPerception}),
    send_perception(Players).

send_to_process(Process, NewPerception) when is_pid(Process) ->
    lager:debug("Sending ~p to ~p", [NewPerception, Process]),
    Process ! {local_perception, NewPerception};
send_to_process(_Process, _NewPerception) ->
    none.

broadcast_to_objs(Objs, Message) ->
    Players = get_unique_players(Objs, []),
    
    F = fun(Player) ->
            [Conn] = db:dirty_read(connection, Player),
            lager:debug("Broadcasting ~p to ~p", [Message, Player]),
            Conn#connection.process ! {broadcast, Message}
        end,
    
    lists:foreach(F, Players).    

get_unique_players([], Players) ->
    util:unique_list(Players);
get_unique_players([Obj | Rest], Players) ->
    Player = maps:get(<<"player">>, Obj),
    NewPlayers = case valid_player(Player) of
                     true -> 
                         [maps:get(<<"player">>, Obj) | Players];
                     false ->
                         Players
                 end,

    get_unique_players(Rest, NewPlayers).

valid_player(PlayerId) when PlayerId < 1 -> false;
valid_player(_PlayerId) -> true.

