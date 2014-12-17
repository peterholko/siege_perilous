%% -------------------------------------------------------------------
%% Author  : Peter Holko
%%% Description : Calculates perception data
%%%
%%% Created : Dec 15, 2014
%%% -------------------------------------------------------------------
-module(perception_manager).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([recalculate/0]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, perception_pid}, perception_manager, [], []).

recalculate() ->
    gen_server:cast({global, perception_pid}, recalculate).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast(recalculate, Data) ->   

    do_recalculate(),

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

do_recalculate() ->

    Entities = db:dirty_index_read(map_obj, entity, #map_obj.type),
    UpdatePlayers = entity_perception(Entities, []),
    lager:info("Players to update: ~p", [UpdatePlayers]),
    send_perception(UpdatePlayers).

entity_perception([], Players) ->
    Players;

entity_perception([Entity | Rest], Players) ->
    
    NearbyObjs = map:get_nearby_objs(Entity#map_obj.pos),
    PrevPerception = db:dirty_read(perception, Entity#map_obj.player),

    Result = compare_perception(NearbyObjs, PrevPerception),
    NewPlayers = store_perception(Players, 
                                  Entity#map_obj.player, 
                                  NearbyObjs, 
                                  Result),

    entity_perception(Rest, NewPlayers).

compare_perception(_NewData, []) ->
    false;

compare_perception(New, [Old]) ->
    lager:info("New: ~p Old: ~p", [New, Old]),
    New =:= Old#perception.data.

store_perception(Players, PlayerId, NewPerception, false) ->
    db:dirty_write(#perception {player=PlayerId, data=NewPerception}),
    [{PlayerId, NewPerception} | Players];

store_perception(Players, _PlayerId, _NearbyObjs, _Result) ->
    Players.

send_perception([]) ->
    lager:info("Sent all perception updates");

send_perception([{PlayerId, NewPerception} | Players]) ->
    [Conn] = db:dirty_read(connection, PlayerId),
    lager:info("Connection: ~p", [Conn]),
    send_to_process(Conn#connection.process, NewPerception),
    send_perception(Players).

send_to_process(Process, NewPerception) when is_pid(Process) ->
    lager:info("Sending ~p to ~p", [NewPerception, Process]),
    Process ! {new_perception, {<<"objs">>, NewPerception}};
send_to_process(_Process, _NewPerception) ->
    none.
