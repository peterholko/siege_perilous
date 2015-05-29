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

    %Get all player perceptions from process dict
    PlayerPerceptions = get(),

    lager:debug("PlayerPerceptions ~p", [PlayerPerceptions]),
    %Compare new to previous perception
    UpdatePlayers = compare_perception(PlayerPerceptions, []),

    lager:info("Players to update: ~p", [UpdatePlayers]),
    send_perception(UpdatePlayers).

filter_objs(AllObj) ->
    F = fun(Obj) -> Obj#local_obj.vision end,
    lists:filter(F, AllObj).

entity_perception([], _GlobalPos) ->
    done;

entity_perception([Entity | Rest], GlobalPos) ->
    %Get current player perception from process dict
    PlayerId = Entity#local_obj.player,
    PlayerPerception = convert_undefined(get({PlayerId, GlobalPos})),

    lager:debug("Entity perception: ~p", [Entity]),    
    NearbyObjs = map:get_nearby_objs(Entity#local_obj.pos, {local_map,GlobalPos}, 4),
    lager:debug("NearbyObjs: ~p", [NearbyObjs]), 
    NewPlayerPerception = util:unique_list(PlayerPerception ++ NearbyObjs),

    lager:debug("PlayerId: ~p GlobalPos: ~p", [PlayerId, GlobalPos]),  
    %Store new player perception to process dict
    put({PlayerId, GlobalPos}, NewPlayerPerception),

    entity_perception(Rest, GlobalPos).

convert_undefined(undefined) ->
    [];
convert_undefined(Data) ->
    Data.

compare_perception([], UpdatePlayers) ->
    UpdatePlayers;

compare_perception([{random_seed, _} | Rest], UpdatePlayers) ->
    compare_perception(Rest, UpdatePlayers);

compare_perception([{{Player, GlobalPos}, NewObjPerception} | Rest], UpdatePlayers) ->
    ExploredTiles = map:get_local_explored(Player, GlobalPos, new),
    lager:debug("ExploredTiles ~p", [ExploredTiles]),
    NewPerception = [{<<"explored">>, ExploredTiles},
                     {<<"objs">>, NewObjPerception}],

    OldPerception = db:dirty_read(perception, {Player, GlobalPos}),

    Result = perception_equal(NewPerception, OldPerception),

    NewUpdatePlayers = store_perception(UpdatePlayers, {Player, GlobalPos}, NewPerception, Result),

    compare_perception(Rest, NewUpdatePlayers).

perception_equal(_NewData, []) ->
    false;

perception_equal(New, [Old]) ->
    New =:= Old#perception.data.

store_perception(Players, {Player, GlobalPos}, NewPerception, false) ->
    db:dirty_write(#perception {player={Player, GlobalPos}, data=NewPerception}),
    [{Player, NewPerception} | Players];

store_perception(Players, _Player, _Perception, _Result) ->
    Players.

send_perception([]) ->
    lager:debug("Sent all perception updates");

send_perception([{PlayerId, NewPerception} | Players]) ->
    [Conn] = db:dirty_read(connection, PlayerId),
    send_to_process(Conn#connection.process, NewPerception),
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
            lager:info("Broadcasting ~p to ~p", [Message, Player]),
            Conn#connection.process ! {broadcast, Message}
        end,
    
    lists:foreach(F, Players).    

get_unique_players([], Players) ->
    util:unique_list(Players);
get_unique_players([Obj | Rest], Players) ->
    NewPlayers = [maps:get(<<"player">>, Obj) | Players],
    get_unique_players(Rest, NewPlayers).
