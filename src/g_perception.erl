%% -------------------------------------------------------------------
%% Author  : Peter Holko
%%% Description : Calculates perception data
%%%
%%% Created : Dec 15, 2014
%%% -------------------------------------------------------------------
-module(g_perception).

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
    gen_server:start({global, g_perception_pid}, g_perception, [], []).

recalculate() ->
    gen_server:cast({global, g_perception_pid}, recalculate).

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
    %Erase process dict
    erase(),

    %Get all entities
    AllEntities = db:dirty_index_read(obj, entity, #obj.class),

    %Remove dead entities
    Entities = remove_dead(AllEntities, []),

    %Calculate each player entity perception and store to process dict
    entity_perception(Entities),

    %Get all player perceptions from process dict
    PlayerPerceptions = get(),

    %Compare new to previous perception
    UpdatePlayers = compare_perception(PlayerPerceptions, []),

    lager:debug("Players to update: ~p", [UpdatePlayers]),
    send_perception(UpdatePlayers).

remove_dead([], Entities) ->
    Entities;
remove_dead([Entity | Rest] , Entities) ->

    NewEntities = is_dead(Entities, Entity, Entity#obj.state),
    remove_dead(Rest, NewEntities).

is_dead(Entities, _Entity, dead) ->
    Entities;
is_dead(Entities, Entity, _State) ->
    [Entity | Entities].

entity_perception([]) ->
    done;

entity_perception([Entity | Rest]) ->
    %Get current player perception from process dict
    PlayerPerception = convert_undefined(get(Entity#obj.player)),
    
    NearbyObjs = map:get_nearby_objs(Entity#obj.pos),

    NewPlayerPerception = util:unique_list(PlayerPerception ++ NearbyObjs),
    
    %Store new player perception to process dict
    put(Entity#obj.player, NewPlayerPerception),

    entity_perception(Rest).

convert_undefined(undefined) ->
    [];
convert_undefined(Data) ->
    Data.

compare_perception([], UpdatePlayers) ->
    UpdatePlayers;

compare_perception([{Player, NewObjPerception} | Rest], UpdatePlayers) ->
    ExploredTiles = map:get_explored(Player),
    NewPerception = [{<<"explored">>, ExploredTiles}, 
                     {<<"objs">>, NewObjPerception}],

    OldPerception = db:dirty_read(perception, Player),

    Result = perception_equal(NewPerception, OldPerception),

    NewUpdatePlayers = store_perception(UpdatePlayers, Player, NewPerception, Result),

    compare_perception(Rest, NewUpdatePlayers).

perception_equal(_NewData, []) ->
    false;

perception_equal(New, [Old]) ->
    New =:= Old#perception.data.

store_perception(Players, Player, NewPerception, false) ->
    db:dirty_write(#perception {player=Player, data=NewPerception}),
    [{Player, NewPerception} | Players];

store_perception(Players, _Player, _Perception, _Result) ->
    Players.

send_perception([]) ->
    lager:info("Sent all perception updates");

send_perception([{PlayerId, NewPerception} | Players]) ->
    [Conn] = db:dirty_read(connection, PlayerId),
    send_to_process(Conn#connection.process, NewPerception),
    send_perception(Players).

send_to_process(Process, NewPerception) when is_pid(Process) ->
    lager:info("Sending ~p to ~p", [NewPerception, Process]),
    Process ! {map_perception, NewPerception};
send_to_process(_Process, _NewPerception) ->
    none.
