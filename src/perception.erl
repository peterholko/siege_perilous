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
-export([recalculate/0, broadcast/3]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, perception_pid}, perception, [], []).

recalculate() ->
    gen_server:cast({global, perception_pid}, recalculate).

broadcast(SourcePos, TargetPos, MessageData) ->
    gen_server:cast({global, perception_pid}, {broadcast, SourcePos, TargetPos, MessageData}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast(recalculate, Data) ->   
    do_recalculate(),
    {noreply, Data};

handle_cast({broadcast, SourcePos, TargetPos, MessageData}, Data) ->
    SourceObjs = map:get_nearby_objs(SourcePos, 2),
    TargetObjs = map:get_nearby_objs(TargetPos, 2),

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

do_recalculate() ->
    lager:info("Perception recalculate"),
    %Erase process dict
    erase(),

    %Get all entities
    AllObj = ets:tab2list(obj),
    Entities = filter_objs(AllObj),

    lager:debug("Calculate perceptions ~p", [Entities]),
    %Calculate each player entity perception and store to process dict
    entity_perception(Entities, AllObj).

filter_objs(AllObj) ->
    F = fun(Obj) -> Obj#obj.vision > 0 end,
    lists:filter(F, AllObj).

entity_perception([]) ->
    done;

entity_perception([Entity | Rest], AllObj) ->
    lager:debug("Entity perception: ~p", [Entity]),    
    NearbyObjs = map:get_nearby_objs(Entity#obj.pos, Entity#obj.vision),
    lager:debug("NearbyObjs: ~p", [NearbyObjs]), 

    %Get old perception
    OldPerception = db:dirty_read(perception, Entity#obj.id),

    %Check if new perception equals old perception
    Result = perception_equal(NearbyObjs, OldPerception),

    %Store new perception if is different than old perception
    store_perception(Result, Entity#obj.id, NearbyObjs),
    
    %Send new perception if different than old perception
    send_perception(Result, Entity#obj.player, Entity#obj.id, NearbyObjs),    

    entity_perception(Rest, AllObj).

perception_equal(_NewData, []) ->
    false;

perception_equal(New, [Old]) ->
    New =:= Old#perception.data.

store_perception(false, EntityId, NewPerception) ->
    db:dirty_write(#perception {entity=EntityId, data=NewPerception});
store_perception(_Result, _EntityId, _NewPerception) ->
    nothing.

send_perception(true, _PlayerId, _EntityId, _NewPerception) ->
    nothing;
send_perception(false, PlayerId, EntityId, NewPerception) ->
    [Conn] = db:dirty_read(connection, PlayerId),
    send_to_process(Conn#connection.process, {EntityId, NewPerception}).

send_to_process(Process, NewPerception) when is_pid(Process) ->
    lager:debug("Sending ~p to ~p", [NewPerception, Process]),
    Process ! {perception, NewPerception};
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

visible_objs(AllObjs, Entity) -> 
    VisibleObjs = filter_visible(AllObjs, Entity),



filter_visible(AllObjs, #obj {player = Player}) when Player =:= 99 ->
    F = fun(Obj, VisibleObjs) ->
            NewVisibleObjs = case obj:has_effect(Obj#obj.id, <<"sanctuary">>) of
                                 true ->
                                     VisibleObjs;
                                 false ->

        end,


    lists:foldl(F, [], AllObjs);
