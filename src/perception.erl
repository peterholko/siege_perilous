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
-export([is_visible/2]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, perception_pid}, perception, [], []).

recalculate() ->
    gen_server:cast({global, perception_pid}, recalculate).

broadcast(SourcePos, TargetPos, MessageData) ->
    gen_server:cast({global, perception_pid}, {broadcast, SourcePos, TargetPos, MessageData}).

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

    store_perception(Result, Entity#obj.id, NearbyObjs),

    send_perception(Result, Entity, NearbyObjs),

    entity_perception(Rest, AllObj).

compare_perception(_New, []) -> false;
compare_perception(New, [Old]) -> New =:= Old#perception.data.

store_perception(false, EntityId, NewPerception) ->
    db:dirty_write(#perception {entity=EntityId, data=NewPerception});
store_perception(_Result, _EntityId, _NewPerception) ->
    nothing.

send_perception(true, _, _) -> nothing;
send_perception(false, Entity, NewPerception) ->
    Process = case Entity#obj.subclass of
                  <<"villager">> -> 
                      global:whereis_name(villager);
                  _ -> 
                      [Conn] = db:read(connection, Entity#obj.player),
                      Conn#connection.process
              end,

    send_to_process(Process, {Entity#obj.id, NewPerception}).

send_to_process(Process, NewPerception) when is_pid(Process) ->
    lager:debug("Sending ~p to ~p", [NewPerception, Process]),
    Process ! {perception, NewPerception};
send_to_process(_Process, _NewPerception) ->
    none.

broadcast_to_objs(Objs, Message) ->
    Players = get_unique_players(Objs, []),
    
    F = fun(Player) ->
            [Conn] = db:read(connection, Player),
            lager:debug("Broadcasting ~p to ~p", [Message, Conn]),
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

visible_objs(AllObjs, #obj {pos = Pos, player = Player, vision = Vision}) when Player =< ?NPC_ID ->
    F = fun(Target, Visible) ->
            Result = Target#obj.state =/= hiding andalso
                     map:distance(Pos, Target#obj.pos) =< Vision andalso
                     not effect:has_effect(Target#obj.id, ?SANCTUARY),
            
            case Result of
                true -> [build_message(Target) | Visible];
                false -> Visible
            end
        end,

    lists:foldl(F, [], AllObjs);

visible_objs(AllObjs, #obj {pos = Pos, vision = Vision}) ->
    F = fun(Target, Visible) ->
            Result = Target#obj.state =/= hiding andalso
                     map:distance(Pos, Target#obj.pos) =< Vision,

            case Result of
                true -> [build_message(Target) | Visible];
                false -> Visible
            end
        end,

    lists:foldl(F, [], AllObjs).

build_message(MapObj) ->
    {X, Y} = MapObj#obj.pos,
    #{<<"id">> => MapObj#obj.id, 
      <<"player">> => MapObj#obj.player, 
      <<"x">> => X,
      <<"y">> => Y,
      <<"class">> => MapObj#obj.class,
      <<"type">> => MapObj#obj.name, %TODO fix client to accept name instead of type
      <<"vision">> => MapObj#obj.vision,
      <<"state">> => MapObj#obj.state}.

