%% -------------------------------------------------------------------
%% Author  : Peter Holko
%%% Description : NPC server
%%%
%%% Created : Jan 5, 2015
%%% -------------------------------------------------------------------
-module(npc).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

start(PlayerId) ->
    gen_server:start({global, {npc, PlayerId}}, npc, [PlayerId], []).

%% ====================================================================
%% Server functions
%% ====================================================================

init([PlayerId]) ->
    %Store player id in process dict
    put(player_id, PlayerId),

    %Store npc process in connection table
    [Connection] = db:dirty_read(connection, PlayerId),
    NewConnection = Connection#connection {process = self()},
    db:dirty_write(NewConnection),

    {ok, []}.

handle_cast(create, Data) ->   

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

handle_info({new_perception, Perception}, Data) ->

    lager:info("NPC perception: ~p", [Perception]),

    {Explored, Objs} = new_perception(Perception),
    {NPCObjs, EnemyObjs} = split_objs(Objs, [], []),
    Action = process_action(NPCObjs, Explored, Objs),

    {noreply, Data};

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

new_perception([{<<"explored">>, Explored}, {<<"objs">>, Objs}]) ->
    {Explored, Objs}.    
        
split_objs([], NPCObjs, EnemyObjs) ->
    {NPCObjs, EnemyObjs};
split_objs([Obj | Rest], NPCObjs, EnemyObjs) ->

    Player = get(player_id),
    ObjPlayer = maps:get(<<"player">>, Obj),
    ComparePlayer = Player =:= ObjPlayer,

    {NewNPCObjs, NewEnemyObjs} = add_npc_obj(Obj, NPCObjs, ComparePlayer),

    get_npc_objs(Rest, NewNPCObjs, NewEnemyObjs).

add_npc_obj(_Obj, NPCObjs, EnemyObjs, false) ->
    {NPCObjs, [Obj | NPCObjs;
add_npc_obj(Obj, NPCObjs, true) ->
    [Obj | NPCObjs].

process_action(NPCObjs, Explored, Objs) ->

    %Do nothing for now for explored

    Action = none.
    NewAction = check_objs(NPCObjs, Objs, Action),

check_objs(_NPCObjs, [], Action) ->
    Action;

check_objs([NPCObj], [Obj | Rest], Action) ->

    Player = get(player_id),
    NPCPos = maps:get(<<"pos">>, NPCObj),

    Id = maps:get(<<"id">>, Obj),
    ObjPlayer = maps:get(<<"player">>, Obj),
    Pos = maps:get(<<"pos">>, Obj),

    CheckPlayer = Player =:= ObjPlayer,
    CheckPos = NPCPos =:= NPCPos, 

    NewAction = determine_action(Action, 
                                 CheckPlayer,
                                 CheckPos,
                                 Id,
                                 Pos),

determine_action(none, false, Id, Pos) ->
    {move, pos}
