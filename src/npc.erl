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

handle_info({map_perception, Perception}, Data) ->

    lager:info("NPC perception: ~p", [Perception]),

    {Explored, Objs} = new_perception(Perception),
    {NPCObjs, EnemyObjs} = split_objs(Objs, [], []),
    process_action(NPCObjs, EnemyObjs),

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

    {NewNPCObjs, NewEnemyObjs} = add_npc_obj(Obj, NPCObjs, EnemyObjs, ComparePlayer),

    split_objs(Rest, NewNPCObjs, NewEnemyObjs).

add_npc_obj(Obj, NPCObjs, EnemyObjs, true) ->
    {[Obj | NPCObjs], EnemyObjs};
add_npc_obj(Obj, NPCObjs, EnemyObjs, false) ->
    {NPCObjs, [Obj | EnemyObjs]}.

process_action(NPCObjs, EnemyObjs) ->
    %Do nothing for now for explored

    F = fun(NPCObj) ->
            Action = none,
            NewAction = check_objs(NPCObj, EnemyObjs, Action),
            add_action(NewAction)
        end,

    lists:foreach(F, NPCObjs).

check_objs(_NPCObjs, [], Action) ->
    Action;

check_objs(NPCObj, [EnemyObj | Rest], Action) ->

    NPCId = maps:get(<<"id">>, NPCObj),
    NPCPos = maps:get(<<"pos">>, NPCObj),
    NPCState = maps:get(<<"state">>, NPCObj),

    Id = maps:get(<<"id">>, EnemyObj),
    Pos = maps:get(<<"pos">>, EnemyObj),
    State = maps:get(<<"state">>, EnemyObj),

    CheckPos = NPCPos =:= Pos, 

    lager:info("Action: ~p CheckPos: ~p NPC: ~p Enemy: ~p", [Action, CheckPos, {NPCId, NPCPos, NPCState}, {Id, Pos, State}]),
    NewAction = determine_action(Action, 
                                 CheckPos,
                                 {NPCId, NPCPos, NPCState},
                                 {Id, Pos, State}),
    lager:info("NewAction: ~p", [NewAction]),
    check_objs(NPCObj, Rest, NewAction).

determine_action(_Action, false, {NPCId, _NPCPos, none}, {_Id, Pos, none}) ->
    lager:info("determine_action move npcid: ~p pos: ~p", [NPCId, Pos]),
    {move, {NPCId, Pos}};
determine_action(_Action, true, {NPCId, _NPCPos, none}, {Id, _Pos, none}) ->
    lager:info("determine_action attack npcid: ~p id: ~p", [NPCId, Id]),
    {attack, {NPCId, Id}};
determine_action(_Action, _Pos, NPC, _Enemy) ->
    {none, {NPC}}.

add_action({move, {NPCId, Pos1D}}) ->
    lager:info("npc ~p adding move", [NPCId]),
    Obj = map:get_obj(NPCId),

    map:update_obj_state(Obj, moving),
    
    NewPos = map:convert_coords(Pos1D),

    NumTicks = 8,

    %Create event data 
    EventData = {Obj#map_obj.player,
                 Obj#map_obj.id,
                 NewPos},
    
    game:add_event(self(), move_obj, EventData, NumTicks);

add_action({attack, {NPCId, Id}}) ->
    lager:info("npc adding attack"),

    NumTicks = 8,
    EventData = {NPCId, Id},
 
    game:add_event(self(), attack_obj, EventData, NumTicks);

add_action(none) ->
    lager:info("NPC doing nothing").
