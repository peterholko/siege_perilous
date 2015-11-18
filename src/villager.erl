%% -------------------------------------------------------------------
%% Author  : Peter Holko
%%% Description : Villager Manager server
%%%
%%% Created : Aug 6, 2015
%%% -------------------------------------------------------------------
-module(villager).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([check_task/0, assign/2]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, villager}, villager, [], []).

check_task() ->
    gen_server:cast({global, villager}, process).

assign(SourceId, TargetId) ->
    Villager = #villager {id = SourceId,
                          task = {structure, TargetId},
                          dwelling = none},
    db:write(Villager).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast(process, Data) ->   

    process(mnesia:dirty_first(villager)),

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
process('$end_of_table') ->
    lager:debug("Done processing villagers");
process(Id) ->
    [Villager] = db:dirty_read(villager, Id),
    [Obj] = db:dirty_read(obj, Id),

    process_state(Villager, Obj),
    
    process(mnesia:dirty_next(villager, Id)).

process_state(Villager, Obj = #obj {state = State}) when State =:= none ->
    Task = Villager#villager.task,
    process_task(Task, Villager, Obj);
process_state(_Villager, _Obj) ->
    nothing.

process_task({structure, StructureId}, Villager, Obj) ->
    [Structure] = db:dirty_read(obj, StructureId),

    %Check same position    
    SamePos = Structure#obj.pos =:= Obj#obj.pos,
    
    %Assign action
    Action = case SamePos of
                true ->
                    {harvest, Structure#obj.id};
                false ->
                    {move_dest, Structure#obj.pos}
             end,

    process_action(Action, Villager, Obj);

process_task(_, _, _) ->
    nothing.

process_action({move_dest, Dest}, _Villager, Obj) ->
    Pathfinding = astar:astar(Obj#obj.pos, Dest),
    
    case Pathfinding of 
        failure ->
            lager:info("~p could not find path", [Obj#obj.id]);
        PathList ->
            NextPos = lists:nth(2, PathList),
            add_move_unit(Obj, NextPos)
    end;
process_action({harvest, _StructureId}, _Villager, Obj) ->
    obj:update_state(Obj#obj.id, harvesting),
    EventData = {Obj#obj.id, <<"Cragroot Popular">>, 50, true},
    game:add_event(self(), harvest, EventData, Obj#obj.id, 50);
 
process_action(_, _, _) ->
    nothing.
    
add_move_unit(Obj, NewPos) ->
    %Update unit state
    obj:update_state(Obj#obj.id, moving),
    
    %Create event data
    EventData = {Obj#obj.player,
                 Obj#obj.id,
                 NewPos},

    NumTicks = 30,

    game:add_event(self(), move_obj, EventData, Obj#obj.id, NumTicks).
