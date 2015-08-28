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
    [LocalObj] = db:dirty_read(local_obj, Id),

    process_state(Villager, LocalObj),
    
    process(mnesia:dirty_next(villager, Id)).

process_state(Villager, LocalObj = #local_obj {state = State}) when State =:= none ->
    Task = Villager#villager.task,
    process_task(Task, Villager, LocalObj);
process_state(_Villager, _LocalObj) ->
    nothing.

process_task({structure, StructureId}, Villager, LocalObj) ->
    [Structure] = db:dirty_read(local_obj, StructureId),

    %Check same position    
    SamePos = Structure#local_obj.pos =:= LocalObj#local_obj.pos,
    
    %Assign action
    Action = case SamePos of
                true ->
                    {harvest, Structure#local_obj.id};
                false ->
                    {move_dest, Structure#local_obj.pos}
             end,

    process_action(Action, Villager, LocalObj);

process_task(_, _, _) ->
    nothing.

process_action({move_dest, Dest}, _Villager, LocalObj) ->
    Pathfinding = astar:astar(LocalObj#local_obj.pos, Dest),
    
    case Pathfinding of 
        failure ->
            lager:info("~p could not find path", [LocalObj#local_obj.id]);
        PathList ->
            NextPos = lists:nth(2, PathList),
            add_move_unit(LocalObj, NextPos)
    end;
process_action({harvest, _StructureId}, _Villager, LocalObj) ->
    local:update_state(LocalObj#local_obj.id, harvesting),
    EventData = {LocalObj#local_obj.id, <<"Cragroot Popular">>, 50, true},
    game:add_event(self(), harvest, EventData, LocalObj#local_obj.id, 50);
 
process_action(_, _, _) ->
    nothing.
    
add_move_unit(LocalObj, NewPos) ->
    %Update unit state
    local:update_state(LocalObj#local_obj.id, moving),
    
    %Create event data
    EventData = {LocalObj#local_obj.global_pos,
                 LocalObj#local_obj.player,
                 LocalObj#local_obj.id,
                 NewPos},

    NumTicks = 30,

    game:add_event(self(), move_local_obj, EventData, LocalObj#local_obj.id, NumTicks).
