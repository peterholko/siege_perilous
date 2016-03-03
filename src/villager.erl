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

    process(db:first(villager)),

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
    [Villager] = db:read(villager, Id),
    [Obj] = db:read(obj, Id),

    check_food(Villager, Obj),
    check_dwelling(Villager, Obj),

    process_state(Villager, Obj),
    
    process(db:next(villager, Id)).

process_state(Villager, Obj = #obj{state = State}) when State =:= none ->
    process_morale(Villager, Obj);
process_state(_Villager, _Obj) ->
    nothing.

process_morale(Villager = #villager{morale = Morale}, Obj) when Morale >= 50 ->
    process_task(Villager#villager.task, Villager, Obj);
process_morale(Villager = #villager{morale = Morale}, Obj) when Morale >= 25 ->
    NewVillager = Villager#villager{task = forage},
    db:write(NewVillager),
    process_task(Villager#villager.task, Villager, Obj);
process_morale(Villager = #villager{morale = Morale}, Obj) when Morale >= 0 ->
    process_abandon(Villager, Obj).

process_task(assign, Villager, Obj) ->
    [Structure] = db:read(obj, Villager#villager.structure),

    %Check same position    
    SamePos = Structure#obj.pos =:= Obj#obj.pos,
    
    %Assign action
    Action = case SamePos of
                true ->
                    {harvest, Structure#obj.id};
                false ->
                    {move_to, Structure#obj.pos}
             end,

    process_action(Action, Villager, Obj);
process_task(gather, Villager, Obj) ->
    Action = case resource:survey(Obj#obj.pos) of
                [Resource | _Rest] ->
                    ResourceName = maps:get(<<"name">>, Resource),
                    {harvest, ResourceName};
                [] ->
                    Pos = map:get_random_neighbour(Obj#obj.pos),
                    {move_to, Pos}
             end,

    process_action(Action, Villager, Obj);

process_task(forage, Villager, Obj) ->
    Action = case resource:survey(Obj#obj.pos) of
                 [Resource | _Rest] ->
                     ResourceName = maps:get(<<"name">>, Resource),
                     
                     case item:is_subclass(ResourceName, ?FOOD) of
                         true -> 
                             {harvest, ResourceName};
                         false ->
                             Pos = map:get_random_neighbour(Obj#obj.pos),
                             {move_to, Pos}
                     end;
                 [] ->
                    Pos = map:get_random_neighbour(Obj#obj.pos),
                    {move_to, Pos}
             end,

    process_action(Action, Villager, Obj);

process_task(follow, _Villager, Obj) ->
    [Hero] = db:read(hero, Obj#obj.player),
    [HeroObj] = db:read(obj, Hero#hero.obj),
    Path = astar:astar(Obj#obj.pos, HeroObj#obj.pos),
    
    case Path of
        failure -> lager:info("~p could not find path", [Obj#obj.id]);
        PathList when length(PathList) =< 2 -> lager:info("Adjacent to following target");
        PathList ->
            NextPos = lists:nth(2, PathList),
            add_move_unit(Obj, NextPos)
    end.

process_action({move_to, Dest}, _Villager, Obj) ->
    Pathfinding = astar:astar(Obj#obj.pos, Dest),
    
    case Pathfinding of 
        failure ->
            lager:info("~p could not find path", [Obj#obj.id]);
        PathList ->
            NextPos = lists:nth(2, PathList),
            add_move_unit(Obj, NextPos)
    end;
process_action({harvest, ResourceName}, _Villager, Obj) ->
    obj:update_state(Obj#obj.id, harvesting),
    EventData = {Obj#obj.id, ResourceName, Obj#obj.pos, 25, false},
    game:add_event(self(), harvest, EventData, Obj#obj.id, 25);
process_action(_, _, _) ->
    nothing.

process_abandon(Villager, Obj) ->
    NewVillager = Villager#villager{task = forage,
                                    morale = 50}, 

    [NewObj] = Obj#obj{player = ?NATIVES},

    db:write(NewVillager),
    db:write(NewObj).

add_move_unit(Obj, NewPos) ->
    %Update unit state
    obj:update_state(Obj#obj.id, moving),
    
    %Create event data
    EventData = {Obj#obj.player,
                 Obj#obj.id,
                 NewPos},

    NumTicks = 16,
    lager:info("Villager add move ~p", [NewPos]),
    game:add_event(self(), move, EventData, Obj#obj.id, NumTicks).

check_food(Villager, Obj) ->
    case item:get_by_subclass(Obj#obj.id, ?FOOD) of
        [] -> update_morale(Villager, -5);
        [_Item | _Rest] -> update_morale(Villager, 5) 
    end.

check_dwelling(Villager, _Obj) ->
    case Villager#villager.dwelling of
        none -> update_morale(Villager, -1);
        _ -> update_morale(Villager, 1)
    end.

update_morale(Villager, Value) ->
    Morale = Villager#villager.morale + Value,

    NewMorale = case Morale > 0 of 
                    true -> erlang:min(Morale, 100);
                    false -> 0
                end,

    NewVillager = Villager#villager {morale = NewMorale},
    db:write(NewVillager).

