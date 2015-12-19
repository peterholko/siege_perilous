-module(astar).

-include("common.hrl").
-include("schema.hrl").

-export([astar/2, get_move_cost/1]).

astar(Start, Goal) ->
    lager:debug("Start: ~p Goal: ~p", [Start, Goal]),
    Frontier = pqueue2:in(Start, 0, pqueue2:new()),
    CameFrom = dict:store(Start, none, dict:new()),
    CostSoFar = dict:store(Start, 0, dict:new()),

    {From, _Cost} = search(pqueue2:is_empty(Frontier), search, Start, Goal, Frontier, CameFrom, CostSoFar),

    %Check if a path is found
    Path = case dict:is_key(Goal, From) of
                true ->
                    Next = dict:fetch(Goal, From),
                    to_path(Start, Next, From, [Goal]);
                false ->
                    []
           end,
    Path.
        
search(true, _EarlyExit, _Start, _Goal, _Frontier, CameFrom, CostSoFar) ->
    {CameFrom, CostSoFar};
search(_, early_exit, _Start, _Goal, _Frontier, CameFrom, CostSoFar) ->
    {CameFrom, CostSoFar};    
search(false, _EarlyExit, Start, Goal, Frontier1, CameFrom, CostSoFar) ->
    {{value, Current}, Frontier2} = pqueue2:out(Frontier1),

    New = case Current =:= Goal of
            true -> {early_exit, {Frontier2, CameFrom, CostSoFar}};
            false -> 
                  {X, Y} = Current,
                  Neighbours = get_neighbours(X, Y),
                  {search, check_neighbours(Neighbours, Current, Goal, Frontier2, CameFrom, CostSoFar)}
          end,

    {NewEarlyExit, {NewFrontier, NewCameFrom, NewCostSoFar}} = New,
    search(pqueue2:is_empty(NewFrontier), NewEarlyExit, Start, Goal, NewFrontier, NewCameFrom, NewCostSoFar).

check_neighbours([], _Current, _Goal, NewFrontier, NewCameFrom, NewCostSoFar) ->
    {NewFrontier, NewCameFrom, NewCostSoFar};
check_neighbours([Neighbour | Rest], Current, Goal, Frontier, CameFrom, CostSoFar) ->
    NewCost = dict:fetch(Current, CostSoFar) + get_move_cost(Neighbour),
    Result = not dict:is_key(Neighbour, CostSoFar) orelse NewCost < dict:fetch(Neighbour, CostSoFar),

    New = case Result of
                true -> 
                    Priority = NewCost + heuristic(Goal, Neighbour),
                    Frontier2 = pqueue2:in(Neighbour, Priority, Frontier),
                    CameFrom2 = dict:store(Neighbour, Current, CameFrom),
                    CostSoFar2 = dict:store(Neighbour, NewCost, CostSoFar),
                    {Frontier2, CameFrom2, CostSoFar2};
                false ->
                    {Frontier, CameFrom, CostSoFar}
          end,

    {NewFrontier, NewCameFrom, NewCostSoFar} = New,
    check_neighbours(Rest, Current, Goal, NewFrontier, NewCameFrom, NewCostSoFar).

heuristic(Start, End) ->
    2 * map:distance(Start, End).

get_neighbours(X, Y) ->
    Neighbours = map:neighbours(X, Y),
    F = fun(Pos) -> map:is_passable(Pos) end,
    lists:filter(F, Neighbours).
    
get_move_cost(Pos) ->
    MoveCost = case obj:is_empty(Pos) of
                    true -> map:movement_cost(Pos);
                    false -> 10
               end,
    MoveCost.

to_path(Start, Next, _From, Path) when Start =:= Next ->
    [Start | Path];
to_path(Start, Next, From, Path) ->    
    NewNext = dict:fetch(Next, From),
    to_path(Start, NewNext, From, [Next | Path]).
