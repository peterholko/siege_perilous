-module(astar).

-include("common.hrl").
-include("schema.hrl").

-export([astar/2, get_move_cost/1]).

astar(Start, Goal) ->
    Frontier = pqueue2:in(Start, 0, pqueue2:new()),
    CameFrom = dict:store(Start, none, dict:new()),
    CostSoFar = dict:store(Start, 0, dict:new()),

    search(pqueue2:is_empty(Frontier), Start, Goal, Frontier, CameFrom, CostSoFar).

search(true, _Start, _Goal, _Frontier, CameFrom, CostSoFar) ->
    {CameFrom, CostSoFar};
search(false, Start, Goal, Frontier1, CameFrom, CostSoFar) ->
    {{value, Current}, Frontier2} = pqueue2:out(Frontier1),
    %lager:info("Current: ~p Frontier2: ~p", [Current, Frontier2]),
    New = case Current =:= Goal of
            true -> {Frontier2, CameFrom, CostSoFar};
            false -> 
                  {X, Y} = Current,
                  Neighbours = get_neighbours(X, Y),
                  check_neighbours(Neighbours, Current, Goal, Frontier2, CameFrom, CostSoFar)
          end,

    {NewFrontier, NewCameFrom, NewCostSoFar} = New,
    search(pqueue2:is_empty(NewFrontier), Start, Goal, NewFrontier, NewCameFrom, NewCostSoFar).

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
    Neighbours = map:neighbours(X, Y, ?MAP_WIDTH, ?MAP_HEIGHT),
    F = fun(Neighbour) -> local:is_empty(Neighbour) end,
    lists:filter(F, Neighbours).
    
get_move_cost(Pos) ->
    map:movement_cost(Pos).
