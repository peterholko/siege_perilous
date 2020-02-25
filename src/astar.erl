-module(astar).

-include("common.hrl").
-include("schema.hrl").

-export([astar/3, astar/5, get_move_cost/2]).

astar(Start, Start, _) ->
    {success, [Start]};
astar(Start, Goal, Obj) ->
    lager:debug("Start3: ~p Goal: ~p", [Start, Goal]),
    astar(Start, Goal, Obj, true, true).

astar(Start, Goal, Obj, CheckPassable, CheckBlocking) ->
    lager:debug("Start5: ~p Goal: ~p", [Start, Goal]),
    Frontier = pqueue2:in(Start, 0, pqueue2:new()),
    CameFrom = dict:store(Start, none, dict:new()),
    CostSoFar = dict:store(Start, 0, dict:new()),

    {From, _Cost, Result} = search(pqueue2:is_empty(Frontier), 
                                   search, 
                                   Start, 
                                   Goal, 
                                   Frontier, 
                                   CameFrom, 
                                   CostSoFar, 
                                   Obj,
                                   CheckPassable,
                                   CheckBlocking,
                                   []),
    %lager:info("From: ~p", [From]),
    %lager:info("Cost: ~p", [Cost]),
    %lager:info("Result: ~p", [Result]),

    case Result of
        {nearby, Dist, Closest} ->
            {nearby, Dist, Closest};
        success ->
            %Check if a path is found
            FinalResult = case dict:is_key(Goal, From) of
                            true ->
                                Next = dict:fetch(Goal, From),
                                Path = to_path(Start, Next, From, [Goal]),
                                {success, Path};
                            false ->
                                {failed, []}
                          end,
            FinalResult;
        none ->
            {failed, []}
    end.
                
search(true, _EarlyExit, _Start, _Goal, _Frontier, CameFrom, CostSoFar, _Obj, _CheckPassable, _CheckBlocking, _StatusData) ->
    {CameFrom, CostSoFar, none};
search(_, {early_exit, Result}, _Start, _Goal, _Frontier, CameFrom, CostSoFar, _Obj, _CheckPassable, _CheckBlocking, _StatusData) ->
    {CameFrom, CostSoFar, Result};    
search(false, _EarlyExit, Start, Goal, Frontier1, CameFrom, CostSoFar, Obj, CheckPassable, CheckBlocking, StatusData) ->
    {{value, Current}, Frontier2} = pqueue2:out(Frontier1),
    %lager:info("Current: ~p Distance: ~p", [Current, map:distance(Current, Goal)]),
    NewStatusData = [{map:distance(Current, Goal), Current} | StatusData],

    {NewSearchData, NewResult} = 

        case Current =:= Goal of
            true -> 
                SearchData = {Frontier2, CameFrom, CostSoFar},
                Result = {early_exit, success},

                {SearchData, Result};
            false ->
                case length(NewStatusData) > 40 of
                    true ->
                        SortedStatusData = lists:sort(NewStatusData),
                        [{Dist, Closest} | _Rest] = SortedStatusData,
                        Result = {early_exit, {nearby, Dist, Closest}},
                        SearchData = {Frontier2, CameFrom, CostSoFar},

                        {SearchData, Result};
                    false ->
                        {X, Y} = Current,
                        Neighbours = get_neighbours(X, Y, Obj, CheckPassable, CheckBlocking),
                        SearchData = check_neighbours(Neighbours, 
                                                      Current, 
                                                      Goal, 
                                                      Frontier2, 
                                                      CameFrom, 
                                                      CostSoFar, 
                                                      Obj),
                        Result = {search, continue},

                        {SearchData, Result}
                end
          end,

    {NewFrontier, NewCameFrom, NewCostSoFar} = NewSearchData,

    search(pqueue2:is_empty(NewFrontier), NewResult, Start, Goal, NewFrontier, NewCameFrom, 
           NewCostSoFar, Obj, CheckPassable, CheckBlocking, NewStatusData).

check_neighbours([], _Current, _Goal, NewFrontier, NewCameFrom, NewCostSoFar, _Obj) ->
    {NewFrontier, NewCameFrom, NewCostSoFar};
check_neighbours([Neighbour | Rest], Current, Goal, Frontier, CameFrom, CostSoFar, Obj) ->
    NewCost = dict:fetch(Current, CostSoFar) + get_move_cost(Neighbour, Obj),
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
    check_neighbours(Rest, Current, Goal, NewFrontier, NewCameFrom, NewCostSoFar, Obj).

heuristic(Start, End) ->
    2 * map:distance(Start, End).

get_neighbours(X, Y, Obj, CheckPassable, CheckBlocking) ->
    Neighbours = map:neighbours(X, Y),

    F = fun(Pos) -> 
        is_passable(Pos, Obj, CheckPassable) and 
        is_not_blocked(Pos, Obj, CheckBlocking)
    end,

    lists:filter(F, Neighbours).

is_passable(_Pos, _Obj, false) -> true;
is_passable(Pos, Obj, true) -> map:is_passable(Pos, Obj).

is_not_blocked(_Pos, _Obj, false) -> true;
is_not_blocked(Pos, Obj, true) -> map:is_not_blocked(Pos, Obj).

get_move_cost(Pos, Obj) ->
    MoveCost = case obj:is_empty(Obj, Pos) of
                    true -> map:movement_cost(Pos);
                    false -> 10
               end,
    MoveCost.

to_path(Start, Next, _From, Path) when Start =:= Next ->
    [Start | Path];
to_path(Start, Next, From, Path) ->    
    NewNext = dict:fetch(Next, From),
    to_path(Start, NewNext, From, [Next | Path]).
