-module(astar).

-type cnode() :: {integer(), integer()}.

-define(MINX, 0).
-define(MINY, 0).
-define(MAXX, 3).
-define(MAXY, 3).

-export([
         astar/2,
         neighbour_nodes/2
        ]).

%% @doc Performs A* for finding a path from `Start' node to `Goal' node
-spec astar(cnode(), cnode()) -> list(cnode()) | failure.
astar(Start, Goal) ->
    ClosedSet = sets:new(),
    OpenSet   = sets:add_element(Start, sets:new()),

    Fscore    = dict:store(Start, h_score(Start, Goal), dict:new()),
    Gscore    = dict:store(Start, 0, dict:new()),

    CameFrom  = dict:store(Start, none, dict:new()),

    astar_step(Goal, ClosedSet, OpenSet, Fscore, Gscore, CameFrom).

%% @doc Performs a step of A*.
%% Takes the best element from `OpenSet', evaluates neighbours, updates scores, etc..
-spec astar_step(cnode(), sets:set(), sets:set(), dict:dict(), dict:dict(), dict:dict()) -> list(cnode()) | failure.
astar_step(Goal, ClosedSet, OpenSet, Fscore, Gscore, CameFrom) ->
    case sets:size(OpenSet) of
        0 ->
            failure;
        _ ->
            BestStep = best_step(sets:to_list(OpenSet), Fscore, none, infinity),
            if
                Goal == BestStep ->
                    lists:reverse(reconstruct_path(CameFrom, BestStep));
                true ->
                    Parent     = dict:fetch(BestStep, CameFrom),
                    NextOpen   = sets:del_element(BestStep, OpenSet),
                    NextClosed = sets:add_element(BestStep, ClosedSet),
                    Neighbours = neighbour_nodes(BestStep, Parent),

                    {NewOpen, NewF, NewG, NewFrom} = scan(Goal, BestStep, Neighbours, NextOpen, NextClosed, Fscore, Gscore, CameFrom),
                    astar_step(Goal, NextClosed, NewOpen, NewF, NewG, NewFrom)
            end
    end.

%% @doc Returns the heuristic score from `Current' node to `Goal' node
-spec h_score(Current :: cnode(), Goal :: cnode()) -> Hscore :: number().
h_score(Current, Goal) ->
    dist_between(Current, Goal).

%% @doc Returns the distance from `Current' node to `Goal' node
-spec dist_between(cnode(), cnode()) -> Distance :: number().
dist_between(Current, Goal) ->
    map:distance(Current, Goal).

%% @doc Returns the best next step from `OpenSetAsList'
%% TODO: May be optimized by making OpenSet an ordered set.
-spec best_step(OpenSetAsList :: list(cnode()), Fscore :: dict(), BestNodeTillNow :: cnode() | none, BestCostTillNow :: number() | infinity) -> cnode().
best_step([H|Open], Score, none, infinity) ->
    V = dict:fetch(H, Score),
    best_step(Open, Score, H, V);

best_step([], _Score, Best, _BestValue) ->
    Best;

best_step([H|Open], Score, Best, BestValue) ->
    Value = dict:fetch(H, Score),
    case Value < BestValue of
        true ->
            best_step(Open, Score, H, Value);
        false ->
            best_step(Open, Score, Best, BestValue)
    end.

%% @doc Returns the neighbour nodes of `Node', and excluding its `Parent'.
-spec neighbour_nodes(cnode(), cnode() | none) -> list(cnode()).
neighbour_nodes(Node, Parent) ->
    {X, Y} = Node,
    Neighbours = map:neighbours(X,Y),
    %Remove parent
    lists:delete(Parent, Neighbours).

%% @doc Scans the `Neighbours' of `BestStep', and adds/updates the Scores and CameFrom dicts accordingly.
-spec scan(
        Goal :: cnode(),
        BestStep :: cnode(),
        Neighbours :: list(cnode()),
        NextOpen :: set(),
        NextClosed :: set(),
        Fscore :: dict(),
        Gscore :: dict(),
        CameFrom :: dict()
       ) ->
    {NewOpen :: set(), NewF :: dict(), NewG :: dict(), NewFrom :: dict()}.
scan(_Goal, _X, [], Open, _Closed, F, G, From) ->
    {Open, F, G, From};
scan(Goal, X, [Y|N], Open, Closed, F, G, From) ->
    case sets:is_element(Y, Closed) of
        true ->
            scan(Goal, X, N, Open, Closed, F, G, From);
        false ->
            G0 = dict:fetch(X, G),
            TrialG = G0 + dist_between(X, Y),
            case sets:is_element(Y, Open) of
                true ->
                    OldG = dict:fetch(Y, G),
                    case TrialG < OldG of
                        true ->
                            NewFrom = dict:store(Y, X, From),
                            NewG    = dict:store(Y, TrialG, G),
                            NewF    = dict:store(Y, TrialG + h_score(Y, Goal), F), % Estimated total distance from start to goal through y.
                            scan(Goal, X, N, Open, Closed, NewF, NewG, NewFrom);
                        false ->
                            scan(Goal, X, N, Open, Closed, F, G, From)
                    end;
                false ->
                    NewOpen = sets:add_element(Y, Open),
                    NewFrom = dict:store(Y, X, From),
                    NewG    = dict:store(Y, TrialG, G),
                    NewF    = dict:store(Y, TrialG + h_score(Y, Goal), F), % Estimated total distance from start to goal through y.
                    scan(Goal, X, N, NewOpen, Closed, NewF, NewG, NewFrom)
            end
    end.

%% @doc Reconstructs the calculated path using the `CameFrom' dict
-spec reconstruct_path(dict(), cnode()) -> list(cnode()).
reconstruct_path(CameFrom, Node) ->
    case dict:fetch(Node, CameFrom) of
        none ->
            [Node];
        Value ->
            [Node | reconstruct_path(CameFrom, Value)]
    end.

