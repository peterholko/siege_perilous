%% Author: Peter
%% Created: Feb, 2015
%% Description: Local map module
-module(local).

-include("common.hrl").
-include("schema.hrl").

-export([init_perception/3, has_entered/2, has_entered/1, enter_map/4, exit_map/1, create/7, move/2, update_state/2]).
-export([is_exit_valid/1, is_empty/2]).

init_perception(PlayerId, GlobalPos, _TileType) ->
    LocalPlayerUnits = db:index_read(local_obj, PlayerId, #local_obj.player),

    LocalExploredMap = map:get_local_explored(PlayerId, GlobalPos, all),
    LocalObjData = util:unique_list(get_visible_objs(LocalPlayerUnits, [], GlobalPos)),

    lager:info("LocalExploredMap: ~p", [LocalExploredMap]), 
    lager:info("LocalObjData: ~p", [LocalObjData]), 
    {LocalExploredMap, LocalObjData}.

is_exit_valid(GlobalObjId) ->
    lager:info("is_exit_valid: ~p", [GlobalObjId]),
    LocalObjs = db:index_read(local_obj, GlobalObjId, #local_obj.global_obj_id),
    
    F = fun(LocalObj, ExitValid) ->
            lager:info("LocalObj: ~p ~p", [LocalObj, ExitValid]),
            OnEdge = is_on_edge(LocalObj#local_obj.pos),
            ExitValid and OnEdge
        end,
    
    lists:foldl(F, true, LocalObjs).

has_entered(GlobalObjId, GlobalPos) ->
    LocalObjs = db:index_read(local_obj, GlobalObjId, #local_obj.global_obj_id),
    lists:keymember(GlobalPos, #local_obj.global_pos, LocalObjs).

has_entered(GlobalObjId) ->
    case db:index_read(local_obj, GlobalObjId, #local_obj.global_obj_id) of
        [] ->
            false;
        _LocalObjs ->
            true
    end.

enter_map(PlayerId, GlobalObjId, GlobalPos, LastPos) ->
    lager:info("Enter map: ~p", [{GlobalObjId, GlobalPos, LastPos}]),
    Units = local_obj:units_from_obj(GlobalObjId),
    lager:info("Units from obj: ~p", [Units]), 
    EnterPos = get_enter_pos(GlobalPos, LastPos),
    map:add_local_explored(PlayerId, GlobalPos, EnterPos),
 
    F = fun(Unit) ->
                lager:info("enter_map unit: ~p", [Unit]),
                {Id} = bson:lookup('_id', Unit), 
                {TypeName} = bson:lookup(type_name, Unit),
                enter_obj(GlobalPos, GlobalObjId, Id, EnterPos, PlayerId, unit, TypeName, none)
        end,

    lists:foreach(F, Units).

exit_map(GlobalObjId) ->
    lager:info("Exit map: ~p", [GlobalObjId]),
    LocalObjs = db:index_read(local_obj, GlobalObjId, #local_obj.global_obj_id),

    %Remove any local objs owned by global obj id
    case LocalObjs of
        [] ->            
            false;
        LocalObjs ->
            remove_all_objs(LocalObjs),
            true
    end.

enter_obj(GlobalPos, GlobalObjId, Id, Pos, PlayerId, Class, Type, State) ->
    lager:info("Enter obj ~p", [Pos]),
    LocalObj = #local_obj {id = Id,
                           global_obj_id = GlobalObjId,
                           global_pos = GlobalPos,
                           pos = Pos,
                           player = PlayerId,
                           class = Class,
                           type = Type,
                           state = State},

    db:write(LocalObj),
    game:trigger_local(GlobalPos).

create(GlobalPos, GlobalObjId, Pos, PlayerId, Class, Type, State) ->
    lager:info("Creating ~p", [Type]),

    %Create mongo db local obj
    [LocalObjM] = local_obj:create(GlobalObjId, Type),
    {Id} = bson:lookup('_id', LocalObjM),

    %Create mnesia local obj
    LocalObj = #local_obj {id = Id,
                           global_obj_id = GlobalObjId,
                           global_pos = GlobalPos,
                           pos = Pos,
                           player = PlayerId,
                           class = Class,
                           type = Type,
                           state = State}, 
    db:write(LocalObj),
    game:trigger_local(GlobalPos),

    %Return ID
    Id.

move(Id, Pos) ->
    %TODO convert to transaction
    lager:info("Move local obj ~p ~p", [Id, Pos]),
    [LocalObj] = db:read(local_obj, Id),
    NewLocalObj = LocalObj#local_obj {pos = Pos,
                                      state = none},
    db:write(NewLocalObj).

update_state(Id, State) ->
    lager:info("Update state: ~p ~p", [Id, State]),
    %TODO make transaction
    [LocalObj] = db:read(local_obj, Id),
    NewLocalObj = LocalObj#local_obj {state = State},
    db:write(NewLocalObj),

    %Trigger new perception
    game:trigger_local(LocalObj#local_obj.global_pos).
 
is_empty(GlobalPos, LocalPos) ->
    LocalObjs = db:index_read(local_obj, GlobalPos, #local_obj.global_pos),
    Units = filter_units(LocalObjs),
    Result = not lists:keymember(LocalPos, #local_obj.pos, Units),
    Result.

%
% Internal functions
%

remove_obj(LocalObj) ->
    lager:info("Removing local obj: ~p", [LocalObj]),
    db:delete(local_obj, LocalObj#local_obj.id).

remove_all_objs([]) ->
    done;
remove_all_objs([LocalObj | Rest]) ->
    remove_obj(LocalObj),
    remove_all_objs(Rest).

get_visible_objs([], Objs, _GlobalPos) ->
    Objs;
get_visible_objs([Obj | Rest], Objs, GlobalPos) ->
    NearbyObjs = map:get_nearby_objs(Obj#local_obj.pos, {local_map, GlobalPos}, 4),
    NewObjs = Objs ++ NearbyObjs,
    get_visible_objs(Rest, NewObjs, GlobalPos).

get_enter_pos(_Pos, none) ->
    {0,0};
get_enter_pos(Pos, LastPos) ->
    CubePos = map:odd_q_to_cube(Pos),
    LastCubePos = map:odd_q_to_cube(LastPos),
    {X, Y, Z} = CubePos,
    {LX, LY, LZ} = LastCubePos,    

    DiffX = X - LX,
    DiffY = Y - LY,
    DiffZ = Z - LZ,

    lager:info("Diff: ~p ~p ~p", [DiffX, DiffY, DiffZ]),

    Direction = get_direction(DiffX, DiffY, DiffZ),
    enter_pos(Direction).

get_direction(-1, 1, 0) -> se;
get_direction(0, 1, -1) -> s;
get_direction(1, 0, -1) -> sw;
get_direction(-1, 0, 1) -> ne;
get_direction(0, -1, 1) -> n;
get_direction(1, -1, 0) -> nw;
get_direction(_, _, _) -> nw.

enter_pos(nw) -> {0,0};
enter_pos(n) -> {8,0};
enter_pos(ne) -> {16,0};
enter_pos(sw) -> {0,12};
enter_pos(s) -> {8,12};
enter_pos(se) -> {16,12}. 

is_on_edge({X, _Y}) when X =:= 0 -> true;
is_on_edge({_X, Y}) when Y =:= 0 -> true;
is_on_edge({X, Y}) when X =:= 0, Y =:= 0 -> true;
is_on_edge({_X, _Y}) -> false.

filter_units(LocalObjs) ->
    F = fun(LocalObj) -> LocalObj#local_obj.class =:= unit end,
    lists:filter(F, LocalObjs).
