%% Author: Peter
%% Created: Feb, 2015
%% Description: Local map module
-module(local).

-include("common.hrl").
-include("schema.hrl").

-export([init_perception/3, enter_map/4, exit_map/1, create/6, move/2, update_state/2]).

init_perception(PlayerId, GlobalPos, _TileType) ->
    LocalPlayerUnits = db:index_read(local_obj, PlayerId, #local_obj.player),

    LocalExploredMap = map:get_local_explored(PlayerId, GlobalPos, all),
    LocalObjData = util:unique_list(get_visible_objs(LocalPlayerUnits, [], GlobalPos)),

    lager:info("LocalExploredMap: ~p", [LocalExploredMap]), 
    lager:info("LocalObjData: ~p", [LocalObjData]), 
    {LocalExploredMap, LocalObjData}.

enter_map(PlayerId, GlobalObjId, GlobalPos, LastPos) ->
    lager:info("Enter map: ~p", [{GlobalObjId, GlobalPos, LastPos}]),
    Units = unit:get_units(GlobalObjId), 
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

    db:write(LocalObj).    

create(GlobalPos, GlobalObjId, Pos, PlayerId, Class, Type) ->
    lager:info("Creating ~p", [Type]),
    Id = insert(GlobalObjId, Class, Type),

    LocalObj = #local_obj {id = Id,
                           global_obj_id = GlobalObjId,
                           global_pos = GlobalPos,
                           pos = Pos,
                           player = PlayerId,
                           class = Class,
                           type = Type,
                           state = none}, 
    db:write(LocalObj). 

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
    db:write(NewLocalObj).
 
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
    NearbyObjs = map:get_nearby_objs(Obj#local_obj.pos, {local_map, GlobalPos}, 3),
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

insert(GlobalObjId, unit, TypeName) ->
    [Unit] = unit:create(GlobalObjId, TypeName),
    {Id} = bson:lookup('_id', Unit),
    Id.
