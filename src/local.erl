%% Author: Peter
%% Created: Feb, 2015
%% Description: Local map module
-module(local).

-include("common.hrl").
-include("schema.hrl").

-export([init_perception/2, enter_map/3, exit_map/1, create/7, update_state/2]).

init_perception(Pos, TileType) ->
    LocalObjList = db:read(local_obj, Pos),

    LocalMap = get_map(TileType),
    LocalObjData = get_obj_data(LocalObjList, []),
    {LocalMap, LocalObjData}.

enter_map(GlobalObjId, GlobalPos, LastPos) ->
    lager:info("Enter map: ~p", [{GlobalObjId, GlobalPos, LastPos}]),
    Units = unit:get_units(GlobalObjId), 
    EnterPos = get_enter_pos(GlobalPos, LastPos),

    F = fun(Unit) ->
                lager:info("enter_map unit: ~p", [Unit]),
                {Id} = bson:lookup('_id', Unit), 
                {TypeName} = bson:lookup(type_name, Unit),
                create(GlobalPos, GlobalObjId, Id, EnterPos, unit, TypeName, none)
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

create(GlobalPos, GlobalObjId, Id, Pos, Class, Type, State) ->
    lager:info("Creating local obj"),
    LocalObj = #local_obj {global_pos = GlobalPos,
                           global_obj_id = GlobalObjId,
                           id = Id,
                           pos = Pos,
                           class = Class,
                           type = Type,
                           state = State},

    db:write(LocalObj).    

update_state(Id, State) ->
    lager:info("Update state: ~p ~p", [Id, State]),
    %TODO make transaction
    [LocalObj] = db:index_read(local_obj, Id, #local_obj.id),
    NewLocalObj = LocalObj#local_obj {state = State},
    db:write(NewLocalObj).
 
%
% Internal functions
%

remove_obj(LocalObj) ->
    lager:info("Removing local obj: ~p", [LocalObj]),
    db:delete(local_obj, LocalObj#local_obj.global_pos).

remove_all_objs([]) ->
    done;
remove_all_objs([LocalObj | Rest]) ->
    remove_obj(LocalObj),
    remove_all_objs(Rest).

get_map(TileType) ->
    LocalMap = db:read(local_map, TileType),

    F = fun(TileData, MsgTiles) ->
            {local_map, _LocalType, Pos, Type} = TileData,
            {X, Y} = Pos,
            [#{<<"x">> => X,
               <<"y">> => Y,
               <<"t">> => Type} | MsgTiles]
        end,

    lists:foldl(F, [], LocalMap).

get_obj_data([], ObjData) ->
    ObjData;
get_obj_data([Obj | Rest], ObjData) ->
    {X, Y} = Obj#local_obj.pos,
    NewObjData = [ #{<<"global_id">> => Obj#local_obj.global_obj_id,
                     <<"id">> => Obj#local_obj.id,
                     <<"x">> => X,
                     <<"y">> => Y,
                     <<"player">> => Obj#local_obj.player,
                     <<"class">> => Obj#local_obj.class,                   
                     <<"type">> => Obj#local_obj.type,
                     <<"state">> => Obj#local_obj.state} | ObjData],

    get_obj_data(Rest, NewObjData).

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
    
