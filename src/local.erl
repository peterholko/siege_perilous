%% Author: Peter
%% Created: Feb, 2015
%% Description: Local map module
-module(local).

-include("common.hrl").
-include("schema.hrl").

-export([init_perception/2, enter_map/2, exit_map/2, create/7, update_state/2]).

init_perception(Pos, TileType) ->
    LocalObjList = db:read(local_obj, Pos),

    LocalMap = get_map(TileType),
    LocalObjData = get_obj_data(LocalObjList, []),
    {LocalMap, LocalObjData}.

enter_map(GlobalObjId, GlobalPos) ->
    Units = unit:get_units(GlobalObjId), 

    F = fun(Unit) ->
                lager:info("enter_map: ~p", [Unit]),
                {Id} = bson:lookup('_id', Unit), 
                {TypeName} = bson:lookup(type_name, Unit),
                create(GlobalPos, GlobalObjId, Id, {0,0}, unit, TypeName, none)
        end,

    lists:foreach(F, Units).

exit_map(GlobalObjId, GlobalPos) ->
    LocalObjs = db:read(local_obj, GlobalPos),
    Result = lists:keymember(GlobalObjId, #local_obj.global_obj_id, LocalObjs),

    %Remove any local objs owned by global obj id
    case Result of
        true ->
            remove_all_objs(LocalObjs);
        false ->
            nothing
    end,

    %Return result 
    Result.

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
    NewObjData = [ #{<<"id">> => Obj#local_obj.id,
                     <<"x">> => X,
                     <<"y">> => Y,
                     <<"type">> => Obj#local_obj.type,
                     <<"state">> => Obj#local_obj.state} | ObjData],

    get_obj_data(Rest, NewObjData).


