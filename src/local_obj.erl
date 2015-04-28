%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to local obj mongodb data
-module(local_obj).

-include("schema.hrl").

-export([get/1, get_info/1, get_type/1, get_stats/1, units_from_obj/1, units_stats_from_obj/1]).
-export([create/2, update/3, remove/1]).
-export([find_type/1]).

get(Id) ->
    LocalObj = find(Id),
    LocalObj.

get_info(Id) ->
    LocalObjInfo = case find(Id) of
                [LocalObj] ->
                    %TODO compare requester id to unit player id
                    info(LocalObj);
                _ ->
                    none
               end,
    LocalObjInfo.

units_from_obj(ObjId) ->
    find_units(ObjId).

units_stats_from_obj(ObjId) ->
    Units = find_units(ObjId),

    F = fun(Unit, UnitStats) ->
                UnitWithStats = stats(Unit),
                [UnitWithStats | UnitStats]
        end,
    lager:info("units: ~p", [Units]),
    lists:foldl(F, [], Units).

get_type(TypeName) ->
    {LocalObjType} = find_type(TypeName),
    LocalObjType.

get_stats(Id) ->
    LocalObj = find(Id),
    stats(LocalObj).

create(ObjId, TypeName) ->
    {LocalObjType} = find_type(TypeName),
    insert(ObjId, LocalObjType).

update(Id, Attr, Val) ->
    mdb:update(<<"local_obj">>, Id, {Attr, Val}).

remove(Id) ->
    mdb:delete(<<"local_obj">>, Id).

%%Internal function
%%

find(Id) ->
    Cursor = mongo:find(mdb:get_conn(), <<"local_obj">>, {'_id', Id}),
    LocalObj = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    LocalObj.

find_type(Name) ->
    mongo:find_one(mdb:get_conn(), <<"local_obj_type">>, {'name', Name}).

find_units(ObjId) ->
    lager:info("ObjId: ~p", [ObjId]), 
    Cursor = mongo:find(mdb:get_conn(), <<"local_obj">>, {'obj_id', ObjId, 'class', <<"unit">>}),
    Units = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    lager:info("find_units: ~p", [Units]),
    Units.

insert(ObjId, Type) ->
    {TypeName} = bson:lookup('name', Type),
    {BaseHp} = bson:lookup(base_hp, Type),
    Unit = {obj_id, ObjId, hp, BaseHp, type_name, TypeName},
    mongo:insert(mdb:get_conn(), <<"local_obj">>, [Unit]).

stats([]) ->
    false;

stats([LocalObj]) ->
    stats(LocalObj);

stats(LocalObj) ->
    {TypeName} = bson:lookup(type_name, LocalObj),
    {LocalObjType} = find_type(TypeName),
    bson:merge(LocalObj, LocalObjType).

info(LocalObjM) ->
    LocalObjStats = stats(LocalObjM),
    {Id} = bson:lookup('_id', LocalObjStats),
    Items = item:get_by_owner(Id),

    %Get state from local obj table
    [LocalObj] = db:read(local_obj, Id),

    LocalObjStats2 = bson:update(items, Items, LocalObjStats),
    LocalObjStats3 = bson:update(state, LocalObj#local_obj.state, LocalObjStats2),
    LocalObjStats3.
