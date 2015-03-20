%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to unit data
-module(unit).

-include("schema.hrl").

-export([get/1, get_info/1, get_type/1, get_stats/1, get_units/1, get_units_and_stats/1]).
-export([create/2, remove/1]).

get(Id) ->
    Unit = find(Id),
    Unit.

get_info(Id) ->
    UnitInfo = case find(Id) of
                [Unit] ->
                    %TODO compare requester id to unit player id
                    info(Unit);
                _ ->
                    none
               end,
    UnitInfo.

get_units(ObjId) ->
    find_units(ObjId).

get_units_and_stats(ObjId) ->
    Units = find_units(ObjId),

    F = fun(Unit, UnitStats) ->
                UnitWithStats = stats(Unit),
                [UnitWithStats | UnitStats]
        end,
    lager:info("units: ~p", [Units]),
    lists:foldl(F, [], Units).

get_type(TypeName) ->
    [UnitType] = find_type_by_name(TypeName),
    UnitType.

get_stats(Id) ->
    Unit = find(Id),
    stats(Unit).

create(ObjId, TypeName) ->
    {UnitType} = find_type_by_name(TypeName),
    insert(ObjId, UnitType).

remove(UnitId) ->
    mdb:delete(<<"unit">>, UnitId).

%%Internal function
%%

find(Id) ->
    Cursor = mongo:find(mdb:get_conn(), <<"unit">>, {'_id', Id}),
    Unit = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Unit.

find_type(Id) ->
    Cursor = mongo:find(mdb:get_conn(), <<"unit_type">>, {'_id', Id}),
    UnitType = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    UnitType.

find_type_by_name(Name) ->
    mongo:find_one(mdb:get_conn(), <<"unit_type">>, {'name', Name}).

find_units(ObjId) ->
    lager:info("ObjId: ~p", [ObjId]), 
    Cursor = mongo:find(mdb:get_conn(), <<"unit">>, {'obj_id', ObjId}),
    Units = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    lager:info("find_units: ~p", [Units]),
    Units.

insert(ObjId, Type) ->
    {TypeName} = bson:lookup('name', Type),
    {BaseHp} = bson:lookup(base_hp, Type),
    Unit = {obj_id, ObjId, hp, BaseHp, type_name, TypeName},
    mongo:insert(mdb:get_conn(), <<"unit">>, [Unit]).

stats([]) ->
    false;

stats([Unit]) ->
    stats(Unit);

stats(Unit) ->
    {UnitTypeId} = bson:lookup(type, Unit),
    [UnitType] = find_type(UnitTypeId),
    bson:merge(Unit, UnitType).

info(Unit) ->
    UnitStats = stats(Unit),
    {UnitId} = bson:lookup('_id', UnitStats),
    Items = item:get_by_owner(UnitId),
    UnitItems = bson:update(items, Items, UnitStats),
    UnitItems.

