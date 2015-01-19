%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to unit data
-module(unit).

-include("schema.hrl").

-export([get_unit/1, get_unit_type/1, get_unit_and_type/1]).

get_unit(Id) ->
    Unit = find_unit(Id),
    Unit.

get_unit_type(Id) ->
    [UnitType] = find_unit_type(Id),
    UnitType.

get_unit_and_type(Id) ->
    [Unit] = unit:get_unit(Id),
    UnitData = type_from_unit(Unit),
    UnitData.

type_from_unit([]) ->
    false;

type_from_unit(Unit) ->
    {UnitTypeId} = bson:lookup(type, Unit),
    UnitType = get_unit_type(UnitTypeId),
    bson:merge(Unit, UnitType).

%%Internal function
%%

find_unit(Id) ->
    Cursor = mongo:find(mdb:get_conn(), <<"unit">>, {'_id', Id}),
    Unit = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Unit.

find_unit_type(Id) ->
    Cursor = mongo:find(mdb:get_conn(), <<"unit_type">>, {'_id', Id}),
    UnitType = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    UnitType.
