%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to unit data
-module(unit).

-include("schema.hrl").

-export([get_unit/1, get_unit_type/1, get_unit_and_type/1]).

get_unit(Id) -> 
    Cursor = mongo:find(mdb:get_conn(), <<"unit">>, {'_id', Id}),
    [Unit] = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Unit.

get_unit_type(Id) ->
    Cursor = mongo:find(mdb:get_conn(), <<"unit_type">>, {'_id', Id}),
    [UnitType] = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    UnitType.

get_unit_and_type(Id) ->
    Unit = get_unit(Id),
    {UnitTypeId} = bson:lookup(type, Unit),
    UnitType = get_unit_type(UnitTypeId),
    bson:merge(Unit, UnitType).
