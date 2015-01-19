%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to unit data
-module(unit).

-include("schema.hrl").

-export([get/1, get_type/1, get_stats/1]).

get(Id) ->
    Unit = find(Id),
    Unit.

get_type(Id) ->
    [UnitType] = find_type(Id),
    UnitType.

get_stats(Id) ->
    Unit = find(Id),
    stats(Unit).
    
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

stats([]) ->
    false;

stats([Unit]) ->
    all_stats(Unit).

all_stats(Unit) ->
    {UnitTypeId} = bson:lookup(type, Unit),
    [UnitType] = find_type(UnitTypeId),
    bson:merge(Unit, UnitType).
