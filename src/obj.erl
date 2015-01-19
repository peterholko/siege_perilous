%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to unit data
-module(obj).

-include("schema.hrl").

-export([get_obj/1, get_units/1, get_obj_from_unit/1]).

get_obj(Id) ->
    Obj = find_obj(Id),
    Obj.

get_obj_from_unit(UnitId) ->
    BinId = util:hex_to_bin(binary_to_list(UnitId)),
    Cursor = mongo:find(mdb:get_conn(), <<"obj">>, {'units', {'$in', [{BinId}]}}),
    [Obj] = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Obj.

get_units(Id) ->
    [Obj] = find_obj(Id),
    lager:info("obj: ~p", [Obj]),
    {UnitIds} = bson:lookup(units, Obj),
    Units = units_perception(UnitIds, []),
    Units.

units_perception([], Units) ->
    Units;
units_perception([UnitId | Rest], Units) ->
    Unit = unit:get_unit_and_type(UnitId),
    units_perception(Rest, [Unit | Units]).


%%% Internal only 

find_obj(Id) ->
    Cursor = mongo:find(mdb:get_conn(), <<"obj">>, {'_id', Id}),
    Obj = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Obj.
