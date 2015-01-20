%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to unit data
-module(obj).

-include("schema.hrl").

-export([get_obj/1, get_units/1, create_obj/5, unit_removed/1]).

get_obj(Id) ->
    Obj = find_obj(Id),
    Obj.

get_units(Id) ->
    [Obj] = find_obj(Id),
    {UnitIds} = bson:lookup(units, Obj),
    Units = units_perception(UnitIds, []),
    Units.

create_obj(Player, Pos, Type, State, Units) ->
    lager:info("create_obj"),

    Id = insert(Player, Units),
    map:create_obj(Id, Player, Pos, Type, State),

    perception:recalculate().

unit_removed(UnitId) ->
    Obj = find_from_unit(UnitId),

    {ObjId} = bson:lookup('_id', Obj),
    {Units} = bson:lookup(units, Obj),
    NewUnits = lists:delete(UnitId, Units),
    NewObj = bson:update(units, NewUnits, Obj),
    mongo:update(mdb:get_conn(), <<"obj">>, {'_id', ObjId}, NewObj),

    is_dead(ObjId, NewUnits).
    
%%% Internal only 

find_obj(Id) ->
    Cursor = mongo:find(mdb:get_conn(), <<"obj">>, {'_id', Id}),
    Obj = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Obj.

find_from_unit(UnitId) ->
    Cursor = mongo:find(mdb:get_conn(), <<"obj">>, {'units', {'$in', [UnitId]}}),
    [Obj] = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Obj.

insert(Player, Units) ->
    UnitIds = insert_units(Units, []),
    Obj = {player, Player, units, UnitIds},

    [NewObj] = mongo:insert(mdb:get_conn(), <<"obj">>, [Obj]),
    {NewObjId} = bson:lookup('_id', NewObj),
    NewObjId.

insert_units([], UnitIds) ->
    UnitIds;
insert_units([{UnitType, UnitSize} | Rest], UnitIds) ->
    [Unit] = unit:create(UnitType, UnitSize), 
    {UnitId} = bson:lookup('_id', Unit),
    NewUnitIds = [UnitId | UnitIds],

    insert_units(Rest, NewUnitIds).

units_perception([], Units) ->
    Units;
units_perception([UnitId | Rest], Units) ->
    Unit = unit:get_stats(UnitId),
    units_perception(Rest, [Unit | Units]).

is_dead(ObjId, []) ->
    map:update_obj_state(ObjId, dead);
is_dead(_Id, _Units) ->
    nothing.
