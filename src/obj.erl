%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to unit data
-module(obj).

-include("schema.hrl").

-export([get_obj/1, create/5]).

get_obj(Id) ->
    Obj = find(Id),
    Obj.

create(Player, Pos, Type, State, Units) ->
    lager:info("create_obj"),

    Id = insert(Player, Units),
    map:create_obj(Id, Player, Pos, Type, State),

    perception:recalculate().

%%% Internal only 

find(Id) ->
    Cursor = mongo:find(mdb:get_conn(), <<"obj">>, {'_id', Id}),
    Obj = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Obj.

insert(Player, Units) ->
    Obj = {player, Player},
    [NewObj] = mongo:insert(mdb:get_conn(), <<"obj">>, [Obj]),
    {ObjId} = bson:lookup('_id', NewObj),

    %Insert units
    insert_units(ObjId, Units),

    ObjId.

insert_units(_ObjId, []) ->
    done;
insert_units(ObjId, [{UnitType, UnitSize} | Rest]) ->
    unit:create(ObjId, UnitType, UnitSize), 

    insert_units(ObjId, Rest).
