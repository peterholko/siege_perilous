%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to unit data
-module(obj).

-include("schema.hrl").

-export([get_obj/1, get_info/2, create/3, create/5]).

get_obj(Id) ->
    Obj = find(Id),
    Obj.

get_info(Requester, Id) ->
    ObjInfo = case find(Id) of
                  [Obj] ->
                      {PlayerId} = bson:lookup(player, Obj),
                      info(Requester == PlayerId, Obj);
                  _ ->
                      none
              end,
    lager:info("ObjInfo: ~p", [ObjInfo]),
    ObjInfo.

create(Player, Pos, Type) ->
    lager:info("Create: ~p ~p ~p", [Player, Pos, Type]),
    Id = insert(Player),

    map:create_obj(Id, Player, Pos, Type, none),

    perception:recalculate(),

    %Return ID
    Id.

create(Player, Pos, Type, State, Units) ->
    lager:info("Create: ~p ~p ~p ~p", [Player, Pos, Type, Units]),

    %Insert obj
    Id = insert(Player),

    %Insert units
    insert_units(Id, Units),

    %Create map obj
    map:create_obj(Id, Player, Pos, Type, State),

    perception:recalculate(),

    %Return ID
    Id.

%%% Internal only 

find(Id) ->
    Cursor = mongo:find(mdb:get_conn(), <<"obj">>, {'_id', Id}),
    Obj = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Obj.


info(_IsPlayers = false, Obj) ->
    Obj;
info(_IsPlayers = true, Obj) ->
    {ObjId} = bson:lookup('_id', Obj),
    Units = unit:get_units_and_stats(ObjId),
    lager:info("Units: ~p", [Units]),
    bson:update(units, Units, Obj).

insert(Player) ->
    Obj = {player, Player},
    [NewObj] = mongo:insert(mdb:get_conn(), <<"obj">>, [Obj]),
    {ObjId} = bson:lookup('_id', NewObj),
    ObjId.

insert_units(_ObjId, []) ->
    done;
insert_units(ObjId, [{UnitType, UnitSize} | Rest]) ->
    unit:create(ObjId, UnitType, UnitSize), 

    insert_units(ObjId, Rest).
