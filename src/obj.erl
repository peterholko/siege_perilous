%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to unit data
-module(obj).

-include("schema.hrl").

-export([get_obj/1, get_info/2, create/6, remove/1]).
-export([get_obj_by_pos/1, get_map_obj/1, move/2, update_state/2]).

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

create(Player, Pos, Class, Type, State, Units) ->
    lager:info("Create: ~p ~p ~p ~p", [Player, Pos, Type, Units]),

    %Insert obj
    Id = insert(Player),

    %Insert units
    insert_units(Id, Units),

    %Create new map obj
    NewObj = #obj {id = Id,
                   player = Player,
                   pos = Pos,
                   class = Class,
                   type = Type,
                   state = State},

    db:write(NewObj),

    %game:set_perception(true),
    %Return ID
    Id.

remove(ObjId) ->
    db:delete(obj, ObjId),
    mdb:delete(<<"obj">>, ObjId).

%%% Map obj interface
get_obj_by_pos(Pos) ->
    db:index_read(obj, Pos, #obj.pos).

get_map_obj(Id) ->
    [Obj] = db:read(obj, Id),
    Obj.

move(Id, Pos) ->
    %TODO convert to transaction
    [Obj] = mnesia:dirty_read(obj, Id),
    LastPos = Obj#obj.pos,
    NewObj = Obj#obj {pos = Pos,
                      last_pos = LastPos,
                      state = none},
    mnesia:dirty_write(NewObj).

update_state(Obj, State) when is_record(Obj, obj) ->
    NewObj = Obj#obj { state = State},
    db:write(NewObj);

update_state(ObjId, State) ->
    Obj = get_map_obj(ObjId),
    update_state(Obj, State).

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
    Units = local_obj:units_stats_from_obj(ObjId),
    Items = item:get_by_owner(ObjId),
    ObjUnits = bson:update(units, Units, Obj),
    ObjUnitsItems = bson:update(items, Items, ObjUnits),
    ObjUnitsItems.

insert(Player) ->
    Obj = {player, Player},
    [NewObj] = mongo:insert(mdb:get_conn(), <<"obj">>, [Obj]),
    {ObjId} = bson:lookup('_id', NewObj),
    ObjId.

insert_units(_ObjId, []) ->
    done;
insert_units(ObjId, [{UnitType, UnitSize} | Rest]) ->
    local_obj:create(ObjId, UnitType, UnitSize), 

    insert_units(ObjId, Rest).
