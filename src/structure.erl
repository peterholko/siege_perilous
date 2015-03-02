%% Author: Peter
%% Created: Feb, 2015
%% Description: Structure module
-module(structure).

-include("common.hrl").
-include("schema.hrl").

-export([get/1, start_build/4]).

get(Id) ->
    Structure = find(Id),
    Structure.

start_build(ObjId, GlobalPos, LocalPos, StructureName) ->
    lager:info("ObjId: ~p GlobalPos: ~p LocalPos: ~p StructureName: ~p", [ObjId, GlobalPos, LocalPos, StructureName]), 
    [StructureType] = find_type(name, StructureName),
    {Hp} = bson:lookup(hp, StructureType),

    Attrs = {owner, ObjId, name, StructureName, hp, Hp},
    NewStructure = mongo:insert(mdb:get_conn(), <<"structure">>, Attrs),
    lager:info("NewStructure: ~p", [NewStructure]),
    {StructureId} = bson:lookup('_id', NewStructure),    

    local:create(GlobalPos, StructureId, LocalPos, structure, StructureName, building).
%
% Internal functions
%



find(Id) ->
    Cursor = mongo:find(mdb:get_conn(), <<"structure">>, {'_id', Id}),
    Structure = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Structure.

find_type(Key, Value) ->
    Cursor = mongo:find(mdb:get_conn(), <<"structure_type">>, {Key, Value}),
    ItemTypes = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    ItemTypes.

