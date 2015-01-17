%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to unit data
-module(unit).

-include("schema.hrl").

-export([get_unit/1, get_unit_type/1, get_unit_and_type/1]).
-export([to_bin_id/1]).

get_unit(Id) ->
    BinId = to_bin_id(Id),
    Cursor = mongo:find(mdb:get_conn(), <<"unit">>, {'_id', BinId}),
    [Unit] = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Unit.

get_unit_type(Id) ->
    BinId = to_bin_id(Id),
    Cursor = mongo:find(mdb:get_conn(), <<"unit_type">>, {'_id', BinId}),
    [UnitType] = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    UnitType.

get_unit_and_type(Id) ->
    lager:info("Id: ~p", [Id]),
    Unit = get_unit(Id),
    {UnitTypeId} = bson:lookup(type, Unit),
    UnitType = get_unit_type(UnitTypeId),
    bson:merge(Unit, UnitType).

%%Internal function
%%

to_bin_id(Id) when is_tuple(Id) ->
    Id;
to_bin_id(BinId = <<_Id:92>>) ->
    {BinId};
to_bin_id(Id) when is_binary(Id) ->
    BinId = util:hex_to_bin(binary_to_list(Id)),
    {BinId}.
