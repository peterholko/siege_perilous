%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to item data
-module(item).

-include("schema.hrl").

-export([get/1, get_by_owner/1, transfer/2, create/3]).

get(Id) ->
    Item = find(Id),
    Item.

get_by_owner(OwnerId) ->
    Items = find_by_owner(OwnerId),
    Items.

transfer(Item, TargetId) ->
    ItemId = bson:lookup('_id', Item),
    mdb:update(<<"item">>, ItemId, {owner, TargetId}).

create(Owner, Type, Quantity) ->
    mongo:insert(mdb:get_conn(), <<"item">>, {owner, Owner, type, Type, quantity, Quantity}).

% Internal

find(Id) ->
    Cursor = mongo:find(mdb:get_conn(), <<"item">>, {'_id', Id}),
    Item = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Item.

find_by_owner(OwnerId) ->
    Cursor = mongo:find(mdb:get_conn(), <<"item">>, {owner, OwnerId}),
    Items = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Items.
