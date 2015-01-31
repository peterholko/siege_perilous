%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to item data
-module(item).

-include("schema.hrl").

-export([get/1, get_by_owner/1, transfer/2, create/3]).
-export([obj_perception/1]).

get(Id) ->
    Item = find('_id', Id),
    Item.

get_by_owner(OwnerId) ->
    Items = find(owner, OwnerId),
    Items.

transfer(Item, TargetId) ->
    ItemId = bson:lookup('_id', Item),
    mdb:update(<<"item">>, ItemId, {owner, TargetId}).

create(Owner, Type, Quantity) ->
    % Find existing item type in owner
    Items = find(type, Type),
    
    case Items of
        [] ->
            Attrs = {owner, Owner, type, Type, quantity, Quantity},
            mongo:insert(mdb:get_conn(), <<"item">>, Attrs);
        [Item] ->
            {ItemId} = bson:lookup('_id', Item),
            {OldQuantity} = bson:lookup(quantity, Item),

            Attrs = {owner, Owner, type, Type, quantity, Quantity + OldQuantity},
            mdb:update(<<"item">>, ItemId, Attrs)
    end.

obj_perception(ObjId) ->
    find(owner, ObjId).

% Internal

find(Key, Value) ->
    Cursor = mongo:find(mdb:get_conn(), <<"item">>, {Key, Value}),
    Items = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Items.
