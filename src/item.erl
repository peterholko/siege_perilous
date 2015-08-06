%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to item data
-module(item).

-include("schema.hrl").

-export([get/1, get_by_name/1, get_type/1, get_by_owner/1, transfer/2, split/2, update/2, create/3, equip/1]).
-export([obj_perception/1, find/1, find_type/2]).


get(Id) ->
    Item = find('_id', Id),
    Item.

get_by_name(Name) ->
    Item = find(name, Name),
    Item.

get_type(Name) ->
    [ItemType] = find_type(name, Name),
    ItemType.

get_by_owner(OwnerId) ->
    Items = find(owner, OwnerId),
    Items.

transfer(ItemId, TargetId) ->
    mdb:update(<<"item">>, ItemId, {owner, TargetId}).

split(Item, NewQuantity) ->
    lager:info("Item: ~p NewQuantity: ~p", [Item, NewQuantity]),
    {ItemId} = bson:lookup('_id', Item),
    {Quantity} = bson:lookup(quantity, Item),
    
    NewItem = bson:update(quantity, NewQuantity, Item),
    NewItem2 = bson:exclude(['_id'], NewItem),
    mongo:insert(mdb:get_conn(), <<"item">>, NewItem2),

    lager:info("Updating original item quantity"),
    update(ItemId, Quantity - NewQuantity).

equip(ItemId) ->
    mdb:update(<<"item">>, ItemId, {equip, <<"true">>}).


update(ItemId, 0) ->
    mdb:delete(<<"item">>, ItemId);
update(ItemId, NewQuantity) ->
    mdb:update(<<"item">>, ItemId, {quantity, NewQuantity}).

create(Owner, Name, Quantity) ->
    % Find existing item type in owner
    ExistingItem = find({name, Name, owner, Owner}),
    lager:info("ExistingItem: ~p", [ExistingItem]),
    case ExistingItem of
        [] ->
            [ItemType] = find_type(name, Name),
            NewItem = bson:exclude(['_id'], ItemType),
            NewItem2 = bson:merge({quantity, Quantity}, NewItem),
            NewItem3 = bson:merge({owner, Owner}, NewItem2),
            InsertedItem = mongo:insert(mdb:get_conn(), <<"item">>, NewItem3),
            lager:info("InsertedItem: ~p", [InsertedItem]),
            InsertedItem;
        [Item] ->
            lager:info("Updating existing item"),
            {ItemId} = bson:lookup('_id', Item),
            {OldQuantity} = bson:lookup(quantity, Item),
            UpdatedItem = bson:update(quantity, OldQuantity + Quantity, Item),
            
            lager:info("UpdatedItem: ~p", [UpdatedItem]),
            mdb:update(<<"item">>, ItemId, UpdatedItem),
            UpdatedItem;
        Items ->
            %Pick the first item of the same type and owner
            lager:info("Found multiple existing items, updating first existing item"),
            [Item | _Rest] = Items,
            {ItemId} = bson:lookup('_id', Item),
            {OldQuantity} = bson:lookup(quantity, Item),
            UpdatedItem = bson:update(quantity, OldQuantity + Quantity, Item),
            lager:info("UpdatedItem: ~p", [UpdatedItem]),
            mdb:update(<<"item">>, ItemId, UpdatedItem),
            UpdatedItem 
    end.

obj_perception(ObjId) ->
    find(owner, ObjId).

% Internal

find(Key, Value) ->
    Cursor = mongo:find(mdb:get_conn(), <<"item">>, {Key, Value}),
    Items = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Items.

find_type(Key, Value) ->
    Cursor = mongo:find(mdb:get_conn(), <<"item_type">>, {Key, Value}),
    ItemTypes = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    ItemTypes.

find(Tuple) ->
    Cursor = mongo:find(mdb:get_conn(), <<"item">>, Tuple),
    Items = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Items.

