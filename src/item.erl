%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to item data
-module(item).

-include("schema.hrl").

-export([get/1, get_info/1, get_by_owner/1, transfer/2, create/3, equip/1]).
-export([obj_perception/1]).

get(Id) ->
    Item = find('_id', Id),
    Item.

get_info(Id) when is_tuple(Id) ->
    [Item] = find('_id', Id),
    ItemInfo = info(Item),
    ItemInfo;

get_info(Name) when is_binary(Name) ->
    [ItemType] = find_type('name', Name),
    ItemType.

get_by_owner(OwnerId) ->
    Items = find(owner, OwnerId),
    Items.

transfer(ItemId, TargetId) ->
    mdb:update(<<"item">>, ItemId, {owner, TargetId}).

equip(ItemId) ->
    mdb:update(<<"item">>, ItemId, {equip, <<"true">>}).

create(Owner, TypeName, Quantity) ->
    % Find existing item type in owner
    ExistingItem = find(name, TypeName),
   
    case ExistingItem of
        [] ->
            NewItem = {owner, Owner, name, TypeName, quantity, Quantity},
            InsertedItem = mongo:insert(mdb:get_conn(), <<"item">>, NewItem),
            lager:info("InsertedItem: ~p", [InsertedItem]),
            InsertedItem;
        [Item] ->
            {ItemId} = bson:lookup('_id', Item),
            {OldQuantity} = bson:lookup(quantity, Item),
            UpdatedItem = {owner, Owner, name, TypeName, quantity, OldQuantity + Quantity},
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

stats([]) ->
    false;

stats([Item]) ->
    stats(Item);

stats(Item) ->
    {ItemName} = bson:lookup(name, Item),
    [ItemType] = find_type(name, ItemName),
    bson:merge(Item, ItemType).

info(Item) ->
    ItemStats = stats(Item),
    ItemStats.


