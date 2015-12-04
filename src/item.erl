%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to item data
-module(item).

-include("schema.hrl").

-export([get/1, get_by_name/1, get_by_owner/1, get_by_subclass/2, get_equiped/1, get_equiped_weapon/1]).
-export([transfer/2, split/2, update/2, create/1, create/3, equip/1]).
-export([obj_perception/1, find/1, find_type/2]).


get(Id) ->
    Item = find_one(<<"_id">>, Id),
    Item.

get_by_name(Name) ->
    Item = find_one(<<"name">>, Name),
    Item.

get_by_owner(OwnerId) ->
    Items = find(<<"owner">>, OwnerId),
    Items.

get_by_subclass(OwnerId, SubClass) ->
    Items = find({owner, OwnerId, subclass, SubClass}),
    Items.

get_equiped(OwnerId) ->
    Items = find({owner, OwnerId, equip, <<"true">>}),
    Items.

get_equiped_weapon(OwnerId) ->
    Items = find({owner, OwnerId, equip, <<"true">>, class, <<"weapon">>}),
    Items.

transfer(ItemId, TargetId) ->
    mdb:update(<<"item">>, ItemId, {owner, TargetId}).

split(Item, NewQuantity) ->
    lager:info("Item: ~p NewQuantity: ~p", [Item, NewQuantity]),
    ItemId = maps:get(<<"_id">>, Item),
    Quantity = maps:get(<<"quantity">>, Item),
    
    NewItem = maps:update(<<"quantity">>, NewQuantity, Item),
    NewItem2 = maps:remove(<<"_id">>, NewItem),
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

            NewItem = maps:remove(<<"_id">>, ItemType),
            NewItem2 = maps:put(<<"quantity">>, Quantity, NewItem),
            NewItem3 = maps:put(<<"owner">>, Owner, NewItem2),

            InsertedItem = mongo:insert(mdb:get_conn(), <<"item">>, NewItem3),
            lager:info("InsertedItem: ~p", [InsertedItem]),
            InsertedItem;
        [Item] ->
            lager:info("Updating existing item"),
            ItemId = maps:get(<<"_id">>, Item),
            OldQuantity = maps:get(<<"quantity">>, Item),
            UpdatedItem = maps:update(<<"quantity">>, OldQuantity + Quantity, Item),
            
            lager:info("UpdatedItem: ~p", [UpdatedItem]),
            mdb:update(<<"item">>, ItemId, UpdatedItem),
            lager:info("Finished updating item...", [UpdatedItem]),
            UpdatedItem;
        Items ->
            %Pick the first item of the same type and owner
            lager:info("Found multiple existing items, updating first existing item"),
            [Item | _Rest] = Items,            
            ItemId = maps:get(<<"_id">>, Item),
            OldQuantity = maps:get(<<"quantity">>, Item),
            UpdatedItem = maps:update(<<"quantity">>, OldQuantity + Quantity, Item),
            
            lager:info("UpdatedItem: ~p", [UpdatedItem]),
            mdb:update(<<"item">>, ItemId, UpdatedItem),
            UpdatedItem 
    end.

create(Item) ->
    InsertedItem = mongo:insert(mdb:get_conn(), <<"item">>, Item),
    InsertedItem.

obj_perception(ObjId) ->
    find(owner, ObjId).

% Internal

find_one(Key, Value) ->
    mdb:find_one(<<"item">>, Key, Value).

find(Key, Value) ->
    mdb:find(<<"item">>, Key, Value).

find(Tuple) ->
    mdb:find(<<"item">>, Tuple).

find_type(Key, Value) ->
    mdb:find(<<"item_type">>, Key, Value).
