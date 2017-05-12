%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to item data
-module(item).

-include("schema.hrl").

-export([id/1]).
-export([get_rec/1, get_map/1, get_all_attr/1, get_map_by_name/1, get_by_owner/1, get_by_owner_rec/1, 
         get_by_subclass/2, get_by_name/2, get_equiped/1, get_non_equiped/1, get_equiped_weapon/1,
         get_weapon_range/1]).
-export([transfer/2, transfer/3, split/2, update/2, create/1, create/3, equip/1, unequip/1]).
-export([is_equipable/1, is_slot_free/2, is_player_owned/2, is_valid_split/3, is_subclass/2]).
-export([get_total_weight/1, weight/2]).
-export([match_req/3]).

id(Item) -> maps:get(<<"id">>, Item).

get_rec(Id) ->
    case db:read(item, Id) of
        [Item] -> Item;
        _ -> invalid
    end.

get_map(Id) ->
    Item = get_rec(Id),
    rec_to_map(Item).

get_all_attr(Id) ->
    case get_rec(Id) of
        invalid -> invalid;
        ItemRec -> all_attr_map(ItemRec)
    end.

get_map_by_name(Name) ->
    item_def:all_to_map(Name).

get_by_owner(OwnerId) ->
    Items = db:index_read(item, OwnerId, #item.owner),

    F = fun(Item) ->            
            all_attr_map(Item)
        end,

    lists:map(F, Items).

get_by_owner_rec(OwnerId) ->
    Items = db:index_read(item, OwnerId, #item.owner),
    Items.

get_by_subclass(OwnerId, SubClass) ->
    AllItems = get_by_owner(OwnerId),
    F = fun(ItemMap) -> maps:get(<<"subclass">>, ItemMap) =:= SubClass end,
    lists:filter(F, AllItems).

get_by_name(OwnerId, Name) ->
    AllItems = get_by_owner(OwnerId),
    F = fun(ItemMap) -> maps:get(<<"name">>, ItemMap) =:= Name end,
    lists:filter(F, AllItems).

get_equiped(OwnerId) ->
    AllItems = get_by_owner(OwnerId),
    F = fun(ItemMap) -> maps:get(<<"equip">>, ItemMap) =:= <<"true">> end,
    lists:filter(F, AllItems).

get_non_equiped(OwnerId) ->
    AllItems = get_by_owner(OwnerId),
    F = fun(ItemMap) -> maps:get(<<"equip">>, ItemMap) =:= <<"false">> end,
    lists:filter(F, AllItems).

get_equiped_weapon(OwnerId) ->
    AllItems = get_by_owner(OwnerId),
    F = fun(ItemMap) -> 
                (maps:get(<<"equip">>, ItemMap) =:= <<"true">>) and
                (maps:get(<<"class">>, ItemMap) =:= <<"Weapon">>)
        end,
    lists:filter(F, AllItems).

get_weapon_range(OwnerId) ->
    Weapons = get_equiped_weapon(OwnerId),

    %TODO fix for dual wield
    [Weapon | _] = Weapons,

    maps:get(<<"range">>, Weapon).

get_total_weight(ObjId) ->
    AllItems = db:dirty_index_read(item, ObjId, #item.owner),

    F = fun(Item, AccWeight) ->
            ItemWeight = Item#item.quantity * Item#item.weight,
            ItemWeight + AccWeight
        end,

    TotalWeight = lists:foldl(F, 0, AllItems),
    TotalWeight.   

is_subclass(ItemName, Subclass) ->
    item_def:value(ItemName, <<"subclass">>) =:= Subclass.

is_equipable(Item) ->
    case Item#item.class of
        <<"Weapon">> -> true;
        <<"Armor">> -> true;
        _ -> false
    end.

is_slot_free(OwnerId, Slot) ->
    AllItems = get_by_owner(OwnerId),
    F = fun(ItemMap) -> 
                (maps:get(<<"equip">>, ItemMap) =:= <<"true">>) and
                (maps:get(<<"slot">>, ItemMap) =:= Slot)
        end,

    ItemsInSlot = lists:filter(F, AllItems),
    ItemsInSlot =:= [].

is_player_owned(Player, ItemId) ->
    case db:read(item, ItemId) of
        [Item] -> Item#item.owner =:= Player;
        [] -> false
    end.

is_valid_split(Player, ItemId, Quantity) when Quantity > 0 ->
    case db:read(item, ItemId) of
        [Item] -> 
            [Owner] = db:read(obj, Item#item.owner),
            (Item#item.quantity > Quantity) and (Owner#obj.player =:= Player);
        [] -> false
    end;
is_valid_split(_, _, _) -> false.

weight(ItemName, ItemQuantity) ->
    ItemWeight = item_def:value(ItemName, <<"weight">>),
    ItemWeight * ItemQuantity.

can_merge(ItemClass) ->
    case ItemClass of
        <<"Weapon">> -> false;
        <<"Armor">> -> false;
        _ -> true
    end.

transfer(TransferItemId, TargetId) ->
    [TransferItem] = db:dirty_read(item, TransferItemId),
    AllItems = db:dirty_index_read(item, TargetId, #item.owner),
    
    NewItem = case can_merge(TransferItem#item.class) of
                  true ->
                      case filter_by_name(AllItems, TransferItem#item.name) of
                          [] -> 
                              TransferItem#item{owner = TargetId};
                          [Item | _Rest] -> 
                              %Update new quantity,
                              NewQuantity = Item#item.quantity + TransferItem#item.quantity,
                                
                              %Remove the transfer item
                              db:delete(item, TransferItemId),

                              %Merge with existing item
                              Item#item{owner = TargetId,
                                        quantity = NewQuantity}
                      end;
                  false ->
                      TransferItem#item{owner = TargetId}
             end,
    db:write(NewItem),

    %Return item map with all the attributes
    all_attr_map(NewItem).
                
transfer(ItemId, TargetId, Quantity) ->
    NewItem = split(ItemId, Quantity),
    transfer(NewItem#item.id, TargetId).

split(ItemId, NewQuantity) ->
    [Item] = db:read(item, ItemId),
    CurrentQuantity = Item#item.quantity,
    NewId = util:get_id(),

    SourceItem = Item#item{quantity = NewQuantity},
    NewItem = Item#item{id = NewId,
                        quantity = CurrentQuantity - NewQuantity},
    
    db:write(SourceItem),
    db:write(NewItem),

    SourceItem.

equip(ItemId) ->
    [Item] = db:read(item, ItemId),
    NewItem = Item#item{equip = <<"true">>},
    db:write(NewItem).

unequip(ItemId) ->
    [Item] = db:read(item, ItemId),
    NewItem = Item#item{equip = <<"false">>},
    db:write(NewItem).

update(ItemId, 0) ->
    db:delete(item, ItemId);
update(ItemId, NewQuantity) ->
    [Item] = db:read(item, ItemId),
    NewItem = Item#item{quantity = NewQuantity},
    db:write(NewItem).

create(Owner, Name, Quantity) ->
    lager:debug("Owner: ~p Name: ~p Quantity: ~p", [Owner, Name, Quantity]),
    AllItems = db:dirty_index_read(item, Owner, #item.owner),

    NewItem = case filter_by_name(AllItems, Name) of
                  [] -> 
                      Id = util:get_id(),
                      create_item_attr(Id, Name),
                     
                      Class = item_attr:value(Id, <<"class">>),
                      Subclass = item_attr:value(Id, <<"subclass">>),
                      Weight = item_attr:value(Id, <<"weight">>),

                      #item {id = Id,
                             name = Name,
                             quantity = Quantity,
                             owner = Owner,
                             class = Class,
                             subclass = Subclass,
                             weight = Weight};

                  [Item | _Rest] -> 
                      NewQuantity = Item#item.quantity + Quantity,
                      Item#item{quantity = NewQuantity}
              end,
    
    db:write(NewItem),

    %Return item_def map with quantity
    ItemDef1 = item_def:all_to_map(Name),
    ItemDef2 = maps:put(<<"quantity">>, Quantity, ItemDef1),
    ItemDef3 = maps:put(<<"id">>, NewItem#item.id, ItemDef2),
    ItemDef3.

create(ItemMap) ->
    Id = util:get_id(),

    ItemFields = record_info(fields, item),
    ItemKeys = maps:keys(ItemMap),

    % Insert the item attributes
    F = fun(Key) ->
            case lists:member(Key, ItemFields) of
                true -> nothing; %Handled manually below
                false -> 
                    AttrKey = {Id, Key},
                    ItemAttr = #item_attr {key = AttrKey, 
                                           value = maps:get(Key, ItemMap)},
                    db:dirty_write(ItemAttr)
            end
        end,

    lists:foreach(F, ItemKeys),

    % Insert the base item entry
    NewItem = #item{id = Id,
                    name = maps:get(<<"name">>, ItemMap),
                    quantity = maps:get(<<"quantity">>, ItemMap),
                    owner = maps:get(<<"owner">>, ItemMap),
                    class = maps:get(<<"class">>, ItemMap),
                    subclass = maps:get(<<"subclass">>, ItemMap),
                    weight = maps:get(<<"weight">>, ItemMap, 0)},

    db:write(NewItem),
    
    %Return item map with all attrs
    all_attr_map(NewItem).

match_req(Item, ReqType, ReqQuantity) ->
    ItemName = maps:get(<<"name">>, Item),
    ItemSubClass = maps:get(<<"subclass">>, Item),
    ItemQuantity = maps:get(<<"quantity">>, Item),

    QuantityMatch = ReqQuantity =< ItemQuantity,            
    ItemNameMatch = ReqType =:= ItemName,
    ItemSubClassMatch = ReqType =:= ItemSubClass,

    lager:info("NameMatch ~p ~p ~p", [ReqType, ItemName, ItemSubClass]),
    lager:info("QuantityMatch ~p ~p", [ReqQuantity, ItemQuantity]),

    ItemMatch = ItemNameMatch or ItemSubClassMatch,
    lager:info("ItemMatch: ~p", [ItemMatch]),
    ItemMatch and QuantityMatch.

% Internal

rec_to_map(Item) when is_record(Item, item) ->
    #{<<"id">> => Item#item.id,
      <<"name">> => Item#item.name,
      <<"quantity">> => Item#item.quantity,
      <<"owner">> => Item#item.owner,
      <<"class">> => Item#item.class,
      <<"subclass">> => Item#item.subclass,
      <<"weight">> => Item#item.weight,
      <<"equip">> => Item#item.equip};
rec_to_map(Item) -> Item.

all_attr_map(Item) ->
    ItemMap = rec_to_map(Item),
    AttrMap = item_attr:all_to_map(Item#item.id),
    maps:merge(ItemMap, AttrMap).

filter_by_name(Items, Name) ->
    F = fun(Item) -> Item#item.name =:= Name end,
    lists:filter(F, Items).

create_item_attr(Id, Name) ->
    AllItemDef = item_def:all(Name),
    
    F = fun(ItemDef) -> 
            {Name, Attr} = item_def:key(ItemDef),
            case item_def:key(ItemDef) of
                {Name, Attr} ->
                    AttrKey = {Id, Attr},
                    ItemAttr = #item_attr {key = AttrKey, 
                                           value = item_def:value(ItemDef)},
                    db:dirty_write(ItemAttr)
            end
        end,

    lists:foreach(F, AllItemDef).


