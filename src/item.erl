%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to item data
-module(item).

-include("schema.hrl").

-export([get_rec/1, get_map/1, get_all_attr/1, get_map_by_name/1, get_by_owner/1, get_by_owner_rec/1, 
         get_by_subclass/2, get_by_name/2, get_equiped/1, get_non_equiped/1, get_equiped_weapon/1,
         get_weapon_range/1, get_by_class/2]).
-export([transfer/2, transfer/3, transfer_by_class/4,
         split/2, update/2, create/1, create/3, create/4, equip/1, unequip/1]).
-export([has_by_class/2, has_by_subclass/2, has_price/1]).
-export([is_equipable/1, is_slot_free/2, is_player_owned/2, is_valid_split/3, 
         is_class/2, is_subclass/2]).
-export([get_total_weight/1, total_gold/1, weight/2]).
-export([id/1, owner/1, quantity/1, price/1]).
-export([match_req/3]).

id(Item) when is_map(Item) -> maps:get(<<"id">>, Item);
id(Item) -> Item#item.id.

owner(Item) when is_map(Item) -> maps:get(<<"owner">>, Item);
owner(Item) -> Item#item.owner.

quantity(Item) when is_map(Item) -> maps:get(<<"quantity">>, Item);
quantity(Item) -> Item#item.quantity.

price(ItemId) ->
    item_attr:value(ItemId, <<"price">>).

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

get_by_class(OwnerId, Class) ->
    AllItems = get_by_owner(OwnerId),
    F = fun(ItemMap) -> maps:get(<<"class">>, ItemMap) =:= Class end,
    lists:filter(F, AllItems).

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

get_weapon_range(_OwnerId) ->
    1.
    %Weapons = get_equiped_weapon(OwnerId),

    %TODO fix for dual wield
    %[Weapon | _] = Weapons,

    %maps:get(<<"range">>, Weapon).

%TODO explore adding weight to obj instead of calculating
get_total_weight(ObjId) ->
    AllItems = db:dirty_index_read(item, ObjId, #item.owner),

    F = fun(Item, AccWeight) ->
            ItemWeight = Item#item.quantity * Item#item.weight,
            ItemWeight + AccWeight
        end,

    TotalWeight = lists:foldl(F, 0, AllItems),
    TotalWeight.   

total_gold(ObjId) ->
    AllItems = db:dirty_index_read(item, ObjId, #item.owner),

    F = fun(Item, AccGold) ->
            case Item#item.class =:= <<"Gold">> of
                true ->
                    AccGold + Item#item.quantity;
                false ->
                    AccGold
            end
        end,

    TotalGold = lists:foldl(F, 0, AllItems),
    TotalGold.

has_by_class(OwnerId, Class) ->
    Items = get_by_class(OwnerId, Class),
    Items =/= [].

has_by_subclass(OwnerId, Subclass) ->
    Items = get_by_subclass(OwnerId, Subclass),
    Items =/= [].

has_price(ItemId) ->
    item_attr:has(ItemId, <<"price">>).

is_class(ItemName, Class) ->
    item_def:value(ItemName, <<"class">>) =:= Class.

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

transfer(TransferItemId, TargetOwnerId) ->
    [TransferItem] = db:dirty_read(item, TransferItemId),
    SourceOwnerId = TransferItem#item.owner,

    AllItems = db:dirty_index_read(item, TargetOwnerId, #item.owner),
    
    {Merged, NewItem} = case can_merge(TransferItem#item.class) of
                              true ->
                                  case filter_by_name(AllItems, TransferItem#item.name) of
                                      [] -> 
                                          {false, TransferItem#item{owner = TargetOwnerId}};
                                      [Item | _Rest] -> 
                                          %Update new quantity,
                                          NewQuantity = Item#item.quantity + TransferItem#item.quantity,
                                            
                                          %Remove the transfer item
                                          db:delete(item, TransferItemId),

                                          %Merge with existing item
                                          {true, Item#item{owner = TargetOwnerId,
                                                           quantity = NewQuantity}}
                                  end;
                              false ->
                                  {false, TransferItem#item{owner = TargetOwnerId}}
                         end,

    %Potentially send item change to player
    game:send_item_transfer(SourceOwnerId, 
                            TargetOwnerId, 
                            TransferItemId, 
                            all_attr_map(NewItem), 
                            Merged),

    db:write(NewItem),

    lager:info("Transfer Item: ~p", [NewItem]),
    %Return item map with all the attributes
    all_attr_map(NewItem).
                
transfer(ItemId, TargetId, Quantity) ->
    lager:info("ItemId: ~p TargetId: ~p Quantity: ~p", [ItemId, TargetId, Quantity]),
    NewItem = split(ItemId, Quantity), %TODO Split isn't required if it is a transfer
    transfer(NewItem#item.id, TargetId).

transfer_by_class(SourceId, TargetId, Class, Quantity) ->
    ClassItems = get_by_class(SourceId, Class),

    F = fun(ClassItem, Total) ->
            Total + quantity(ClassItem)
        end,

    ClassQuantity = lists:foldl(F, 0, ClassItems),

    case ClassQuantity >= Quantity of
        true ->
            transfer_class_items(ClassItems, TargetId, 0, Quantity);
        false ->
            nothing %Failure do not transfer
    end.
            
transfer_class_items(_ClassItems, _TargetId, AccQuantity, TargetQuantity) 
  when AccQuantity =:= TargetQuantity ->
    done;
transfer_class_items([ClassItem | Rest], TargetId, AccQuantity, TargetQuantity) ->
    Remaining = TargetQuantity - AccQuantity,

    NewAccQuantity = case Remaining < quantity(ClassItem) of
                        true ->
                            transfer(id(ClassItem), TargetId, Remaining),
                            AccQuantity + Remaining; % None remaining
                        false ->
                            transfer(id(ClassItem), TargetId),
                            AccQuantity + quantity(ClassItem)
                     end,
    transfer_class_items(Rest, TargetId, NewAccQuantity, TargetQuantity).
    

split(ItemId, NewQuantity) ->
    [Item] = db:read(item, ItemId),
    CurrentQuantity = Item#item.quantity,
    NewId = util:get_id(),

    %Added quantity checks to ensure mistakes at the player checks 
    %won't cause duplication of items

    case CurrentQuantity - NewQuantity of
        N when N > 0 -> %New Object
            SourceItem = Item#item{quantity = CurrentQuantity - NewQuantity},
            NewItem = Item#item{id = NewId,
                                quantity = NewQuantity},

            item_attr:copy(ItemId, NewId),

            db:write(SourceItem),
            db:write(NewItem),

            NewItem;
        N when N =:= 0 -> 
            Item; %Passthrough to transfer function
        N when N < 0 ->
            erlang:error("Invalid item split", {ItemId, NewQuantity})
    end.

equip(ItemId) ->
    [Item] = db:read(item, ItemId),
    NewItem = Item#item{equip = <<"true">>},
    db:write(NewItem).

unequip(ItemId) ->
    [Item] = db:read(item, ItemId),
    NewItem = Item#item{equip = <<"false">>},
    db:write(NewItem).

update(ItemId, 0) ->
    [Item] = db:read(item, ItemId),
    NewItem = Item#item{quantity = 0},

    %Potentially send item change to player
    game:send_item_update(NewItem#item.owner, all_attr_map(NewItem), true),

    db:delete(item, ItemId);
update(ItemId, NewQuantity) ->
    [Item] = db:read(item, ItemId),
    NewItem = Item#item{quantity = NewQuantity},

    %Potentially send item change to player
    game:send_item_update(Item#item.owner, all_attr_map(NewItem), true),

    db:write(NewItem).

%TODO revisit true/false binary string
create(Owner, Name, Quantity) ->
    create(Owner, Name, Quantity, <<"false">>).

create(Owner, Name, Quantity, Equip) ->
    lager:info("Owner: ~p Name: ~p Quantity: ~p", [Owner, Name, Quantity]),
    AllItems = db:dirty_index_read(item, Owner, #item.owner),

    {Merged, NewItem} = case filter_by_name(AllItems, Name) of
                  [] -> 
                      Id = util:get_id(),
                      create_item_attr(Id, Name),
                     
                      Class = item_attr:value(Id, <<"class">>),
                      Subclass = item_attr:value(Id, <<"subclass">>),
                      Weight = item_attr:value(Id, <<"weight">>),

                      {false, #item {id = Id,
                                     name = Name,
                                     quantity = Quantity,
                                     owner = Owner,
                                     class = Class,
                                     subclass = Subclass,
                                     weight = Weight,
                                     equip = Equip}};
                  [Item | _Rest] -> 
                      NewQuantity = Item#item.quantity + Quantity,
                      {true, Item#item{quantity = NewQuantity}}
              end,
    
    %Potentially send item change to player
    game:send_item_update(NewItem#item.owner, all_attr_map(NewItem), Merged),

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
    ItemClass = maps:get(<<"class">>, Item),
    ItemSubClass = maps:get(<<"subclass">>, Item),
    ItemQuantity = maps:get(<<"quantity">>, Item),

    QuantityMatch = ReqQuantity =< ItemQuantity,            
    ItemNameMatch = ReqType =:= ItemName,
    ItemClassMatch = ReqType =:= ItemClass, 
    ItemSubClassMatch = ReqType =:= ItemSubClass,
    

    lager:info("NameMatch ~p ~p ~p", [ReqType, ItemName, ItemSubClass]),
    lager:info("QuantityMatch ~p ~p", [ReqQuantity, ItemQuantity]),

    ItemMatch = ItemNameMatch or ItemClassMatch or ItemSubClassMatch,
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


