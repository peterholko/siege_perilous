%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to item data
-module(item).

-include("schema.hrl").

-export([get_rec/1, get_map/1, get_by_owner/1, get_by_subclass/2, get_by_name/2, get_equiped/1, get_equiped_weapon/1]).
-export([transfer/2, split/2, update/2, create/1, create/3, equip/1, unequip/1]).
-export([is_equipable/1, is_slot_free/2, is_player_owned/2, is_valid_split/3]).

get_rec(Id) ->
    case db:read(item, Id) of
        [Item] -> Item;
        _ -> invalid
    end.

get_map(Id) ->
    Item = get_rec(Id),
    item_to_map(Item).

get_by_owner(OwnerId) ->
    Items = db:dirty_index_read(item, OwnerId, #item.owner),

    F = fun(Item) ->            
            ItemMap = item_to_map(Item),
            AttrMap = item_attr:all_to_map(Item#item.id),
            maps:merge(ItemMap, AttrMap)
        end,

    lists:map(F, Items).

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

get_equiped_weapon(OwnerId) ->
    AllItems = get_by_owner(OwnerId),
    F = fun(ItemMap) -> 
                (maps:get(<<"equip">>, ItemMap) =:= <<"true">>) and
                (maps:get(<<"class">>, ItemMap) =:= <<"weapon">>)
        end,
    lists:filter(F, AllItems).

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
    lists:filter(F, AllItems).

is_player_owned(Player, ItemId) ->
    case db:read(item, ItemId) of
        [Item] -> Item#item.owner =:= Player;
        [] -> false
    end.

is_valid_split(Player, ItemId, Quantity) when Quantity > 1 ->
    case db:read(item, ItemId) of
        [Item] -> (Item#item.quantity > Quantity) and (Item#item.owner =:= Player);
        [] -> false
    end.

can_merge(ItemClass) ->
    case ItemClass of
        <<"Weapon">> -> false;
        <<"Armor">> -> false;
        _ -> true
    end.

transfer(ItemId, TargetId) ->
    [TransferItem] = db:dirty_read(item, ItemId),
    AllItems = db:dirty_index_read(item, TargetId, #item.owner),
    
    NewItem = case can_merge(TransferItem#item.class) of
                  true ->
                      case filter_by_name(AllItems, TransferItem#item.name) of
                          [] -> 
                              TransferItem#item{owner = TargetId};
                          [Item | _Rest] -> 
                              NewQuantity = Item#item.quantity + TransferItem#item.quantity,
                              Item#item{quantity = NewQuantity}
                      end;
                  false ->
                      TransferItem#item{owner = TargetId}
             end,
    db:write(NewItem).
                
split(ItemId, NewQuantity) ->
    [Item] = db:read(item, ItemId),
    CurrentQuantity = Item#item.quantity,

    NewItem1 = Item#item{quantity = NewQuantity},
    NewItem2 = Item#item{quantity = CurrentQuantity - NewQuantity},
    
    db:write(NewItem1),
    db:write(NewItem2).

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
    AllItems = db:dirty_index_read(item, Owner, #item.owner),

    NewItem = case filter_by_name(AllItems, Name) of
                  [] -> 
                      Id = util:get_id(),
                      create_item_attr(Id, Name),
                     
                      Class = item_attr:value(Id, <<"class">>),
                      Subclass = item_attr:value(Id, <<"subclass">>),
                      Weight = item_attr:value(Id, <<"weight">>, 0),

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
    
    db:write(NewItem).

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

    db:write(NewItem).

% Internal

item_to_map(Item) ->
    #{<<"id">> => Item#item.id,
      <<"name">> => Item#item.name,
      <<"quantity">> => Item#item.quantity,
      <<"owner">> => Item#item.owner,
      <<"class">> => Item#item.class,
      <<"subclass">> => Item#item.subclass,
      <<"weight">> => Item#item.weight,
      <<"equip">> => Item#item.equip}.

filter_by_name(Items, Name) ->
    F = fun(Item) -> Item#item.name =:= Name end,
    lists:filter(F, Items).

create_item_attr(Id, Name) ->
    AllItemDef = item_def:all(Name),
    
    F = fun(ItemDef) -> 
            {Name, Attr} = ItemDef#item_def.key,
            case ItemDef#item_def.key of
                {Name, Attr} ->
                    AttrKey = {Id, Attr},
                    ItemAttr = #item_attr {key = AttrKey, 
                                           value = ItemDef#item_def.value},
                    db:dirty_write(ItemAttr)
            end
        end,

    lists:foreach(F, AllItemDef).

