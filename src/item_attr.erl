%% Author: Peter
%% Created: Feb 15, 2009
%% Description: Implements simple persistent counter using mnesia
-module(item_attr).

-include("schema.hrl").

-export([all/1, all_to_map/1, value/2, value/3]).
-export([has/2, set/3, update/3, copy/2, delete/2]). 

all(Id) ->
    db:dirty_match_object({item_attr, {Id, '_'}, '_'}).

all_to_map(Id) ->
    All = db:dirty_match_object({item_attr, {Id, '_'}, '_'}),

    F = fun(ItemAttr, AllMap) ->
            {Id, Name} = ItemAttr#item_attr.key,
            Value = ItemAttr#item_attr.value,
            maps:put(Name, Value, AllMap)
        end,

    lists:foldl(F, #{}, All).

has(Id, Attr) ->
    case db:dirty_read(item_attr, {Id, Attr}) of
        [_ItemAttr] -> true;
        [] -> false
    end.

value(All, Attr) when is_list(All) ->
    [H | _Rest] = All,
    Key = H#item_attr.key,
    {Id, _} = Key,
    ItemAttr = lists:keyfind({Id, Attr}, 2, All),
    ItemAttr#item_attr.value;

value(Id, Attr) ->
    [ItemAttr] = db:dirty_read(item_attr, {Id, Attr}),
    ItemAttr#item_attr.value.

value(Id, Attr, Default) ->
    case db:dirty_read(item_attr, {Id, Attr}) of
        [ItemAttr] -> ItemAttr#item_attr.value;
        [] -> Default
    end.

set(Id, Attr, Value) ->
    NewItemAttr = case db:dirty_read(item_attr, {Id, Attr}) of
                      [ItemAttr] -> 
                          ItemAttr#item_attr {value = Value};
                      [] -> 
                          #item_attr{key = {Id, Attr},
                                     value = Value}
                  end,
    db:dirty_write(NewItemAttr).

update(Id, Attr, Value) ->
    [ItemAttr] = db:dirty_read(item_attr, {Id, Attr}),
    NewValue = ItemAttr#item_attr.value + Value,
    NewItemAttr = ItemAttr#item_attr {value = NewValue},
    db:dirty_write(NewItemAttr).

copy(SourceId, TargetId) ->
    All = db:dirty_match_object({item_attr, {SourceId, '_'}, '_'}),

    F = fun(ItemAttr) ->
            {_Id, Name} = ItemAttr#item_attr.key,
            set(TargetId, Name, ItemAttr#item_attr.value)
        end,

    lists:foreach(F, All).

delete(Id, Attr) ->
    %TODO delete match object
    lager:info("item attr delete"),
    db:dirty_delete(item_attr, {Id, Attr}).

