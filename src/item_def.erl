%% Author: Peter
%% Created: Feb 15, 2009
%% Description: Implements simple persistent counter using mnesia
-module(item_def).

-include("schema.hrl").

-export([all/1, all_to_map/1, value/2]).

all(Name) ->
    db:dirty_match_object({item_def, {Name, '_'}, '_'}).

all_to_map(Name) ->
    All = db:dirty_match_object({item_def, {Name, '_'}, '_'}),

    F = fun(ItemDef, AllMap) ->
            {Name, Attr} = ItemDef#item_def.key,
            Value = ItemDef#item_def.value,
            maps:put(Attr, Value, AllMap)
        end,

    lists:foldl(F, #{}, All).

value(All, Attr) when is_list(All) ->
    [H | _Rest] = All,
    Key = H#item_def.key,
    {Name, _} = Key,
    ItemDef = lists:keyfind({Name, Attr}, 2, All),
    ItemDef#item_def.value;

value(Name, Attr) ->
    [ItemDef] = db:dirty_read(item_def, {Name, Attr}),
    ItemDef#item_def.value.

