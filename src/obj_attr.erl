%% Author: Peter
%% Created: Feb 15, 2015
%% Description: Implements simple persistent counter using mnesia
-module(obj_attr).

-include("schema.hrl").

-export([all/1, all_to_map/1, value/2, value/3, has/2]).
-export([set/3, update/3, delete/2]). 

all(Id) ->
    db:dirty_match_object({obj_attr, {Id, '_'}, '_'}).

all_to_map(Id) ->
    All = db:dirty_match_object({obj_attr, {Id, '_'}, '_'}),

    F = fun(ObjAttr, AllMap) ->
            {Id, Name} = ObjAttr#obj_attr.key,
            Value = ObjAttr#obj_attr.value,
            maps:put(Name, Value, AllMap)
        end,

    lists:foldl(F, #{}, All).

has(Id, Attr) ->
    case db:dirty_read(obj_attr, {Id, Attr}) of
        [_ObjAttr] -> true;
        [] -> false
    end.

value(All, Attr) when is_list(All) ->
    [H | _Rest] = All,
    Key = H#obj_attr.key,
    {Id, _} = Key,
    ObjAttr = lists:keyfind({Id, Attr}, 2, All),
    ObjAttr#obj_attr.value;

value(Id, Attr) ->
    [ObjAttr] = db:dirty_read(obj_attr, {Id, Attr}),
    ObjAttr#obj_attr.value.

value(Id, Attr, Default) ->
    case db:dirty_read(obj_attr, {Id, Attr}) of
        [ObjAttr] -> ObjAttr#obj_attr.value;
        [] -> Default
    end.

set(Id, Attr, Value) ->
    ObjAttr = #obj_attr{key = {Id, Attr},
                        value = Value},
    db:dirty_write(ObjAttr).

update(Id, Attr, Value) ->
    [ObjAttr] = db:dirty_read(obj_attr, {Id, Attr}),
    NewValue = ObjAttr#obj_attr.value + Value,
    NewObjAttr = ObjAttr#obj_attr {value = NewValue},
    db:dirty_write(NewObjAttr).

delete(Id, Attr) ->
    db:dirty_delete(obj_attr, {Id, Attr}).
