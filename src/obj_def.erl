%% Author: Peter
%% Created: Feb 15, 2009
%% Description: Implements simple persistent counter using mnesia
-module(obj_def).

-include("schema.hrl").

-export([all/1, select/2, value/2]).

all(Name) ->
    db:dirty_match_object({obj_def, {Name, '_'}, '_'}).

select(Key, Val) ->
    ListOfMatches = db:dirty_match_object({obj_def, {'_', Key}, Val}),
    
    F = fun(ObjDef, Acc) ->
            {Name, _} = ObjDef#obj_def.key,
            All = all(Name),
            [to_map(All) | Acc]
        end,

    lists:foldl(F, [], ListOfMatches).

to_map(All) ->
    F = fun(ObjDef, AllMap) ->
            {_Name, Attr} = ObjDef#obj_def.key,
            Value = ObjDef#obj_def.value,
            maps:put(Attr, Value, AllMap) 
        end,
    lists:foldl(F, #{}, All).

value(All, Attr) when is_list(All) ->
    [H | _Rest] = All,
    Key = H#obj_def.key,
    {Name, _} = Key,
    ObjDef = lists:keyfind({Name, Attr}, 2, All),
    ObjDef#obj_def.value;

value(Name, Attr) ->
    [ObjDef] = db:dirty_read(obj_def, {Name, Attr}),
    ObjDef#obj_def.value.

