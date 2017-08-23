%% Author: Peter
%% Created: Feb 15, 2009
%% Description: Implements simple persistent counter using mnesia
-module(obj_template).

-include("schema.hrl").

-export([all/1, select/2, value/2]).

all(Name) ->
    db:dirty_match_object({obj_template, {Name, '_'}, '_'}).

select(Key, Val) ->
    ListOfMatches = db:dirty_match_object({obj_template, {'_', Key}, Val}),
    
    F = fun(ObjTemplate, Acc) ->
            {Name, _} = ObjTemplate#obj_template.key,
            All = all(Name),
            [to_map(All) | Acc]
        end,

    lists:foldl(F, [], ListOfMatches).

to_map(All) ->
    F = fun(ObjTemplate, AllMap) ->
            {_Name, Attr} = ObjTemplate#obj_template.key,
            Value = ObjTemplate#obj_template.value,
            maps:put(Attr, Value, AllMap) 
        end,
    lists:foldl(F, #{}, All).

value(All, Attr) when is_list(All) ->
    [H | _Rest] = All,
    Key = H#obj_template.key,
    {Name, _} = Key,
    ObjTemplate = lists:keyfind({Name, Attr}, 2, All),
    ObjTemplate#obj_template.value;

value(Name, Attr) ->
    [ObjTemplate] = db:dirty_read(obj_template, {Name, Attr}),
    ObjTemplate#obj_template.value.

