%% Author: Peter
%% Created: Jan 15, 2017
%% Description: Wrapper for recipe_def table
-module(recipe_def).

-include("schema.hrl").

-export([all/1, all_to_map/1, select/2, value/2]).

all(Name) ->
    db:dirty_match_object({recipe_def, {Name, '_'}, '_'}).

all_to_map(Name) ->
    to_map(all(Name)).

select(Key, Val) ->
    ListOfMatches = db:dirty_match_object({recipe_def, {'_', Key}, Val}),
    
    F = fun(RecipeDef, Acc) ->
            {Name, _} = RecipeDef#recipe_def.key,
            All = all(Name),
            [to_map(All) | Acc]
        end,

    lists:foldl(F, [], ListOfMatches).

to_map(All) ->
    F = fun(RecipeDef, AllMap) ->
            {_Name, Attr} = RecipeDef#recipe_def.key,
            Value = RecipeDef#recipe_def.value,
            maps:put(Attr, Value, AllMap) 
        end,
    lists:foldl(F, #{}, All).

value(All, Attr) when is_list(All) ->
    [H | _Rest] = All,
    Key = H#recipe_def.key,
    {Name, _} = Key,
    RecipeDef = lists:keyfind({Name, Attr}, 2, All),
    RecipeDef#recipe_def.value;

value(Name, Attr) ->
    [RecipeDef] = db:dirty_read(recipe_def, {Name, Attr}),
    RecipeDef#recipe_def.value.

