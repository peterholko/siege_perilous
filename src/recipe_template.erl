%% Author: Peter
%% Created: Jan 15, 2017
%% Description: Wrapper for recipe_template table
-module(recipe_template).

-include("schema.hrl").

-export([all/1, all_to_map/1, select/2, value/2]).

all(Name) ->
    db:dirty_match_object({recipe_template, {Name, '_'}, '_'}).

all_to_map(Name) ->
    to_map(all(Name)).

select(Key, Val) ->
    ListOfMatches = db:dirty_match_object({recipe_template, {'_', Key}, Val}),
    
    F = fun(RecipeTemplate, Acc) ->
            {Name, _} = RecipeTemplate#recipe_template.key,
            All = all(Name),
            [to_map(All) | Acc]
        end,

    lists:foldl(F, [], ListOfMatches).

to_map(All) ->
    F = fun(RecipeTemplate, AllMap) ->
            {_Name, Attr} = RecipeTemplate#recipe_template.key,
            Value = RecipeTemplate#recipe_template.value,
            maps:put(Attr, Value, AllMap) 
        end,
    lists:foldl(F, #{}, All).

value(All, Attr) when is_list(All) ->
    [H | _Rest] = All,
    Key = H#recipe_template.key,
    {Name, _} = Key,
    RecipeTemplate = lists:keyfind({Name, Attr}, 2, All),
    RecipeTemplate#recipe_template.value;

value(Name, Attr) ->
    [RecipeTemplate] = db:dirty_read(recipe_template, {Name, Attr}),
    RecipeTemplate#recipe_template.value.

