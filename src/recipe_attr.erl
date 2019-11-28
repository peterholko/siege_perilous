%% Author: Peter
%% Created: Feb 15, 2019
%% Description: Implements recipe_attr using mnesia
-module(recipe_attr).

-include("schema.hrl").

-export([all/1, all_to_map/1, value/2, value/3]).
-export([add/3, set/3, update/3]). 

all(Id) ->
    db:dirty_match_object({recipe_attr, {Id, '_'}, '_'}).

all_to_map(Id) ->
    All = db:dirty_match_object({recipe_attr, {Id, '_'}, '_'}),

    F = fun(RecipeAttr, AllMap) ->
            {Id, Name} = RecipeAttr#recipe_attr.key,
            Value = RecipeAttr#recipe_attr.value,
            maps:put(Name, Value, AllMap)
        end,

    lists:foldl(F, #{}, All).

value(All, Attr) when is_list(All) ->
    [H | _Rest] = All,
    Key = H#recipe_attr.key,
    {Id, _} = Key,
    RecipeAttr = lists:keyfind({Id, Attr}, 2, All),
    RecipeAttr#recipe_attr.value;

value(Id, Attr) ->
    [RecipeAttr] = db:dirty_read(recipe_attr, {Id, Attr}),
    RecipeAttr#recipe_attr.value.

value(Id, Attr, Default) ->
    case db:dirty_read(recipe_attr, {Id, Attr}) of
        [RecipeAttr] -> RecipeAttr#recipe_attr.value;
        [] -> Default
    end.

add(Id, Attr, Value) ->
    RecipeAttr = #recipe_attr{key = {Id, Attr},
                              value = Value},
    db:dirty_write(RecipeAttr).

set(Id, Attr, Value) ->
    [RecipeAttr] = db:dirty_read(recipe_attr, {Id, Attr}),
    NewRecipeAttr = RecipeAttr#recipe_attr {value = Value},
    db:dirty_write(NewRecipeAttr).

update(Id, Attr, Value) ->
    [RecipeAttr] = db:dirty_read(recipe_attr, {Id, Attr}),
    NewValue = RecipeAttr#recipe_attr.value + Value,
    NewRecipeAttr = RecipeAttr#recipe_attr {value = NewValue},
    db:dirty_write(NewRecipeAttr).
