%% Author: Peter
%% Created: June 15, 2017
%% Description: Wrapper for skill_template table
-module(skill_template).

-include("schema.hrl").

-export([all/1, all_to_map/1, select/2, value/2]).

all(Name) ->
    db:dirty_match_object({skill_template, {Name, '_'}, '_'}).

all_to_map(Name) ->
    to_map(all(Name)).

select(Key, Val) ->
    ListOfMatches = db:dirty_match_object({skill_template, {'_', Key}, Val}),
    
    F = fun(SkillTemplate, Acc) ->
            {Name, _} = SkillTemplate#skill_template.key,
            All = all(Name),
            [to_map(All) | Acc]
        end,

    lists:foldl(F, [], ListOfMatches).

to_map(All) ->
    F = fun(SkillTemplate, AllMap) ->
            {_Name, Attr} = SkillTemplate#skill_template.key,
            Value = SkillTemplate#skill_template.value,
            maps:put(Attr, Value, AllMap) 
        end,
    lists:foldl(F, #{}, All).

value(All, Attr) when is_list(All) ->
    [H | _Rest] = All,
    Key = H#skill_template.key,
    {Name, _} = Key,
    SkillTemplate = lists:keyfind({Name, Attr}, 2, All),
    SkillTemplate#skill_template.value;

value(Name, Attr) ->
    [SkillTemplate] = db:dirty_read(skill_template, {Name, Attr}),
    SkillTemplate#skill_template.value.

