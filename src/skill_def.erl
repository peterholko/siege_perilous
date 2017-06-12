%% Author: Peter
%% Created: June 15, 2017
%% Description: Wrapper for skill_def table
-module(skill_def).

-include("schema.hrl").

-export([all/1, all_to_map/1, select/2, value/2]).

all(Name) ->
    db:dirty_match_object({skill_def, {Name, '_'}, '_'}).

all_to_map(Name) ->
    to_map(all(Name)).

select(Key, Val) ->
    ListOfMatches = db:dirty_match_object({skill_def, {'_', Key}, Val}),
    
    F = fun(SkillDef, Acc) ->
            {Name, _} = SkillDef#skill_def.key,
            All = all(Name),
            [to_map(All) | Acc]
        end,

    lists:foldl(F, [], ListOfMatches).

to_map(All) ->
    F = fun(SkillDef, AllMap) ->
            {_Name, Attr} = SkillDef#skill_def.key,
            Value = SkillDef#skill_def.value,
            maps:put(Attr, Value, AllMap) 
        end,
    lists:foldl(F, #{}, All).

value(All, Attr) when is_list(All) ->
    [H | _Rest] = All,
    Key = H#skill_def.key,
    {Name, _} = Key,
    SkillDef = lists:keyfind({Name, Attr}, 2, All),
    SkillDef#skill_def.value;

value(Name, Attr) ->
    [SkillDef] = db:dirty_read(skill_def, {Name, Attr}),
    SkillDef#skill_def.value.

