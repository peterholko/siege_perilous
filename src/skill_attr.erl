%% Author: Peter
%% Created: Feb 15, 2009
%% Description: Implements simple persistent counter using mnesia
-module(skill_attr).

-include("schema.hrl").

-export([all/1, all_to_map/1, value/2, value/3]).
-export([add/3, set/3, update/3]). 

all(Id) ->
    db:dirty_match_object({skill_attr, {Id, '_'}, '_'}).

all_to_map(Id) ->
    All = db:dirty_match_object({skill_attr, {Id, '_'}, '_'}),

    F = fun(SkillAttr, AllMap) ->
            {Id, Name} = SkillAttr#skill_attr.key,
            Value = SkillAttr#skill_attr.value,
            maps:put(Name, Value, AllMap)
        end,

    lists:foldl(F, #{}, All).

value(All, Attr) when is_list(All) ->
    [H | _Rest] = All,
    Key = H#skill_attr.key,
    {Id, _} = Key,
    SkillAttr = lists:keyfind({Id, Attr}, 2, All),
    SkillAttr#skill_attr.value;

value(Id, Attr) ->
    [SkillAttr] = db:dirty_read(skill_attr, {Id, Attr}),
    SkillAttr#skill_attr.value.

value(Id, Attr, Default) ->
    case db:dirty_read(skill_attr, {Id, Attr}) of
        [SkillAttr] -> SkillAttr#skill_attr.value;
        [] -> Default
    end.

add(Id, Attr, Value) ->
    SkillAttr = #skill_attr{key = {Id, Attr},
                          value = Value},
    db:dirty_write(SkillAttr).

set(Id, Attr, Value) ->
    [SkillAttr] = db:dirty_read(skill_attr, {Id, Attr}),
    NewSkillAttr = SkillAttr#skill_attr {value = Value},
    db:dirty_write(NewSkillAttr).

update(Id, Attr, Value) ->
    [SkillAttr] = db:dirty_read(skill_attr, {Id, Attr}),
    NewValue = SkillAttr#skill_attr.value + Value,
    NewSkillAttr = SkillAttr#skill_attr {value = NewValue},
    db:dirty_write(NewSkillAttr).
