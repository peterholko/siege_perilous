%% Author: Peter
%% Created: Oct 10, 2015
%% Description: Skill system module
-module(skill).

-include("schema.hrl").
-include("common.hrl").

-export([get_by_owner/1, update/3]).

get_by_owner(Id) ->
    All = db:match_object({skill, {Id, '_'}, '_'}),

    F = fun(Skill, AllMap) ->
            {Id, Name} = Skill#skill.key,
            Value = Skill#skill.value,
            maps:put(Name, Value, AllMap)
        end,

    lists:foldl(F, #{}, All).


update(Id, SkillName, Value) ->
    lager:info("skill:update id ~p skillname ~p value ~p", [Id, SkillName, Value]),
    [Obj] = db:read(obj, Id),    

    NewSkill = case db:read(skill, {Id, SkillName}) of
                  [] ->
                      #skill {key = {Id, SkillName},
                              value = Value};
                  [Skill] ->
                      CurrValue = Skill#skill.value,
                      Skill#skill {value = CurrValue + Value}
               end,

    db:write(NewSkill).


    %TODO FIX
    %case obj:is_hero_nearby(Obj, Obj#obj.player) of
    %    true ->
    %        send_to_client(Obj#obj.player, skill_update, message(Id, SkillName, NewSkill#skill.value));
    %    false ->
    %        nothing
    %end.

message(SourceId, SkillName, Value) ->
    #{<<"packet">> => <<"skill_update">>,
      <<"sourceid">> => SourceId,
      <<"skill_name">> => SkillName,
      <<"value">> => Value}.

send_to_client(Player, MessageName, Message) ->
    [Conn] = db:read(connection, Player),
    Conn#connection.process ! {MessageName, Message}.
