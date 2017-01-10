%% Author: Peter
%% Created: Oct 10, 2015
%% Description: Skill system module
-module(skill).

-include("schema.hrl").
-include("common.hrl").

-export([get_by_owner/1, update/3]).

get_by_owner(Id) ->
    All = db:dirty_match_object({skill, {Id, '_'}, '_'}),

    F = fun(Skill, AllMap) ->
            {Id, Name} = Skill#skill.key,
            Value = Skill#skill.value,
            maps:put(Name, Value, AllMap)
        end,

    lists:foldl(F, #{}, All).

update(Id, SkillName, Value) ->
    lager:info("skill:update id ~p skillname ~p value ~p", [Id, SkillName, Value]),
    Player = get_player(Id),

    NewSkill = case db:dirty_read(skill, {Id, SkillName}) of
                  [] ->
                      #skill {key = {Id, SkillName},
                              value = Value};
                  [Skill] ->
                      CurrValue = Skill#skill.value,
                      Skill#skill {value = CurrValue + Value}
               end,

    db:dirty_write(NewSkill),

    send_to_client(Player, skill_update, message(Id, SkillName, NewSkill#skill.value)).

get_player(Id) ->
    [Obj] = db:read(obj, Id),
    Obj#obj.player.

message(SourceId, SkillName, Value) ->
    #{<<"packet">> => <<"skill_update">>,
      <<"sourceid">> => SourceId,
      <<"skill_name">> => SkillName,
      <<"value">> => Value}.

send_to_client(Player, MessageName, Message) ->
    [Conn] = db:dirty_read(connection, Player),
    Conn#connection.process ! {MessageName, Message}.
