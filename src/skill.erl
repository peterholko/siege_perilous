%% Author: Peter
%% Created: Oct 10, 2015
%% Description: Skill system module
-module(skill).

-include("schema.hrl").
-include("common.hrl").

-export([get_by_owner/1, get_by_name/2, update/3]).

get_by_owner(Id) ->
    All = db:match_object({skill, {Id, '_'}, '_'}),

    F = fun(Skill, AllMap) ->
            {Id, Name} = Skill#skill.key,
            Value = {Skill#skill.level, Skill#skill.xp},
            maps:put(Name, Value, AllMap)
        end,

    lists:foldl(F, #{}, All).

get_by_name(Id, Name) ->
    case db:read(skill, {Id, Name}) of
        [] -> 0;
        [Skill] -> Skill
    end.

update(Id, SkillName, Value) ->
    lager:info("skill:update id ~p skillname ~p value ~p", [Id, SkillName, Value]),
    FullXpList = skill_template:value(SkillName, <<"xp">>),

    NewSkill = case db:read(skill, {Id, SkillName}) of
                  [] ->
                      Skill = #skill {key = {Id, SkillName},
                                      name = SkillName,
                                      xp = 0},

                      update_xp(Skill, Value, FullXpList);
                  [Skill] ->
                      XpList = lists:sublist(FullXpList, 
                                             Skill#skill.level + 1,
                                             100), %Max levels

                      update_xp(Skill, Value, XpList)
               end,

    db:write(NewSkill).


    %TODO FIX
    %case obj:is_hero_nearby(Obj, Obj#obj.player) of
    %    true ->
    %        send_to_client(Obj#obj.player, skill_update, message(Id, SkillName, NewSkill#skill.value));
    %    false ->
    %        nothing
    %end.

update_xp(Skill, Value, XpList = [XpLevel | _]) ->
    lager:info("Init Skill: ~p", [Skill]),
    case Skill#skill.xp + Value > XpLevel of
        true ->
            lager:info("Init 2 Skill: ~p", [Skill]),
            roll_over_xp(Skill, Value, XpList, false);
        false ->
            Xp = Skill#skill.xp,
            Skill#skill {xp = Xp + Value}
    end.

roll_over_xp(Skill, _Value, _XpList, true) ->
    Skill;
roll_over_xp(Skill, _Value, [], _Result) ->
    Skill;
roll_over_xp(Skill, Value, [XpLevel | Rest], false) ->
    lager:info("Skill: ~p Value: ~p XpLevel: ~p", [Skill, Value, XpLevel]),
    lager:info("Value: ~p", [Value]),
    {NewSkill, NewValue, Result} = case Skill#skill.xp + Value > XpLevel of
                            true ->
                                Level = Skill#skill.level,
                                NewVal = Value - XpLevel,
                                lager:info("NewValue: ~p", [NewVal]),
                                {Skill#skill { xp = 0, level = Level + 1}, 
                                 NewVal, 
                                 false};
                            false ->
                                Xp = Skill#skill.xp,
                                {Skill#skill {xp = Xp + Value}, 
                                 0, 
                                 true}
                         end,

    roll_over_xp(NewSkill, NewValue, Rest, Result).
                                
message(SourceId, SkillName, Value) ->
    #{<<"packet">> => <<"skill_update">>,
      <<"sourceid">> => SourceId,
      <<"skill_name">> => SkillName,
      <<"value">> => Value}.

send_to_client(Player, MessageName, Message) ->
    [Conn] = db:read(connection, Player),
    Conn#connection.process ! {MessageName, Message}.
