%% Author: Peter
%% Created: Oct 10, 2015
%% Description: Skill system module
-module(skill).

-include("schema.hrl").
-include("common.hrl").

-export([get_by_owner/1, 
         get_by_owner_with_xp/1, 
         get_by_name/2, 
         get_total_xp/1,
         update/3,
         hero_advance/1]).

get_by_owner(Id) ->
    All = db:match_object({skill, {Id, '_'}, '_', '_', '_'}),

    F = fun(Skill, AllMap) ->
            {Id, Name} = Skill#skill.key,

            maps:put(Name, Skill#skill.level, AllMap)
        end,

    lists:foldl(F, #{}, All).

get_by_owner_with_xp(Id) ->
    All = db:match_object({skill, {Id, '_'}, '_', '_', '_'}),

    F = fun(Skill, AllMap) ->
            {Id, Name} = Skill#skill.key,

            FullXpList = skill_template:value(Name, <<"xp">>),

            NextLevelXp = 
                case (Skill#skill.level + 1) =< length(FullXpList) of
                    true ->    
                        lists:nth(Skill#skill.level + 1, FullXpList);
                    false ->
                        -1 %Indicates max level
                end,

            LevelXp = #{<<"level">> => Skill#skill.level,
                        <<"xp">> => Skill#skill.xp,
                        <<"next">> => NextLevelXp},

            maps:put(Name, LevelXp, AllMap)
        end,

    lists:foldl(F, #{}, All).

get_by_name(Id, Name) ->
    case db:read(skill, {Id, Name}) of
        [] -> 0;
        [Skill] -> Skill
    end.

get_total_xp(Id) ->
    All = db:match_object({skill, {Id, '_'}, '_', '_', '_'}),

    F = fun(Skill, TotalXp) ->
            {Id, Name} = Skill#skill.key,

            FullXpList = skill_template:value(Name, <<"xp">>),

            SkillXp = sum_level_xp(Skill#skill.level, FullXpList) + Skill#skill.xp,

            SkillXp + TotalXp
        end,

    lists:foldl(F, 0, All).

sum_level_xp(Level, FullXpList) ->
    LevelXpList = lists:sublist(FullXpList, Level),

    F = fun(LevelXp, TotalXp) ->
            LevelXp + TotalXp
        end,

    lists:foldl(F, 0, LevelXpList).


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
    roll_over_xp(Skill, Value, XpList, false).

roll_over_xp(Skill, _Value, _XpList, true) ->
    Skill;
roll_over_xp(Skill, _Value, [], _Result) ->
    Skill;
roll_over_xp(Skill, Value, [XpLevel | Rest], false) ->
    lager:info("Skill: ~p Value: ~p XpLevel: ~p", [Skill, Value, XpLevel]),
    lager:info("Value: ~p", [Value]),
    Level = Skill#skill.level,
    Xp = Skill#skill.xp,

    {NewSkill, NewValue, Result} = case Xp + Value of
                            Total when Total > XpLevel ->
                                NewVal = Value - XpLevel,
                                lager:info("NewValue: ~p", [NewVal]),
                                {Skill#skill {xp = 0, level = Level + 1}, 
                                 NewVal, 
                                 false};
                            Total when Total =:= XpLevel ->
                                {Skill#skill {xp = 0, level = Level + 1},
                                 0,
                                 true};
                            Total when Total < XpLevel ->
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

hero_advance(<<"Novice Warrior">>) -> {<<"Skilled Warrior">>, 10000};
hero_advance(<<"Novice Ranger">>) -> {<<"Skilled Ranger">>, 10000};
hero_advance(<<"Novice Mage">>) -> {<<"Skilled Mage">>, 10000};
hero_advance(<<"Skilled Warrior">>) -> {<<"Great Warrior">>, 50000};
hero_advance(<<"Skilled Ranger">>) -> {<<"Great Ranger">>, 50000};
hero_advance(<<"Skilled Mage">>) -> {<<"Great Mage">>, 50000};
hero_advance(<<"Great Warrior">>) -> {<<"Legendary Warrior">>, 1000000};
hero_advance(<<"Great Ranger">>) -> {<<"Legendary Ranger">>, 1000000};
hero_advance(<<"Great Mage">>) -> {<<"Legendary Mage">>, 1000000};
hero_advance(<<"Legendary Warrior">>) -> {<<"Max Rank">>, -1};
hero_advance(<<"Legendary Ranger">>) -> {<<"Max Rank">>, -1};
hero_advance(<<"Legendary Mage">>) -> {<<"Max Rank">>, -1}.
