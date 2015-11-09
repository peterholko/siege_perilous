%% Author: Peter
%% Created: Oct 10, 2015
%% Description: Skill system module
-module(skill).

-include("schema.hrl").
-include("common.hrl").

-export([update/3]).

update(Id, SkillName, Value) ->
    NewSkill = case db:index_read(skill, SkillName, #skill.skill_name) of
                    [Skill] ->
                        NewValue = Skill#skill.value + Value,
                        Skill#skill {value = NewValue};
                    [] ->
                        #skill {key = {Id, SkillName},
                                id = Id,
                                skill_name = SkillName,
                                value = Value}
                end,
    
    db:write(NewSkill),

    [Obj] = db:read(local_obj, Id),
    Player = Obj#local_obj.player,
    send_to_client(Player, skill_update, message(Id, SkillName, NewSkill#skill.value)).

message(SourceId, SkillName, Value) ->
    Message = #{<<"packet">> => <<"skill_update">>,
                <<"sourceid">> => util:bin_to_hex(SourceId),
                <<"skill_name">> => SkillName,
                <<"value">> => Value},

    Message.

send_to_client(Player, MessageName, Message) ->
    [Conn] = db:dirty_read(connection, Player),
    Conn#connection.process ! {MessageName, Message}.
