%% Author: Peter
%% Created: Oct 10, 2015
%% Description: Skill system module
-module(skill).

-include("schema.hrl").
-include("common.hrl").

-export([get_by_owner/1, update/3]).

get_by_owner(Id) ->
    Skills = mdb:find(<<"skill">>, {owner, Id}),
    Skills.

update(Id, SkillName, Value) ->
    lager:info("skill:update id ~p skillname ~p value ~p", [Id, SkillName, Value]),
    {_SkillType} = mdb:find_one(<<"skill_type">>, {name, SkillName}),

    NewValue = case mdb:find_one(<<"skill">>, {owner, Id, name, SkillName}) of
                    {} ->
                       NewSkill = {owner, Id, name, SkillName, value, Value},
                       InsertedSkill = mongo:insert(mdb:get_conn(), <<"skill">>, NewSkill),
                       lager:info("InsertedSkill: ~p", [InsertedSkill]),
                       Value;
                    {Skill} ->
                       {SkillId} = bson:lookup('_id', Skill),
                       {CurrentValue} = bson:lookup(value, Skill),
                       UpdatedSkill = bson:update(value, CurrentValue + Value, Skill),
                       mdb:update(<<"skill">>, SkillId, UpdatedSkill),
                       CurrentValue + Value
               end,

    Player = get_player(Id),

    send_to_client(Player, skill_update, message(Id, SkillName, NewValue)).

get_player(Id) ->
    [Obj] = db:read(local_obj, Id),
    Obj#local_obj.player.

message(SourceId, SkillName, Value) ->
    Message = #{<<"packet">> => <<"skill_update">>,
                <<"sourceid">> => util:bin_to_hex(SourceId),
                <<"skill_name">> => SkillName,
                <<"value">> => Value},

    Message.

send_to_client(Player, MessageName, Message) ->
    [Conn] = db:dirty_read(connection, Player),
    Conn#connection.process ! {MessageName, Message}.
