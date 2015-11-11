%% Author: Peter
%% Created: Oct 10, 2015
%% Description: Skill system module
-module(skill).

-include("schema.hrl").
-include("common.hrl").

-export([get_by_owner/1, update/3, find/2, find_type/2]).

get_by_owner(Id) ->
    Skills = find(owner, Id),
    Skills.

update(Id, SkillName, Value) ->
    lager:info("skill:update id ~p skillname ~p value ~p", [Id, SkillName, Value]),
    [SkillType] = find_type(name, SkillName),

    NewValue = case find(owner, Id) of
                    [] ->
                        NewSkill = {owner, Id, SkillName, Value},
                        InsertedSkill = mongo:insert(mdb:get_conn(), <<"skill">>, NewSkill),
                        lager:info("InsertedSkill: ~p", [InsertedSkill]),
                        Value;
                    [Skill] ->
                        UpdatedValue = case bson:lookup(binary_to_atom(SkillName, latin1), Skill) of
                                           {} -> Value;
                                           {CurrentValue} -> CurrentValue + Value
                                       end,
                        UpdatedSkill = bson:update(SkillName, UpdatedValue, Skill),
                        lager:info("UpdatedSkill: ~p", [UpdatedSkill]),
                        {SkillId} = bson:lookup('_id', Skill),
                        mdb:update(<<"skill">>, SkillId, UpdatedSkill),
                        UpdatedValue
               end,

    Player = get_player(Id),

    send_to_client(Player, skill_update, message(Id, SkillName, NewValue)).

get_player(Id) ->
    [Obj] = db:read(local_obj, Id),
    Obj#local_obj.player.

find(Key, Value) ->
    Cursor = mongo:find(mdb:get_conn(), <<"skill">>, {Key, Value}),
    Skills = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Skills.

find_type(Key, Value) ->
    Cursor = mongo:find(mdb:get_conn(), <<"skill_type">>, {Key, Value}),
    SkillTypes = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    SkillTypes.

message(SourceId, SkillName, Value) ->
    Message = #{<<"packet">> => <<"skill_update">>,
                <<"sourceid">> => util:bin_to_hex(SourceId),
                <<"skill_name">> => SkillName,
                <<"value">> => Value},

    Message.

send_to_client(Player, MessageName, Message) ->
    [Conn] = db:dirty_read(connection, Player),
    Conn#connection.process ! {MessageName, Message}.
