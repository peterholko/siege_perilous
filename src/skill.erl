%% Author: Peter
%% Created: Oct 10, 2015
%% Description: Skill system module
-module(skill).

-include("schema.hrl").
-include("common.hrl").

-export([update/3]).

update(Id, SkillName, Value) ->
    [SkillType] = find_type(name, SkillName),

    NewValue = case find(owner, Id) of
                    [] ->
                        NewSkill = bson:exclude(['_id'], SkillType),
                        NewSkill2 = bson:merge({owner, Id, SkillName, Value}, NewSkill),
                        InsertedSkill = mongo:insert(mdb:get_conn(), <<"skill">>, NewSkill2),
                        lager:info("InsertedSkill: ~p", [InsertedSkill]),
                        Value;
                    [Skill] ->
                        {SkillId} = bson:lookup('_id', Skill),
                        {SkillValue} = bson:lookup(SkillName, Skill),
                        UpdatedValue = SkillValue = Value,
                        UpdatedSkill = bson:update(SkillName, UpdatedValue),
                        lager:info("UpdatedSkill: ~p", [UpdatedSkill]),
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
