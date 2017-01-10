%% Author: Peter
%% Created: Dec, 2016
%% Description: Sound module
-module(sound).

-include("schema.hrl").
-include("common.hrl").

-export([whisper/2, talk/2, sound/3]).


whisper(ObjId, Text) ->
    speech(ObjId, 1, Text).

talk(ObjId, Text) ->
    speech(ObjId, 2, Text).

speech(ObjId, Range, Text) ->
    Obj = obj:get(ObjId),
    Range = 2,

    SoundPacket = #{<<"packet">> => <<"speech">>,
                    <<"source">> => ObjId,
                    <<"text">> => list_to_binary(Text)},
                   
    perception:broadcast(Obj#obj.pos, Range, SoundPacket).

sound(Pos, Range, Text) ->
    {X, Y} = Pos,

    SoundPacket = #{<<"packet">> => <<"sound">>,
                    <<"x">> => X,
                    <<"y">> => Y,
                    <<"text">> => list_to_binary(Text)},
                   
    perception:broadcast(Pos, Range, SoundPacket).
