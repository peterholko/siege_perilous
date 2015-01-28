% Author: Peter
%% Created: Nov 11, 2014
%% Description: Handles messages from client
-module(message).

-export([decode/1, prepare/2]).

decode(Message) ->
    lager:info("Message: ~p~n", [Message]),

    Decoded = json_decode(Message),
    lager:info("Decoded: ~p~n", [Decoded]),

    Cmd = map_get(<<"cmd">>, Decoded),
    message_handle(Cmd, Decoded).

map_get(Key, Map) ->
    try maps:get(Key, Map)
    catch
        _:_ ->
            lager:info("Invalid Map Key: ~p~n", [Key]),
            none
    end.

message_handle(<<"login">>, Message) -> 
    lager:info("message: login"),
    Username = map_get(<<"username">>, Message),
    Password = map_get(<<"password">>, Message),

    lager:info("Username: ~p~n", [Username]),
    lager:info("Password: ~p~n", [Password]),

    case login:login(Username, Password, self()) of
        {error, Error} ->
            Error;
        {success, PlayerId} ->
            lager:info("Successful login"),
            %Stored player id in process dict for easy access
            put(player_id, PlayerId),

            {PlayerId, Explored, Objs} = player:init_perception(PlayerId),
            Perception = [{<<"player">>, PlayerId},
                          {<<"explored">>, Explored},
                          {<<"objs">>, convert_id(Objs, [])}],
            jsx:encode(Perception)
    end;

message_handle(<<"info">>, Message) ->
    lager:info("message: info"),
    HexId = map_get(<<"id">>, Message),
    BinId = util:hex_to_bin(HexId),
    Type = map_get(<<"type">>, Message),

    Fields = jsx:encode(mdb:to_map(player:get_info(BinId, Type))),
    lager:info("Fields: ~p", [Fields]),
    Fields;

message_handle(<<"move">>, Message) ->
    lager:info("message: move"),

    HexId = map_get(<<"id">>, Message),
    BinId = util:hex_to_bin(HexId),
    
    Pos1D = map_get(<<"pos">>, Message),
    Result = player:move_obj(BinId, Pos1D),
    <<"Move added">>;   

message_handle(<<"attack">>, Message) ->
    lager:info("message: attack"),

    SourceId = map_get(<<"sourceid">>, Message),
    TargetId = map_get(<<"targetid">>, Message),

    player:attack_obj(SourceId, TargetId),
    <<"Attack added">>;

message_handle(<<"attack_unit">>, Message) ->
    lager:info("message: attack_unit"),

    SourceId = map_get(<<"sourceid">>, Message),
    TargetId = map_get(<<"targetid">>, Message),
    SourceBinId = util:hex_to_bin(SourceId), 
    TargetBinId = util:hex_to_bin(TargetId), 

    player:attack_unit(SourceBinId, TargetBinId),
    <<"Attack unit added">>;

message_handle(<<"harvest">>, Message) ->
    lager:info("message: harvest"),

    Id = map_get(<<"id">>, Message),
    Resource = map_get(<<"resource">>, Message),
   
    player:harvest(Id, Resource),

    <<"Harvest added">>; 

message_handle(_Cmd, Message) ->
    Error = "Unrecognized message", 
    lager:info("~p: ~p~n", [Error, Message]),
    list_to_binary(Error).

prepare(map_perception, Message) ->
    [ExploredTuple, {<<"objs">>, Objs}] = Message,
    NewObjs = convert_id(Objs, []),
    [ExploredTuple, {<<"objs">>, NewObjs}];

prepare(battle_perception, Message) ->
    BattlePerception = battle_perception(Message, []),
    [{<<"units">>, BattlePerception}];

prepare(battle, Message) ->
    SourceId = maps:get(<<"sourceid">>, Message),
    TargetId = maps:get(<<"targetid">>, Message),

    SourceHexId = util:bin_to_hex(SourceId),
    TargetHexId = util:bin_to_hex(TargetId),

    NewMessage1 = maps:put(<<"sourceid">>, SourceHexId, Message),
    NewMessage2 = maps:put(<<"targetid">>, TargetHexId, NewMessage1),
    NewMessage2.

json_decode(Data) ->
    try jsx:decode(Data, [return_maps])
    catch
        _:_ ->
            lager:info("Error json_decode")
    end.
 
convert_id([], ConvertedIds) ->
    ConvertedIds;
convert_id([Obj | Rest], ConvertedIds) ->
    
    BinId = maps:get(<<"id">>, Obj),
    HexId = util:bin_to_hex(BinId),
    NewObj = maps:update(<<"id">>, HexId, Obj),
    NewConvertedIds = [NewObj | ConvertedIds],

    convert_id(Rest, NewConvertedIds).

battle_perception([], NewPerception) ->
    NewPerception;

battle_perception([Unit | Rest], NewPerception) ->
    NewUnit = mdb:to_map(Unit),
    battle_perception(Rest, [NewUnit | NewPerception]).
