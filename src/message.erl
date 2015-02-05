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
            Perception = [{<<"packet">>, <<"login">>},
                          {<<"player">>, PlayerId},
                          {<<"explored">>, Explored},
                          {<<"objs">>, convert_id(Objs, [])}],
            jsx:encode(Perception)
    end;
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
    BinId = util:hex_to_bin(Id),
   
    player:harvest(BinId, Resource),

    <<"Harvest added">>;

message_handle(<<"equip">>, Message) ->
    lager:info("message: equip"),

    Id = util:hex_to_bin(map_get(<<"id">>, Message)),
    ItemId = util:hex_to_bin(map_get(<<"item">>, Message)),

    player:equip(Id, ItemId),

    <<"Item Equiped">>;

message_handle(<<"info_tile">>, Message) ->
    lager:info("message: info_tile"),
    Pos = map_get(<<"pos">>, Message),
    InfoMaps = player:get_info_tile(Pos),
    ReturnMsg = maps:put(<<"packet">>, <<"info_tile">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_obj">>, Message) ->
    lager:info("message: info_obj"),
    HexId = map_get(<<"id">>, Message),
    BinId = util:hex_to_bin(HexId),
    InfoMaps = mdb:to_map(player:get_info_obj(BinId)),
    ReturnMsg = maps:put(<<"packet">>, <<"info_obj">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_unit">>, Message) ->
    lager:info("message: info_unit"),
    HexId = map_get(<<"id">>, Message),
    BinId = util:hex_to_bin(HexId),
    InfoMaps = mdb:to_map(player:get_info_unit(BinId)),
    ReturnMsg = maps:put(<<"packet">>, <<"info_unit">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(_Cmd, Message) ->
    Error = "Unrecognized message", 
    lager:info("~p: ~p~n", [Error, Message]),
    list_to_binary(Error).

prepare(map_perception, Message) ->
    [ExploredTuple, {<<"objs">>, Objs}] = Message,
    NewObjs = convert_id(Objs, []),
    [{<<"packet">>, <<"map_perception">>},
     {<<"objs">>, NewObjs},
     ExploredTuple];

prepare(battle_perception, Message) ->
    BattlePerception = battle_perception(Message, []),
    [{<<"packet">>, <<"battle_perception">>},
     {<<"units">>, BattlePerception}];

prepare(item_perception, Message) ->
    ItemPerception = item_perception(Message, []),
    [{<<"packet">>, <<"item_perception">>},
     {<<"items">>, ItemPerception}]; 

prepare(battle, Message) ->
    SourceId = maps:get(<<"sourceid">>, Message),
    TargetId = maps:get(<<"targetid">>, Message),

    SourceHexId = util:bin_to_hex(SourceId),
    TargetHexId = util:bin_to_hex(TargetId),

    NewMessage0 = maps:put(<<"packet">>, <<"battle_event">>, Message),
    NewMessage1 = maps:put(<<"sourceid">>, SourceHexId, NewMessage0),
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

item_perception([], ItemPerception) ->
    ItemPerception;
item_perception([Item | Rest] , ItemPerception) ->
    NewItem = mdb:to_map(Item),
    item_perception(Rest, [NewItem | ItemPerception]).

