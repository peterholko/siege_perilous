% Author: Peter
%% Created: Nov 11, 2014
%% Description: Handles messages from client
-module(message).

-export([decode/1, prepare/2, convert_battle_id/2]).

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
    
    X = map_get(<<"x">>, Message),
    Y = map_get(<<"y">>, Message),
    Result = player:move_obj(BinId, {X, Y}),
    <<"Move added">>;

message_handle(<<"move_unit">>, Message) ->
    lager:info("message: move_unit"),
    HexId = map_get(<<"id">>, Message),
    BinId = util:hex_to_bin(HexId),
    
    X = map_get(<<"x">>, Message),
    Y = map_get(<<"y">>, Message),
    Result = player:move_unit(BinId, {X, Y}),
    <<"Move unit added">>;

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

message_handle(<<"explore">>, Message) ->
    lager:info("message: explore"),

    HexId = map_get(<<"sourceid">>, Message),
    BinId = util:hex_to_bin(HexId),
    
    X = map_get(<<"x">>, Message),
    Y = map_get(<<"y">>, Message),

    {LocalMap, LocalObjs} = player:explore(BinId, {X, Y}),
    LocalPerception = [{<<"packet">>, <<"explore">>},
                       {<<"explored">>, LocalMap},
                       {<<"objs">>, convert_id(LocalObjs, [])}],
    jsx:encode(LocalPerception);

message_handle(<<"exit_local">>, _Message) ->
    lager:info("message: exit_local"),
    player:exit_local(),

    <<"Exit added">>;

message_handle(<<"build">>, Message) ->
    lager:info("message: build"),
    
    HexId = map_get(<<"sourceid">>, Message),
    BinId = util:hex_to_bin(HexId),
    X = map_get(<<"x">>, Message),
    Y = map_get(<<"y">>, Message),
    Structure = map_get(<<"structure">>, Message),

    player:build(BinId, {X, Y}, Structure),

    <<"Structure queued">>;

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

message_handle(<<"info_item">>, Message) ->
    lager:info("message: info_item"),
    HexId = map_get(<<"id">>, Message),
    BinId = util:hex_to_bin(HexId),
    InfoMaps = mdb:to_map(player:get_info_item(BinId)),
    ReturnMsg = maps:put(<<"packet">>, <<"info_item">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_battle">>, Message) ->
    lager:info("message: info_battle"),
    HexId = map_get(<<"id">>, Message),
    BinId = util:hex_to_bin(HexId),
    
    ReturnMsg = case player:get_info_battle(BinId) of
                    {battle_perception, BattlePerception} ->
                        prepare(battle_perception, BattlePerception);
                    {obj_info, ObjInfo} ->
                        InfoMaps = mdb:to_map(ObjInfo),
                        maps:put(<<"packet">>, <<"info_obj">>, InfoMaps)
                end,

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

prepare(local_perception, Message) ->
    [ExploredTuple, {<<"objs">>, Objs}] = Message,
    NewObjs = convert_id(Objs, []),
    [{<<"packet">>, <<"local_perception">>},
     {<<"objs">>, NewObjs},
     ExploredTuple];
    
prepare(battle_perception, Message) ->
    {BattleUnits, BattleMap} = Message,
    ConvertBattleUnits = convert_battle_id(BattleUnits, []),
    [{<<"packet">>, <<"battle_perception">>},
     {<<"map">>, BattleMap},
     {<<"units">>, ConvertBattleUnits}];

prepare(item_perception, Message) ->
    ItemPerception = item_perception(Message, []),
    [{<<"packet">>, <<"item_perception">>},
     {<<"items">>, ItemPerception}]; 

prepare(exit_local, _Message) ->
    [{<<"packet">>, <<"exit_local">>}];

prepare(_MessageType, Message) ->
    Message.

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

convert_local_id([], ConvertedIds) ->
    ConvertedIds;
convert_local_id([Obj | Rest], ConvertedIds) ->
    BinGlobalId = maps:get(<<"id">>, Obj),
    HexGlobalId = util:bin_to_hex(BinGlobalId),
    BinId = maps:get(<<"id">>, Obj),
    HexId = util:bin_to_hex(BinId),
    NewObj1 = maps:update(<<"global_id">>, HexGlobalId, Obj),
    NewObj2 = maps:update(<<"id">>, HexId, NewObj1),
    NewConvertedIds = [NewObj2 | ConvertedIds],

    convert_local_id(Rest, NewConvertedIds).

convert_battle_id([], ConvertedIds) ->
    ConvertedIds;
convert_battle_id([Unit | Rest], ConvertedIds) ->
    BinUnitId = maps:get(<<"unit">>, Unit),
    BinObjId = maps:get(<<"obj">>, Unit),
    HexUnitId = util:bin_to_hex(BinUnitId),
    HexObjId = util:bin_to_hex(BinObjId),
    NewUnit1 = maps:update(<<"unit">>, HexUnitId, Unit),
    NewUnit2 = maps:update(<<"obj">>, HexObjId, NewUnit1),
    NewConvertedIds = [NewUnit2 | ConvertedIds],

    convert_battle_id(Rest, NewConvertedIds).

item_perception([], ItemPerception) ->
    ItemPerception;
item_perception([Item | Rest] , ItemPerception) ->
    NewItem = mdb:to_map(Item),
    item_perception(Rest, [NewItem | ItemPerception]).

