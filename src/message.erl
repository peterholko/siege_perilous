%% Author: Peter
%% Created: Nov 11, 2014
%% Description: Handles messages from client
-module(message).

-export([send_to_process/3]).
-export([decode/1, prepare/2]).
-export([to_hex/1]).

send_to_process(Process, MessageType, Message) when is_pid(Process) ->
    lager:debug("Sending ~p to ~p", [Message, Process]),
    Process ! {MessageType, Message};

send_to_process(_, _, _) ->
    none.

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

            {PlayerId, ExploredMap, Objs} = player:init_perception(PlayerId),
            Perception = [{<<"packet">>, <<"login">>},
                          {<<"player">>, PlayerId},
                          {<<"map">>, ExploredMap},
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

    AttackType = map_get(<<"attacktype">>, Message),
    SourceId = map_get(<<"sourceid">>, Message),
    TargetId = map_get(<<"targetid">>, Message),

    SourceBinId = util:hex_to_bin(SourceId), 
    TargetBinId = util:hex_to_bin(TargetId), 

    player:attack(AttackType, SourceBinId, TargetBinId),

    jsx:encode([{<<"packet">>, <<"attack">>},
                {<<"result">>, <<"Attack added">>}]);

message_handle(<<"defend">>, Message) ->
    lager:info("message: defend"),
    DefendType = map_get(<<"defendtype">>, Message),
    SourceId = map_get(<<"sourceid">>, Message),

    SourceBinId = util:hex_to_bin(SourceId), 
    player:defend(DefendType, SourceBinId),

    jsx:encode([{<<"packet">>, <<"Defend">>},
                {<<"result">>, <<"Defend added">>}]);

message_handle(<<"survey">>, Message) ->
    lager:info("message: survey"),

    SourceId = map_get(<<"sourceid">>, Message),    
    SourceBinId = util:hex_to_bin(SourceId), 
    Result = player:survey(SourceBinId),
    jsx:encode([{<<"packet">>, <<"survey">>},
                {<<"result">>, Result}]);    

message_handle(<<"harvest">>, Message) ->
    lager:info("message: harvest"),

    SourceId = map_get(<<"sourceid">>, Message),
    Resource = map_get(<<"resource">>, Message),
    BinSourceId = util:hex_to_bin(SourceId),
   
    player:harvest(BinSourceId, Resource),

    <<"Harvest added">>;

message_handle(<<"loot">>, Message) ->
    lager:info("message: loot"),
    
    SourceId = map_get(<<"sourceid">>, Message),
    SourceBinId = util:hex_to_bin(SourceId),
    Item = map_get(<<"item">>, Message),
    BinItem = util:hex_to_bin(Item),

    PlayerItems = player:loot(SourceBinId, BinItem),
    ItemPerception = prepare(item_perception, PlayerItems),
    jsx:encode(ItemPerception);

message_handle(<<"item_transfer">>, Message) ->
    lager:info("message: item_transfer ~p", [Message]),
    
    TargetId = map_get(<<"targetid">>, Message),
    TargetBinId = util:hex_to_bin(TargetId),
    Item = map_get(<<"item">>, Message),
    BinItem = util:hex_to_bin(Item),

    Result = player:item_transfer(TargetBinId, BinItem),
    lager:info("Result: ~p", [Result]),
    jsx:encode([{<<"packet">>, <<"item_transfer">>},
                {<<"result">>, Result}]);

message_handle(<<"item_split">>, Message) ->
    lager:info("message: item_split ~p", [Message]),

    Item = map_get(<<"item">>, Message),
    BinItem = util:hex_to_bin(Item),

    QuantityStr = map_get(<<"quantity">>, Message),
    Quantity = list_to_integer(binary_to_list(QuantityStr)),

    Result = player:item_split(BinItem, Quantity),

    jsx:encode([{<<"packet">>, <<"item_split">>},
                {<<"result">>, Result}]);

message_handle(<<"structure_list">>, _Message) ->
    lager:info("message: structure_list"),

    Structures = to_hex(player:structure_list()),

    jsx:encode([{<<"packet">>, <<"structure_list">>},
                {<<"result">>, Structures}]);    

message_handle(<<"build">>, Message) ->
    lager:info("message: build"),
    
    HexId = map_get(<<"sourceid">>, Message),
    BinId = util:hex_to_bin(HexId),
    
    Structure = map_get(<<"structure">>, Message),

    player:build(BinId, Structure),

    <<"Structure started">>;

message_handle(<<"finish_build">>, Message) ->
    lager:info("message: finish_build"),
    
    SrcId = map_get(<<"sourceid">>, Message),
    SrcBinId = util:hex_to_bin(SrcId),
    HexId = map_get(<<"structureid">>, Message),
    StructureBinId = util:hex_to_bin(HexId),

    Result = player:finish_build(SrcBinId, StructureBinId),
    lager:info("Result: ~p", [Result]),
    jsx:encode([{<<"packet">>, <<"finish_build">>} | Result]);    

message_handle(<<"recipe_list">>, Message) ->
    lager:info("message: recipe_list"),
    
    HexId = map_get(<<"sourceid">>, Message),
    SourceBinId = util:hex_to_bin(HexId),

    RecipeList = to_hex(player:recipe_list(SourceBinId)),

    jsx:encode([{<<"packet">>, <<"recipe_list">>},
                {<<"result">>, RecipeList}]);

message_handle(<<"process_resource">>, Message) ->
    lager:info("message: process_resource"),
    StructureId = map_get(<<"structureid">>, Message),
    StructureBinId = util:hex_to_bin(StructureId),

    Reply = player:process_resource(StructureBinId),

    jsx:encode([{<<"packet">>, <<"process_resource">>},
                {<<"reply">>, Reply}]);

message_handle(<<"craft">>, Message) ->
    lager:info("message: craft"),

    HexId = map_get(<<"sourceid">>, Message),
    SourceBinId = util:hex_to_bin(HexId),

    Recipe = map_get(<<"recipe">>, Message),

    Result = player:craft(SourceBinId, Recipe),

    jsx:encode([{<<"packet">>, <<"craft">>},
                {<<"result">>, <<"true">>}]);

message_handle(<<"equip">>, Message) ->
    lager:info("message: equip"),

    ItemId = util:hex_to_bin(map_get(<<"item">>, Message)),

    Result = player:equip(ItemId),

    jsx:encode([{<<"packet">>, <<"equip">>},
                {<<"result">>, Result}]);    

message_handle(<<"unequip">>, Message) ->
    lager:info("message: unequip"),

    ItemId = util:hex_to_bin(map_get(<<"item">>, Message)),

    Result = player:unequip(ItemId),

    jsx:encode([{<<"packet">>, <<"unequip">>},
                {<<"result">>, Result}]);    

message_handle(<<"assign">>, Message) ->
    lager:info("message: assign"),
    
    SourceId = util:hex_to_bin(map_get(<<"sourceid">>, Message)),
    TargetId = util:hex_to_bin(map_get(<<"targetid">>, Message)),

    Result = player:assign(SourceId, TargetId),
    
    jsx:encode([{<<"packet">>, <<"assign">>},
                {<<"result">>, Result}]);

message_handle(<<"cancel">>, Message) ->
    lager:info("message: cancel"),
    SrcId = map_get(<<"sourceid">>, Message),
    SrcBinId = util:hex_to_bin(SrcId),
 
    Result = player:cancel(SrcBinId),

    jsx:encode([{<<"packet">>, <<"cancel">>},
                {<<"result">>, Result}]);    

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
    InfoMaps = to_hex(player:get_info_obj(BinId)),
    ReturnMsg = maps:put(<<"packet">>, <<"info_obj">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_unit">>, Message) ->
    lager:info("message: info_unit"),
    HexId = map_get(<<"id">>, Message),
    BinId = util:hex_to_bin(HexId),
    InfoMaps = to_hex(player:get_info_unit(BinId)),
    ReturnMsg = maps:put(<<"packet">>, <<"info_unit">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_item">>, Message) ->
    lager:info("message: info_item"),
    HexId = map_get(<<"id">>, Message),
    BinId = util:hex_to_bin(HexId),
    InfoMaps = to_hex(player:get_info_item(BinId)),
    ReturnMsg = maps:put(<<"packet">>, <<"info_item">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_item_by_name">>, Message) ->
    lager:info("message: info_item_by_name"),
    ItemName = map_get(<<"name">>, Message),
    InfoMaps = to_hex(player:get_info_item(ItemName)),
    ReturnMsg = maps:put(<<"packet">>, <<"info_item">>, InfoMaps),
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

prepare(perception, Message) ->
    {EntityId, Objs} = Message,
    NewObjs = convert_id(Objs, []),
    [{<<"packet">>, <<"perception">>},
     {<<"entity">>, util:bin_to_hex(EntityId)},
     {<<"objs">>, NewObjs}];
    
prepare(item_perception, Message) ->
    [{<<"packet">>, <<"item_perception">>},
     {<<"items">>, to_hex(Message)}]; 

prepare(map, Message) ->
    [{<<"packet">>, <<"map">>},
     {<<"map">>, Message}];

prepare(new_items, Message) ->
    [{<<"packet">>, <<"new_items">>},
     {<<"items">>, to_hex(Message)}]; 

prepare(stats, Message) ->
    [{<<"packet">>, <<"stats">>},
     {<<"stats">>, Message}]; 

prepare(event_complete, {Event, Id}) ->
    player:set_event_lock(Id, false),
    [{<<"packet">>, atom_to_binary(Event, latin1)}];

prepare(event_failure, {Event, Error}) ->
    [{<<"packet">>, atom_to_binary(Event, latin1)},
     {<<"error">>, atom_to_binary(Error, latin1)}];

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

to_hex(MapData) when is_list(MapData) ->
    F = fun(Map, Acc) ->
            [ids_to_hex(Map) | Acc]
        end,
    lists:foldl(F, [], MapData);
to_hex(MapData) when is_map(MapData) ->
    ids_to_hex(MapData).

ids_to_hex(MapData) ->
    F = fun Hex(_K, V) when is_tuple(V) -> util:bin_to_hex(V);
            Hex(_K, V) when is_list(V), length(V) > 0 -> 
                G = fun(Map, Acc) when is_map(Map) ->[maps:map(Hex, Map) | Acc];
                       (Element, Acc) -> [Element |  Acc]
                    end, 
                lists:foldl(G, [], V);
            Hex(_K, V) -> V 
        end,
    maps:map(F, MapData).

