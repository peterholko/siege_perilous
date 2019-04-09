%% Author: Peter
%% Created: Nov 11, 2014
%% Description: Handles messages from client
-module(message).

-export([send_to_process/3]).
-export([decode/1, prepare/2]).

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

    setup:login(Username, Password, self());

message_handle(<<"move_unit">>, Message) ->
    lager:info("message: move_unit"),
    Id = map_get(<<"id">>, Message),
    
    X = map_get(<<"x">>, Message),
    Y = map_get(<<"y">>, Message),

    Return = player:move(Id, {X, Y}),

    FinalReturn = maps:put(<<"packet">>, <<"move">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"combo">>, Message) ->
    lager:info("message:combo"),

    SourceId = map_get(<<"sourceid">>, Message),
    ComboType = map_get(<<"combotype">>, Message),  

    Return = player:combo(SourceId, ComboType),

    FinalReturn = maps:put(<<"packet">>, <<"combo">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"attack">>, Message) ->
    lager:info("message: attack"),

    AttackType = map_get(<<"attacktype">>, Message),
    SourceId = map_get(<<"sourceid">>, Message),
    TargetId = map_get(<<"targetid">>, Message),

    Return = player:attack(AttackType, SourceId, TargetId),

    FinalReturn = maps:put(<<"packet">>, <<"attack">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"defend">>, Message) ->
    lager:info("message: defend"),
    DefendType = map_get(<<"defendtype">>, Message),
    SourceId = map_get(<<"sourceid">>, Message),

    Return = player:defend(DefendType, SourceId),

    FinalReturn = maps:put(<<"packet">>, <<"defend">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"survey">>, Message) ->
    lager:info("message: survey"),

    SourceId = map_get(<<"sourceid">>, Message),    
    Data = player:survey(SourceId),
    jsx:encode(#{<<"packet">> => <<"survey">>,
                 <<"data">> => Data});    

message_handle(<<"explore">>, Message) ->
    lager:info("message: explore"),

    SourceId = map_get(<<"sourceid">>, Message),    
    Result = player:explore(SourceId),
    jsx:encode([{<<"packet">>, <<"explore">>},
                {<<"result">>, Result}]);    

message_handle(<<"harvest">>, Message) ->
    lager:info("message: harvest"),

    SourceId = map_get(<<"sourceid">>, Message),
    Resource = map_get(<<"resource">>, Message),
   
    Return = player:harvest(SourceId, Resource),

    FinalReturn = maps:put(<<"packet">>, <<"harvest">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"loot">>, Message) ->
    lager:info("message: loot"),
    
    SourceId = map_get(<<"sourceid">>, Message),
    ItemId = map_get(<<"item">>, Message),

    LootData = player:loot(SourceId, ItemId),
    LootPerception = prepare(loot_perception, LootData),
    jsx:encode(LootPerception);

message_handle(<<"item_transfer">>, Message) ->
    lager:info("message: item_transfer ~p", [Message]),
    
    TargetId = map_get(<<"targetid">>, Message),
    ItemId = map_get(<<"item">>, Message),

    Return = player:item_transfer(TargetId, ItemId),
    FinalReturn = maps:put(<<"packet">>, <<"item_transfer">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"item_split">>, Message) ->
    lager:info("message: item_split ~p", [Message]),

    ItemId = map_get(<<"item">>, Message),

    QuantityStr = map_get(<<"quantity">>, Message),
    Quantity = binary_to_integer(QuantityStr),

    Result = player:item_split(ItemId, Quantity),

    jsx:encode([{<<"packet">>, <<"item_split">>},
                {<<"result">>, Result}]);

message_handle(<<"structure_list">>, _Message) ->
    lager:info("message: structure_list"),

    Structures = player:structure_list(),

    jsx:encode([{<<"packet">>, <<"structure_list">>},
                {<<"result">>, Structures}]);    

message_handle(<<"build">>, Message) ->
    lager:info("message: build"),
    
    Id = map_get(<<"sourceid">>, Message),    
    StructureId = map_get(<<"structure">>, Message),

    Return = player:build(Id, StructureId),

    FinalReturn = maps:put(<<"packet">>, <<"build">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"upgrade">>, Message) ->
    lager:info("message: upgrade"),
    
    StructureId = map_get(<<"structure">>, Message),

    Result = player:upgrade(StructureId),

    jsx:encode([{<<"packet">>, <<"upgrade">>},
                {<<"result">>, Result}]);

message_handle(<<"finish_build">>, Message) ->
    lager:info("message: finish_build"),
    
    SourceId = map_get(<<"sourceid">>, Message),
    StructureId = map_get(<<"structureid">>, Message),

    Return = player:finish_build(SourceId, StructureId),
    lager:info("Finish build return: ~p", [Return]),
    FinalReturn = maps:put(<<"packet">>, <<"finish_build">>, Return),
    lager:info("FinalReturn: ~p", [FinalReturn]),
    jsx:encode(FinalReturn);

message_handle(<<"recipe_list">>, Message) ->
    lager:info("message: recipe_list"),
    
    SourceId = map_get(<<"sourceid">>, Message),

    RecipeList = player:recipe_list(SourceId),

    jsx:encode([{<<"packet">>, <<"recipe_list">>},
                {<<"result">>, RecipeList}]);

message_handle(<<"refine">>, Message) ->
    lager:info("message: refine"),
    StructureId = map_get(<<"structureid">>, Message),

    Reply = player:refine(StructureId),

    jsx:encode([{<<"packet">>, <<"refine">>},
                {<<"reply">>, Reply}]);

message_handle(<<"craft">>, Message) ->
    lager:info("message: craft"),
    SourceId = map_get(<<"sourceid">>, Message),
    Recipe = map_get(<<"recipe">>, Message),

    Result = player:craft(SourceId, Recipe),

    jsx:encode([{<<"packet">>, <<"craft">>},
                {<<"result">>, Result}]);

message_handle(<<"equip">>, Message) ->
    lager:info("message: equip"),
    ItemId = map_get(<<"item">>, Message),
    Result = player:equip(ItemId),

    jsx:encode([{<<"packet">>, <<"equip">>},
                {<<"result">>, Result}]);    

message_handle(<<"unequip">>, Message) ->
    lager:info("message: unequip"),
    ItemId = map_get(<<"item">>, Message),
    Result = player:unequip(ItemId),

    jsx:encode([{<<"packet">>, <<"unequip">>},
                {<<"result">>, Result}]);    

message_handle(<<"rest">>, Message) ->
    lager:info("message: rest"),

    SourceId = map_get(<<"sourceid">>, Message),

    Return = player:rest(SourceId),
    FinalReturn = maps:put(<<"packet">>, <<"rest">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"hide">>, Message) ->
    lager:info("message: hide"),

    SourceId = map_get(<<"sourceid">>, Message),

    Return = player:hide(SourceId),
    FinalReturn = maps:put(<<"packet">>, <<"hide">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"assign_list">>, _Message) ->
    lager:info("message: assign_list"),
    
    AssignList = player:assign_list(),
    jsx:encode([{<<"packet">>, <<"assign_list">>},
                {<<"result">>, AssignList}]);

message_handle(<<"assign">>, Message) ->
    lager:info("message: assign"),
    
    SourceId = map_get(<<"sourceid">>, Message),
    TargetId = map_get(<<"targetid">>, Message),

    Return = player:assign(SourceId, TargetId),
    FinalReturn = maps:put(<<"packet">>, <<"assign">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"order_follow">>, Message) ->
    lager:info("message: order_follow"),
    
    SourceId = map_get(<<"sourceid">>, Message),

    Return = player:order_follow(SourceId),
    FinalReturn = maps:put(<<"packet">>, <<"order_follow">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"order_explore">>, Message) ->
    lager:info("message: order_explore"),
    
    SourceId = map_get(<<"sourceid">>, Message),

    Return = player:order_explore(SourceId),
    FinalReturn = maps:put(<<"packet">>, <<"order_explore">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"order_harvest">>, Message) ->
    lager:info("message: order_harvest"),
    
    SourceId = map_get(<<"sourceid">>, Message),

    Return = player:order_harvest(SourceId),
    FinalReturn = maps:put(<<"packet">>, <<"order_harvest">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"order_gather">>, Message) ->
    lager:info("message: order_gather"),
    
    SourceId = map_get(<<"sourceid">>, Message),
    ResourceType = map_get(<<"restype">>, Message),

    Return = player:order_gather(SourceId, ResourceType),
    FinalReturn = maps:put(<<"packet">>, <<"order_gather">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"order_attack">>, Message) ->
    lager:info("message: order_attack"),
    
    SourceId = map_get(<<"sourceid">>, Message),
    TargetId = map_get(<<"targetid">>, Message),

    Return = player:order_attack(SourceId, TargetId),
    FinalReturn = maps:put(<<"packet">>, <<"order_attack">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"order_build">>, Message) ->
    lager:info("message: order_build"),

    SourceId = map_get(<<"sourceid">>, Message),
    TargetId = map_get(<<"targetid">>, Message),

    Return = player:order_build(SourceId, TargetId),
    FinalReturn = maps:put(<<"packet">>, <<"order_build">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"clear">>, Message) ->
    lager:info("message: clear"),
    SourceId = map_get(<<"sourceid">>, Message),

    Return = player:clear(SourceId),
    FinalReturn = maps:put(<<"packet">>, <<"clear">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"cancel">>, Message) ->
    lager:info("message: cancel"),
    SrcId = map_get(<<"sourceid">>, Message),
 
    Result = player:cancel(SrcId),

    jsx:encode([{<<"packet">>, <<"cancel">>},
                {<<"result">>, Result}]);    

message_handle(<<"get_stats">>, Message) ->
    lager:info("message: stats"),
    Id = map_get(<<"id">>, Message),
    Stats = player:get_stats(Id),
    
    jsx:encode([{<<"packet">>, <<"stats">>},
                {<<"data">>, Stats}]);

message_handle(<<"info_tile">>, Message) ->
    lager:info("message: info_tile"),
    X = map_get(<<"x">>, Message),
    Y = map_get(<<"y">>, Message),
    InfoMaps = player:get_info_tile({X, Y}),
    ReturnMsg = maps:put(<<"packet">>, <<"info_tile">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_unit">>, Message) ->
    lager:info("message: info_unit"),
    Id = map_get(<<"id">>, Message),
    InfoMaps = player:get_info_unit(Id),
    ReturnMsg = maps:put(<<"packet">>, <<"info_unit">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_item">>, Message) ->
    lager:info("message: info_item"),
    Id = map_get(<<"id">>, Message),
    InfoMaps = player:get_info_item(Id),
    ReturnMsg = maps:put(<<"packet">>, <<"info_item">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_item_by_name">>, Message) ->
    lager:info("message: info_item_by_name"),
    ItemName = map_get(<<"name">>, Message),
    InfoMaps = player:get_info_item_name(ItemName),
    ReturnMsg = maps:put(<<"packet">>, <<"info_item">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"ford">>, Message) ->
    lager:info("message: ford"),
    Id = map_get(<<"id">>, Message),
    X = map_get(<<"x">>, Message),
    Y = map_get(<<"y">>, Message),
    Return = player:ford(Id, {X, Y}),
    FinalReturn = maps:put(<<"packet">>, <<"ford">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"revent_response">>, Message) ->
    lager:info("message: revent"),
    ResponseNum = map_get(<<"response_num">>, Message),
    Return = player:revent_response(ResponseNum),
    FinalReturn = maps:put(<<"packet">>, <<"revent_resolution">>, Return),
    jsx:encode(FinalReturn);

message_handle(_Cmd, Message) ->
    Error = "Unrecognized message", 
    lager:info("~p: ~p~n", [Error, Message]),
    list_to_binary(Error).

prepare(new_obj_info, Message) ->
    maps:put(<<"packet">>, <<"info_unit">>, Message);

prepare(map_perception, Message) ->
    [ExploredTuple, {<<"objs">>, Objs}] = Message,
    [{<<"packet">>, <<"map_perception">>},
     {<<"objs">>, Objs},
     ExploredTuple];

prepare(perception, Message) ->
    lager:info("Message perception: ~p", [Message]),
    [{<<"packet">>, <<"perception">>},
     {<<"data">>, Message}];
    
prepare(loot_perception, {ObjId, Items}) ->    
    [{<<"packet">>, <<"loot_perception">>},
     {<<"obj">>, ObjId},
     {<<"items">>, Items}]; 

prepare(map, Message) ->
    #{<<"packet">> => <<"map">>,
      <<"data">> => Message};

prepare(new_items, Message) ->
    #{<<"packet">> => <<"new_items">>,
      <<"data">> => Message}; 

prepare(stats, Message) ->
    #{<<"packet">> => <<"stats">>,
      <<"data">> => Message};

prepare(effect, Message) ->
    maps:put(<<"packet">>, <<"effect">>, Message);

prepare(info_tile_update, Message) ->
    maps:put(<<"packet">>, <<"info_tile_update">>, Message);

prepare(info_effect_update, Message) ->
    maps:put(<<"packet">>, <<"info_effect_update">>, Message);

prepare(info_item_update, Message) ->
    maps:put(<<"packet">>, <<"info_item_update">>, Message);

prepare(info_item_transfer, Message) ->
    maps:put(<<"packet">>, <<"info_item_transfer">>, Message);

prepare(villager_change, Message) ->
    maps:put(<<"packet">>, <<"villager_change">>, Message);

prepare(survey, Message) ->
    #{<<"packet">> => <<"survey">>,
      <<"data">> => Message};

prepare(revent, Message) ->
    lager:info("Sending revent: ~p", [Message]),
    maps:put(<<"packet">>, <<"revent">>, Message);

prepare(world, Message) ->
    maps:put(<<"packet">>, <<"world">>, Message);

prepare(events, Message) ->
    lager:info("Sending events: ~p", [Message]),
    #{<<"packet">> => <<"events">>,
      <<"data">> => Message};

prepare(event_complete, {Event, Id}) ->
    player:set_event_lock(Id, false),
    #{<<"packet">> => <<"event_complete">>,
      <<"event">> => atom_to_binary(Event, latin1)};

prepare(event_cancel, {Event, Id}) ->
    player:set_event_lock(Id, false),
    #{<<"packet">> => <<"event_cancel">>,
      <<"event">> => atom_to_binary(Event, latin1)};

prepare(event_failure, {Event, Error}) ->
    [{<<"packet">>, atom_to_binary(Event, latin1)},
     {<<"error">>, atom_to_binary(Error, latin1)}];

prepare(MessageType, Message) ->
    lager:debug("Message Type: ~p ~p", [MessageType, Message]),
    Message.

json_decode(Data) ->
    try jsx:decode(Data, [return_maps])
    catch
        _:_ ->
            lager:info("Error json_decode")
    end.
 
