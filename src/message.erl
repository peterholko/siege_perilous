%% Author: Peter
%% Created: Nov 11, 2014
%% Description: Handles messages from client
-module(message).

-export([send_to_process/3]).
-export([decode/1, prepare/2]).

-include("schema.hrl").

send_to_process(Process, MessageType, Message) when is_pid(Process) ->
    lager:debug("Sending ~p to ~p", [Message, Process]),
    Process ! {MessageType, Message};

send_to_process(_, _, _) ->
    none.

decode(Message) ->
    lager:info("Message: ~p~n", [Message]),

    Decoded = json_decode(Message),
    lager:info("Decoded: ~p~n", [Decoded]),

    Cmd = m_get(<<"cmd">>, Decoded),
    message_handle(Cmd, Decoded).

m_get(Key, Map) ->
    try maps:get(Key, Map)
    catch
        _:_ ->
            lager:info("Invalid Map Key: ~p~n", [Key]),
            none
    end.

message_handle(<<"login">>, Message) -> 
    lager:info("message: login"),
    Username = m_get(<<"username">>, Message),
    Password = m_get(<<"password">>, Message),

    lager:info("Username: ~p~n", [Username]),
    lager:info("Password: ~p~n", [Password]),

    setup:login(Username, Password, self());

message_handle(<<"select_class">>, Message) ->
    lager:info("message: select_class"),
    Class = m_get(<<"classname">>, Message),
    setup:select_class(Class);

message_handle(<<"image_def">>, Message) ->
    RawImageName = m_get(<<"name">>, Message),
    ImageName = re:replace(RawImageName, "[0-9]+", "", [{return, binary}]),

    Return = case db:read(image_def, ImageName) of 
                [ImageDef] -> 
                      Data1 = maps:put(<<"packet">>, <<"image_def">>, #{}),
                      Data2 = maps:put(<<"name">>, RawImageName, Data1),
                      Data3 = maps:put(<<"data">>, ImageDef#image_def.data, Data2),
                      Data3;
                _ -> 
                    #{<<"packet">> => <<"image_def">>,
                      <<"name">> => RawImageName,
                      <<"result">> => <<"404">>}
            end,

    jsx:encode(Return);

message_handle(<<"move_unit">>, Message) ->
    lager:info("message: move_unit"),
    Id = m_get(<<"id">>, Message),
    
    X = m_get(<<"x">>, Message),
    Y = m_get(<<"y">>, Message),

    Return = player:move(Id, {X, Y}),

    FinalReturn = maps:put(<<"packet">>, <<"move">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"combo">>, Message) ->
    lager:info("message:combo"),

    SourceId = m_get(<<"sourceid">>, Message),
    ComboType = m_get(<<"combotype">>, Message),  

    Return = player:combo(SourceId, ComboType),

    FinalReturn = maps:put(<<"packet">>, <<"combo">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"attack">>, Message) ->
    lager:info("message: attack"),

    AttackType = m_get(<<"attacktype">>, Message),
    SourceId = m_get(<<"sourceid">>, Message),
    TargetId = m_get(<<"targetid">>, Message),

    Return = player:attack(AttackType, SourceId, TargetId),

    FinalReturn = maps:put(<<"packet">>, <<"attack">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"defend">>, Message) ->
    lager:info("message: defend"),
    DefendType = m_get(<<"defendtype">>, Message),
    SourceId = m_get(<<"sourceid">>, Message),

    Return = player:defend(DefendType, SourceId),

    FinalReturn = maps:put(<<"packet">>, <<"defend">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"survey">>, Message) ->
    lager:info("message: survey"),

    SourceId = m_get(<<"sourceid">>, Message),    
    Data = player:survey(SourceId),
    jsx:encode(#{<<"packet">> => <<"survey">>,
                 <<"data">> => Data});    

message_handle(<<"explore">>, Message) ->
    lager:info("message: explore"),

    SourceId = m_get(<<"sourceid">>, Message),    
    Result = player:explore(SourceId),
    jsx:encode([{<<"packet">>, <<"explore">>},
                {<<"result">>, Result}]);    

message_handle(<<"harvest">>, Message) ->
    lager:info("message: harvest"),

    SourceId = m_get(<<"sourceid">>, Message),
    Resource = m_get(<<"resource">>, Message),
   
    Return = player:harvest(SourceId, Resource),

    FinalReturn = maps:put(<<"packet">>, <<"harvest">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"loot">>, Message) ->
    lager:info("message: loot"),
    
    SourceId = m_get(<<"sourceid">>, Message),
    ItemId = m_get(<<"item">>, Message),

    LootData = player:loot(SourceId, ItemId),
    LootPerception = prepare(loot_perception, LootData),
    jsx:encode(LootPerception);

message_handle(<<"item_transfer">>, Message) ->
    lager:info("message: item_transfer ~p", [Message]),
    
    TargetId = m_get(<<"targetid">>, Message),
    ItemId = m_get(<<"item">>, Message),

    Return = player:item_transfer(TargetId, ItemId),
    FinalReturn = maps:put(<<"packet">>, <<"item_transfer">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"item_split">>, Message) ->
    lager:info("message: item_split ~p", [Message]),

    ItemId = m_get(<<"item">>, Message),
    Quantity = m_get(<<"quantity">>, Message),

    Return = player:item_split(ItemId, Quantity),
    FinalReturn = maps:put(<<"packet">>, <<"item_split">>, Return),

    jsx:encode(FinalReturn);

message_handle(<<"structure_list">>, _Message) ->
    lager:info("message: structure_list"),

    Structures = player:structure_list(),

    jsx:encode([{<<"packet">>, <<"structure_list">>},
                {<<"result">>, Structures}]);    

message_handle(<<"create_foundation">>, Message) ->
    lager:info("message: build"),
    
    Id = m_get(<<"sourceid">>, Message),    
    StructureId = m_get(<<"structure">>, Message),

    Return = player:create_foundation(Id, StructureId),

    FinalReturn = maps:put(<<"packet">>, <<"create_foundation">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"upgrade">>, Message) ->
    lager:info("message: upgrade"),
    
    StructureId = m_get(<<"structure">>, Message),

    Result = player:upgrade(StructureId),

    jsx:encode([{<<"packet">>, <<"upgrade">>},
                {<<"result">>, Result}]);

message_handle(<<"build">>, Message) ->
    lager:info("message: build"),
    
    SourceId = m_get(<<"sourceid">>, Message),
    StructureId = m_get(<<"structureid">>, Message),

    Return = player:build(SourceId, StructureId),
    lager:info("Build return: ~p", [Return]),
    FinalReturn = maps:put(<<"packet">>, <<"build">>, Return),
    lager:info("FinalReturn: ~p", [FinalReturn]),
    jsx:encode(FinalReturn);

message_handle(<<"recipe_list">>, Message) ->
    lager:info("message: recipe_list"),
    
    SourceId = m_get(<<"structureid">>, Message),

    RecipeList = player:recipe_list(SourceId),

    jsx:encode([{<<"packet">>, <<"recipe_list">>},
                {<<"result">>, RecipeList}]);

message_handle(<<"order_refine">>, Message) ->
    lager:info("message: order_refine"),
    StructureId = m_get(<<"structureid">>, Message),

    Reply = player:order_refine(StructureId),

    jsx:encode([{<<"packet">>, <<"order_refine">>},
                {<<"result">>, Reply}]);

message_handle(<<"order_craft">>, Message) ->
    lager:info("message: order_craft"),
    SourceId = m_get(<<"sourceid">>, Message),
    Recipe = m_get(<<"recipe">>, Message),

    Result = player:order_craft(SourceId, Recipe),

    jsx:encode([{<<"packet">>, <<"order_craft">>},
                {<<"result">>, Result}]);

message_handle(<<"equip">>, Message) ->
    lager:info("message: equip"),
    ItemId = m_get(<<"item">>, Message),
    Result = player:equip(ItemId),

    jsx:encode([{<<"packet">>, <<"equip">>},
                {<<"result">>, Result}]);    

message_handle(<<"unequip">>, Message) ->
    lager:info("message: unequip"),
    ItemId = m_get(<<"item">>, Message),
    Result = player:unequip(ItemId),

    jsx:encode([{<<"packet">>, <<"unequip">>},
                {<<"result">>, Result}]);    

message_handle(<<"rest">>, Message) ->
    lager:info("message: rest"),

    SourceId = m_get(<<"sourceid">>, Message),

    Return = player:rest(SourceId),
    FinalReturn = maps:put(<<"packet">>, <<"rest">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"hide">>, Message) ->
    lager:info("message: hide"),

    SourceId = m_get(<<"sourceid">>, Message),

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
    
    SourceId = m_get(<<"sourceid">>, Message),
    TargetId = m_get(<<"targetid">>, Message),

    Return = player:assign(SourceId, TargetId),
    FinalReturn = maps:put(<<"packet">>, <<"assign">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"order_follow">>, Message) ->
    lager:info("message: order_follow"),
    
    SourceId = m_get(<<"sourceid">>, Message),

    Return = player:order_follow(SourceId),
    FinalReturn = maps:put(<<"packet">>, <<"order_follow">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"order_explore">>, Message) ->
    lager:info("message: order_explore"),
    
    SourceId = m_get(<<"sourceid">>, Message),

    Return = player:order_explore(SourceId),
    FinalReturn = maps:put(<<"packet">>, <<"order_explore">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"order_harvest">>, Message) ->
    lager:info("message: order_harvest"),
    
    SourceId = m_get(<<"sourceid">>, Message),

    Return = player:order_harvest(SourceId),
    FinalReturn = maps:put(<<"packet">>, <<"order_harvest">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"order_gather">>, Message) ->
    lager:info("message: order_gather"),
    
    SourceId = m_get(<<"sourceid">>, Message),
    ResourceType = m_get(<<"restype">>, Message),

    Return = player:order_gather(SourceId, ResourceType),
    FinalReturn = maps:put(<<"packet">>, <<"order_gather">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"order_attack">>, Message) ->
    lager:info("message: order_attack"),
    
    SourceId = m_get(<<"sourceid">>, Message),
    TargetId = m_get(<<"targetid">>, Message),

    Return = player:order_attack(SourceId, TargetId),
    FinalReturn = maps:put(<<"packet">>, <<"order_attack">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"order_build">>, Message) ->
    lager:info("message: order_build"),

    SourceId = m_get(<<"sourceid">>, Message),
    TargetId = m_get(<<"targetid">>, Message),

    Return = player:order_build(SourceId, TargetId),
    FinalReturn = maps:put(<<"packet">>, <<"order_build">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"order_experiment">>, Message) ->
    lager:info("message: order_experiment"),

    StructureId = m_get(<<"structureid">>, Message),

    Return = player:order_experiment(StructureId),
    FinalReturn = maps:put(<<"packet">>, <<"order_experiment">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"clear">>, Message) ->
    lager:info("message: clear"),
    SourceId = m_get(<<"sourceid">>, Message),

    Return = player:clear(SourceId),
    FinalReturn = maps:put(<<"packet">>, <<"clear">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"cancel">>, Message) ->
    lager:info("message: cancel"),
    SrcId = m_get(<<"sourceid">>, Message),
 
    Result = player:cancel(SrcId),

    jsx:encode([{<<"packet">>, <<"cancel">>},
                {<<"result">>, Result}]);    

message_handle(<<"get_stats">>, Message) ->
    lager:info("message: stats"),
    Id = m_get(<<"id">>, Message),
    Stats = player:get_stats(Id),
    
    jsx:encode([{<<"packet">>, <<"stats">>},
                {<<"data">>, Stats}]);

message_handle(<<"info_tile">>, Message) ->
    lager:info("message: info_tile"),
    X = m_get(<<"x">>, Message),
    Y = m_get(<<"y">>, Message),
    InfoMaps = player:get_info_tile({X, Y}),
    ReturnMsg = maps:put(<<"packet">>, <<"info_tile">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_unit">>, Message) ->
    lager:info("message: info_unit"),
    Id = m_get(<<"id">>, Message),
    InfoMaps = player:get_info_unit(Id),
    ReturnMsg = maps:put(<<"packet">>, <<"info_unit">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_item">>, Message) ->
    lager:info("message: info_item"),
    Id = m_get(<<"id">>, Message),
    InfoMaps = player:get_info_item(Id),
    ReturnMsg = maps:put(<<"packet">>, <<"info_item">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_item_by_name">>, Message) ->
    lager:info("message: info_item_by_name"),
    ItemName = m_get(<<"name">>, Message),
    InfoMaps = player:get_info_item_name(ItemName),
    ReturnMsg = maps:put(<<"packet">>, <<"info_item">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_inventory">>, Message) ->
    lager:info("message: info_inventory"),
    Id = m_get(<<"id">>, Message),
    InfoMaps = player:get_info_inventory(Id),
    ReturnMsg = maps:put(<<"packet">>, <<"info_inventory">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_attrs">>, Message) ->
    lager:info("message: info_attrs"),
    Id = m_get(<<"id">>, Message),
    InfoMaps = player:get_info_attrs(Id),
    ReturnMsg = maps:put(<<"packet">>, <<"info_attrs">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_skills">>, Message) ->
    lager:info("message: info_skills"),
    Id = m_get(<<"id">>, Message),
    InfoMaps = player:get_info_skills(Id),
    ReturnMsg = maps:put(<<"packet">>, <<"info_skills">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_item_transfer">>, Message) ->
    lager:info("message: info_item_transfer"),
    SourceId = m_get(<<"sourceid">>, Message),
    TargetId = m_get(<<"targetid">>, Message),
    InfoMaps = player:get_info_item_transfer(SourceId, TargetId),
    ReturnMsg = maps:put(<<"packet">>, <<"info_item_transfer">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_hauling">>, Message) ->
    lager:info("message: info_hauling"),
    SourceId = m_get(<<"sourceid">>, Message),
    InfoMaps = player:get_info_hauling(SourceId),
    ReturnMsg = maps:put(<<"packet">>, <<"info_hauling">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_experiment">>, Message) ->
    lager:info("message: info_experiment"),
    StructureId = m_get(<<"structureid">>, Message),
    InfoMaps = player:get_info_experiment(StructureId),
    ReturnMsg = maps:put(<<"packet">>, <<"info_experiment">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"info_exit">>, Message) ->
    lager:info("message: info_exit"),
    Key = m_get(<<"key">>, Message),
    Type = m_get(<<"type">>, Message),
    InfoMaps = player:info_exit(Key, Type),
    ReturnMsg = maps:put(<<"packet">>, <<"info_exit">>, InfoMaps),
    jsx:encode(ReturnMsg);

message_handle(<<"ford">>, Message) ->
    lager:info("message: ford"),
    Id = m_get(<<"id">>, Message),
    X = m_get(<<"x">>, Message),
    Y = m_get(<<"y">>, Message),
    Return = player:ford(Id, {X, Y}),
    FinalReturn = maps:put(<<"packet">>, <<"ford">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"revent_response">>, Message) ->
    lager:info("message: revent"),
    ResponseNum = m_get(<<"response_num">>, Message),
    Return = player:revent_response(ResponseNum),
    FinalReturn = maps:put(<<"packet">>, <<"revent_resolution">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"tick">>, _Message) ->
    lager:info("message: tick"),
    ReturnMsg = #{<<"packet">> => <<"tick">>,
                  <<"tick">> => game:get_tick()},
    jsx:encode(ReturnMsg);

message_handle(<<"buy_item">>, Message) ->
    lager:info("message: buy_item"),
    ItemId = m_get(<<"itemid">>, Message),
    Quantity = m_get(<<"quantity">>, Message),
    Return = player:buy_item(ItemId, Quantity),
    FinalReturn = maps:put(<<"packet">>, <<"buy_item">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"sell_item">>, Message) ->
    lager:info("message: sell_item"),
    ItemId = m_get(<<"itemid">>, Message),
    TargetId = m_get(<<"targetid">>, Message),
    Quantity = m_get(<<"quantity">>, Message),
    Return = player:sell_item(ItemId, TargetId, Quantity),
    FinalReturn = maps:put(<<"packet">>, <<"sell_item">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"hire">>, Message) ->
    lager:info("message: hire"),
    SourceId = m_get(<<"sourceid">>, Message),
    TargetId = m_get(<<"targetid">>, Message),
    Return = player:hire(SourceId, TargetId),
    FinalReturn = maps:put(<<"packet">>, <<"hire">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"pay_tax">>, Message) ->
    lager:info("message: pay_tax"),
    TaxCollectorId = m_get(<<"taxcollector">>, Message),
    Amount = m_get(<<"amount">>, Message),
    Return = player:pay_tax(TaxCollectorId, Amount),
    FinalReturn = maps:put(<<"packet">>, <<"pay_tax">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"set_exp_item">>, Message) ->
    lager:info("message: set experiment item"),
    ItemId = m_get(<<"itemid">>, Message),

    Return = player:set_exp_item(ItemId),
    FinalReturn = maps:put(<<"packet">>, <<"set_exp_item">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"set_exp_resource">>, Message) ->
    lager:info("message: set experiment resource"),
    ItemId = m_get(<<"itemid">>, Message),

    Return = player:set_exp_resource(ItemId),
    FinalReturn = maps:put(<<"packet">>, <<"set_exp_resource">>, Return),
    jsx:encode(FinalReturn);

message_handle(<<"reset_experiment">>, Message) ->
    lager:info("message: reset experiment"),
    ItemId = m_get(<<"structureid">>, Message),

    Return = player:reset_experiment(ItemId),
    FinalReturn = maps:put(<<"packet">>, <<"info_experiment">>, Return),
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

prepare(item_transfer_result, Message) ->
    maps:put(<<"packet">>, <<"item_transfer_result">>, Message);

prepare(info_experiment, Message) ->
    maps:put(<<"packet">>, <<"info_experiment">>, Message);

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
 
