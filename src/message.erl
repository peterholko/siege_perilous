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

    case login:login(Username, Password, self()) of
        {error, Error} ->
            Error;
        {success, PlayerId} ->
            lager:info("Successful login"),
            %Stored player id in process dict for easy access
            put(player_id, PlayerId),

            %Get init perception 
            {PlayerId, ExploredMap, Objs} = player:init_perception(PlayerId),

            %Check if initial login state requires any special data
            player:init_state(PlayerId),

            Perception = [{<<"packet">>, <<"login">>},
                          {<<"player">>, PlayerId},
                          {<<"map">>, ExploredMap},
                          {<<"objs">>, Objs}],
            jsx:encode(Perception)
    end;

message_handle(<<"move_unit">>, Message) ->
    lager:info("message: move_unit"),
    Id = map_get(<<"id">>, Message),
    
    X = map_get(<<"x">>, Message),
    Y = map_get(<<"y">>, Message),

    Return = player:move(Id, {X, Y}),

    FinalReturn = maps:put(<<"packet">>, <<"move">>, Return),
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
    Result = player:survey(SourceId),
    jsx:encode([{<<"packet">>, <<"survey">>},
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
    
    Structure = map_get(<<"structure">>, Message),

    player:build(Id, Structure),

    <<"Structure started">>;

message_handle(<<"finish_build">>, Message) ->
    lager:info("message: finish_build"),
    
    SourceId = map_get(<<"sourceid">>, Message),
    StructureId = map_get(<<"structureid">>, Message),

    Result = player:finish_build(SourceId, StructureId),
    lager:info("Result: ~p", [Result]),
    jsx:encode([{<<"packet">>, <<"finish_build">>} | Result]);    

message_handle(<<"recipe_list">>, Message) ->
    lager:info("message: recipe_list"),
    
    SourceId = map_get(<<"sourceid">>, Message),

    RecipeList = player:recipe_list(SourceId),

    jsx:encode([{<<"packet">>, <<"recipe_list">>},
                {<<"result">>, RecipeList}]);

message_handle(<<"process_resource">>, Message) ->
    lager:info("message: process_resource"),
    StructureId = map_get(<<"structureid">>, Message),

    Reply = player:process_resource(StructureId),

    jsx:encode([{<<"packet">>, <<"process_resource">>},
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

message_handle(<<"assign">>, Message) ->
    lager:info("message: assign"),
    
    SourceId = map_get(<<"sourceid">>, Message),
    TargetId = map_get(<<"targetid">>, Message),

    Return = player:assign(SourceId, TargetId),
    FinalReturn = maps:put(<<"packet">>, <<"assign">>, Return),
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
                {<<"stats">>, Stats}]);

message_handle(<<"info_tile">>, Message) ->
    lager:info("message: info_tile"),
    Id = map_get(<<"id">>, Message),
    X = map_get(<<"x">>, Message),
    Y = map_get(<<"y">>, Message),
    InfoMaps = player:get_info_tile(Id, {X, Y}),
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
    InfoMaps = player:get_info_item(ItemName),
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

prepare(map_perception, Message) ->
    [ExploredTuple, {<<"objs">>, Objs}] = Message,
    [{<<"packet">>, <<"map_perception">>},
     {<<"objs">>, Objs},
     ExploredTuple];

prepare(perception, Message) ->
    {EntityId, Objs} = Message,
    [{<<"packet">>, <<"perception">>},
     {<<"entity">>, EntityId},
     {<<"objs">>, Objs}];
    
prepare(loot_perception, {ObjId, Items}) ->    
    [{<<"packet">>, <<"loot_perception">>},
     {<<"obj">>, ObjId},
     {<<"items">>, Items}]; 

prepare(map, Message) ->
    [{<<"packet">>, <<"map">>},
     {<<"map">>, Message}];

prepare(new_items, Message) ->
    [{<<"packet">>, <<"new_items">>},
     {<<"items">>, Message}]; 

prepare(stats, Message) ->
    [{<<"packet">>, <<"stats">>},
     {<<"stats">>, Message}];

prepare(revent, Message) ->
    lager:info("Sending revent: ~p", [Message]),
    maps:put(<<"packet">>, <<"revent">>, Message);

prepare(world, Message) ->
    maps:put(<<"packet">>, <<"world">>, Message);

prepare(event_complete, {Event, Id}) ->
    player:set_event_lock(Id, false),
    #{<<"packet">> => <<"event_complete">>,
      <<"event">> => atom_to_binary(Event, latin1)};

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
 
