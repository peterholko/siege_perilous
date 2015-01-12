% Author: Peter
%% Created: Nov 11, 2014
%% Description: Handles messages from client
-module(message).

-export([decode/1]).
-export([fields/1]).

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

            InitPerception = player:init_perception(PlayerId),
            lager:info("Perception: ~p", [InitPerception]),
            jsx:encode(InitPerception)
    end;

message_handle(<<"info">>, Message) ->
    lager:info("message: info"),
    Id = map_get(<<"id">>, Message),

    Fields = jsx:encode(fields(player:get_info(Id))),
    lager:info("Fields: ~p", [Fields]),
    Fields;

message_handle(<<"move">>, Message) ->
    lager:info("message: move"),

    Id = map_get(<<"id">>, Message),
    
    Pos1D = map_get(<<"pos">>, Message),
    Result = player:move_obj(Id, Pos1D),
    <<"Move added">>;   

message_handle(<<"attack">>, Message) ->
    lager:info("message: attack"),

    SourceId = map_get(<<"sourceid">>, Message),
    TargetId = map_get(<<"targetid">>, Message),

    player:attack_obj(SourceId, TargetId),
    <<"Attack added">>;

message_handle(_Cmd, Message) ->
    Error = "Unrecognized message", 
    lager:info("~p: ~p~n", [Error, Message]),
    list_to_binary(Error).

json_decode(Data) ->
    try jsx:decode(Data, [return_maps])
    catch
        _:_ ->
            lager:info("Error json_decode")
    end.

doc_foldr (Fun, Acc, Doc) -> doc_foldrN (Fun, Acc, Doc, 0, tuple_size (Doc) div 2).

doc_foldrN (_, Acc, _, Low, Low) -> Acc;
doc_foldrN (Fun, Acc, Doc, Low, High) ->
    Acc1 = Fun (element (High * 2 - 1, Doc), element (High * 2, Doc), Acc),
    doc_foldrN (Fun, Acc1, Doc, Low, High - 1).
  
%  Convert document to a list of all its fields
fields([]) ->
    [];
fields(Doc) when is_list(Doc) ->
    [D] = Doc, 
    fields(D);
fields(Doc) -> doc_foldr (fun (Label, Value, List) -> [{Label, convert_id(Value)} | List] end, [], Doc).

convert_id(Value) when is_tuple(Value) ->
    convert_bin_id(Value);
convert_id(Value) when is_list(Value) ->
    F = fun(V, Rest) -> [ convert_bin_id(V) | Rest] end,
    lists:foldl(F, [], Value);
convert_id(Value) ->
    Value.

convert_bin_id({Value}) when is_binary(Value) ->
    to_hex(Value, Value);
convert_bin_id(Value) ->
    Value.
    
to_hex(<<_:96>>, Value) ->
    util:bin_to_hex(Value);
to_hex(Value, Value) ->
    Value.
    
