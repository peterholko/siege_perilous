% Author: Peter
%% Created: Nov 11, 2014
%% Description: Handles messages from client
-module(message).

-export([decode/1]).

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
            ExploredTiles = player:get_perception(PlayerId),
            lager:info("Tiles: ~p", [ExploredTiles]),
            jsx:encode(ExploredTiles)
    end;

message_handle(<<"info">>, Message) ->
    lager:info("message: info"),
    Id = map_get(<<"id">>, Message),
    BinaryId = list_to_binary(Id),

    Obj = player:get_info(BinaryId),
    lager:info("Obj: ~p", [Obj]),
    jsx:encode(bson:fields(Obj));

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
