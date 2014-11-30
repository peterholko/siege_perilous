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
    lager:info("Login"),
    Username = map_get(<<"username">>, Message),
    Password = map_get(<<"password">>, Message),

    lager:info("Username: ~p~n", [Username]),
    lager:info("Password: ~p~n", [Password]);

message_handle(_Cmd, Message) ->
    lager:info("Unrecognized message: ~p~n", [Message]).

json_decode(Data) ->
    try jiffy:decode(Data, [return_maps])
    catch
        _:_ ->
            lager:info("Error json_decode")
    end.
