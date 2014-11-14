% Author: Peter
%% Created: Nov 11, 2014
%% Description: Handles messages from client
-module(message).

-export([decode/1]).

decode(Bin) ->
    lager:info("JSON: ~p~n", [Bin]),
    
    Decoded = json_decode(Bin),
    lager:info("Decoded: ~p~n", [Decoded]),

    try
        Cmd = maps:get(<<"cmd">>, Decoded),    
        message_handle(Cmd, Decoded)
    catch
        _:_ ->
            lager:info("Error invalid message")
    end.

message_handle(<<"login">>, Message) -> 
    lager:info("Login"),
    Username = maps:get(<<"username">>, Message),
    Password = maps:get(<<"password">>, Message),

    lager:info("Username: ~p~n", [Username]),
    lager:info("Password: ~p~n", [Password]),

    login:login(Username, Password, self());

message_handle(_Cmd, Message) ->
    lager:info("Unrecognized message: ~p~n", [Message]).

json_decode(Bin) ->
    try jiffy:decode(Bin, [return_maps]) 
    catch 
        _:_ ->
            lager:info("Error json_decode")
    end. 
