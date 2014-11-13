% Author: Peter
%% Created: Nov 11, 2014
%% Description: Handles messages from client
-module(message).

-export([decode/1]).

decode(Bin) ->
    io:fwrite("JSON: ~p~n", [Bin]),
    NewDecode = case catch jiffy:decode(Bin, [return_maps]) of
                {error, Error} ->
                        io:fwrite("Error: ~p~n", [Error]),
                        [];
                Decode ->
                        Decode
                end,
    io:fwrite("Decode: ~p~n", [NewDecode]),

    Cmd = maps:get(<<"cmd">>, NewDecode),    

    message_handle(Cmd, NewDecode).

message_handle(<<"login">>, Message) -> 

    Username = maps:get(<<"username">>, Message),
    Password = maps:get(<<"password">>, Message),

    io:fwrite("Username: ~p~n", [Username]),
    io:fwrite("Password: ~p~n", [Password]);

message_handle(_Cmd, Message) ->
    io:fwrite("Unrecognized message: ~p~n", [Message]).
