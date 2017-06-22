-module(ws_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_handle({text, Msg}, Req, State) ->
    Result = message:decode(Msg),
	{reply, {text, Result}, Req, State};

websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({battle_perception, Message}, Req, State) ->
    PrepMessage = message:prepare(battle_perception, Message),
    Encoded = jsx:encode(PrepMessage),
	{reply, {text, Encoded}, Req, State};
websocket_info({map_perception, Message}, Req, State) ->
    PrepMessage = message:prepare(map_perception, Message),
    Encoded = jsx:encode(PrepMessage),
	{reply, {text, Encoded}, Req, State};
websocket_info({item_perception, Message}, Req, State) ->
    PrepMessage = message:prepare(item_perception, Message),
    Encoded = jsx:encode(PrepMessage),
	{reply, {text, Encoded}, Req, State};
websocket_info({Type, Message}, Req, State) ->
    PrepMessage = message:prepare(Type, Message),
    Encoded = jsx:encode(PrepMessage),
	{reply, {text, Encoded}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.
