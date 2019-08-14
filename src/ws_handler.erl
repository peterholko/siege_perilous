-module(ws_handler).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_handle({text, Msg}, State) ->
    Result = message:decode(Msg),
	{reply, {text, Result}, State};

websocket_handle(_Data, State) ->
	{ok, State}.

websocket_info({map_perception, Message}, State) ->
    PrepMessage = message:prepare(map_perception, Message),
    Encoded = jsx:encode(PrepMessage),
	{reply, {text, Encoded}, State};
websocket_info({item_perception, Message}, State) ->
    PrepMessage = message:prepare(item_perception, Message),
    Encoded = jsx:encode(PrepMessage),
	{reply, {text, Encoded}, State};
websocket_info({Type, Message}, State) ->
    PrepMessage = message:prepare(Type, Message),
    Encoded = jsx:encode(PrepMessage),
	{reply, {text, Encoded}, State};
websocket_info(_Info, State) ->
	{ok, State}.
