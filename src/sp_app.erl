-module(sp_app).
-behaviour(application).

-include("common.hrl").
-include("schema.hrl").

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, sp, "index.html"}},
			{"/websocket", ws_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, sp, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]),

    % Load game data, start game loop and managers
    setup:start(),
    
	sp_sup:start_link().

stop(_State) ->
	ok.
