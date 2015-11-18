%% Feel free to use, reuse and abuse the code in this file.

%% @private
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
	{ok, _} = cowboy:start_http(http, 100, [{port, 10100}],
		[{env, [{dispatch, Dispatch}]}]),

    lager:info("Creating schema..."),
    db:create_schema(),
    lager:info("Starting mnesia db..."),
    db:start(),
    lager:info("Inserting test data..."),
    db:reset_tables(),

    lager:info("Starting mongodb..."),
    mdb:start(),

    lager:info("Starting game process..."),
    game:start(),

    lager:info("Loading map tileset and data"),
    map:tileset(),
    map:load(),

    lager:info("Starting map process"),
    map:start(),

    lager:info("Starting perception"),
    perception:start(),

    lager:info("Starting NPC manager"),
    npc_mgr:start(),
    npc_mgr:start_all_npc(),

    lager:info("Starting Battle Manager"),
    combat:start(),

    lager:info("Starting Villager Manager"),
    villager:start(),

    lager:info("Loading NPC Task Definitions"),
    htn:load(),
    
    lager:info("Starting game loop"),
    spawn(fun() -> game_loop:loop(0, util:get_time(), global:whereis_name(game_pid)) end),

	sp_sup:start_link().

stop(_State) ->
	ok.
