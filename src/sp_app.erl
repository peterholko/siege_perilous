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
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]),

    lager:info("Creating schema..."),
    db:create_schema(),
    lager:info("Starting mnesia db..."),
    db:start(),
    lager:info("Inserting test data..."),
    db:reset_tables(),

    lager:info("Starting mongodb..."),
    mdb:start(),

    lager:info("Import data from database"),
    db:import(<<"obj_type">>, obj_def),
    db:import(<<"item_type">>, item_def),
    db:import(<<"recipe_type">>, recipe_def),

    lager:info("Starting game process..."),
    game:start(),

    lager:info("Starting game loop"),
    spawn(fun() -> game_loop:loop(0, util:get_time(), global:whereis_name(game_pid)) end),

    lager:info("Starting map process"),
    map:start(),

    lager:info("Spawning resources..."),
    map:spawn_resources(),

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
    
	sp_sup:start_link().

stop(_State) ->
	ok.
