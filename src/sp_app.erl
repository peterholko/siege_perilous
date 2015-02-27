%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(sp_app).
-behaviour(application).

-include("common.hrl").

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

    db:create_schema(),
    ok = db:start(),
    db:reset_tables(),

    map:load_global(),
    map:load_local(),

    game:start(),

    mdb:start(),
    map:start(),

    perception:start(),
    battle:start(),

    npc_mgr:start(),
    npc_mgr:start_all_npc(),

    spawn(fun() -> game_loop:loop(util:get_time(), global:whereis_name(game_pid)) end),

	sp_sup:start_link().

stop(_State) ->
	ok.
