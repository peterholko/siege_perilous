%% Author: Peter
%% Created: Feb, 2016
%% Description: setup module
-module(setup).

-include("schema.hrl").
-include("common.hrl").

-export([start/0, login/3]).

start() ->
    lager:info("Creating schema..."),
    db:create_schema(),
    lager:info("Starting mnesia db..."),
    db:start(),
    lager:info("Inserting test data..."),
    db:reset_tables(),

    lager:info("Import data"),
    db:import("obj_template"),
    db:import("item_def"),
    db:import("recipe_def"),
    db:import("resource_def"),
    db:import("skill_def"),

    lager:info("Starting game process..."),
    game:start(),

    lager:info("Starting game loop"),
    spawn(fun() -> game_loop:loop(0, util:get_time(), global:whereis_name(game_pid)) end),

    lager:info("Starting map process"),
    map:start(),

    lager:info("Spawning resources..."),
    map:spawn_resources(),

    lager:info("Generating resource bonuses..."),
    resource:generate_effects(),

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
    htn:load().

login(Username, Password, Socket) ->
    case login:login(Username, Password, Socket) of
        {error, Error} ->
            Error;
        {success, PlayerId} ->
            lager:info("Successful login"),
            %Stored player id in process dict for easy access
            put(player_id, PlayerId),

            %Get init perception 
            {PlayerId, ExploredMap, Perception} = player:init_perception(PlayerId),

            lager:info("Perception: ~p", [Perception]),

            %Check if initial login state requires any special data
            player:init_state(PlayerId),

            LoginPacket = [{<<"packet">>, <<"login">>},
                           {<<"player">>, PlayerId},
                           {<<"map">>, ExploredMap},
                           {<<"objs">>, Perception}],
            lager:info("LoginPacket: ~p", [LoginPacket]),
            jsx:encode(LoginPacket)
    end.

