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
    
    lager:info("Starting Battle Manager"),
    combat:start(),

    lager:info("Starting NPC manager"),
    npc:start(),

    lager:info("Starting Villager Manager"),
    villager:start(),

    lager:info("Loading NPC Task Definitions"),
    htn:load(),

    lager:info("Starting game loop"),
    spawn(fun() -> game_loop:loop(0, util:get_time(), global:whereis_name(game_pid)) end),

    lager:info("Starting perception"),
    perception:start(),

    lager:info("Starting map process"),
    map:start(),

    lager:info("Spawning resources..."),
    map:spawn_resources(),

    lager:info("Generating resource bonuses..."),
    resource:generate_effects().


login(Username, Password, Socket) ->
    case login:login(Username, Password, Socket) of
        {error, Error} ->
            Error;
        {firstlogin, PlayerId} ->
            lager:info("Successful first login"),
            %Stored player id in process dict for easy access
            put(player_id, PlayerId),

            %Add event to spawn new player
            game:init_new_player(PlayerId),

            LoginPacket = [{<<"packet">>, <<"login">>},
                           {<<"player">>, PlayerId}],

            lager:info("LoginPacket: ~p", [LoginPacket]),
            jsx:encode(LoginPacket);
        {relogin, PlayerId} ->
            lager:info("Successful relogin"),
            put(player_id, PlayerId),

            %Add even to relogin player
            game:login(PlayerId),

            LoginPacket = [{<<"packet">>, <<"login">>},
                           {<<"player">>, PlayerId}],

            lager:info("LoginPacket: ~p", [LoginPacket]),
            jsx:encode(LoginPacket)
    end.

