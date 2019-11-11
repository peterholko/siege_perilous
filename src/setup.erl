%% Author: Peter
%% Created: Feb, 2016
%% Description: setup module
-module(setup).

-include("schema.hrl").
-include("common.hrl").

-export([start/0, login/3, select_class/1]).

start() ->
    application:start(yamerl),

    lager:info("Creating schema..."),
    db:create_schema(),
    lager:info("Starting mnesia db..."),
    db:start(),
    lager:info("Inserting test data..."),
    db:reset_tables(),

    lager:info("Import data"),
    %db:import("obj_template"),
    %db:import("item_template"),
    db:import("recipe_def"),
    db:import("resource_def"),
    db:import("skill_def"),

    db:import_yaml("obj_template"),
    db:import_yaml("item_template"),
    

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
        {first_login, PlayerId} ->
            lager:info("Successful first login"),
            %Stored player id in process dict for easy access
            put(player_id, PlayerId),

            SelectClassPacket = [{<<"packet">>, <<"select_class">>},
                                 {<<"player">>, PlayerId}],

            jsx:encode(SelectClassPacket);
        {select_class, PlayerId} ->
            lager:info("Select Class not completed"),
            SelectClassPacket = [{<<"packet">>, <<"select_class">>},
                                 {<<"player">>, PlayerId}],

            jsx:encode(SelectClassPacket);            
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

select_class(Class) when Class =:= <<"Warrior">> orelse 
                         Class =:= <<"Ranger">> orelse 
                         Class =:= <<"Mage">> ->
    PlayerId = get(player_id),
    [Player] = db:read(player, PlayerId),

    Packet = case player:is_class(Player, none) of
                true ->
                    NewPlayer = Player#player {class = Class},
                    db:write(NewPlayer),

                    game:create_new_player(PlayerId),

                    #{<<"packet">> => <<"info_select_class">>,
                      <<"result">> => <<"success">>};
                false ->
                    #{<<"packet">> => <<"info_select_class">>,
                    <<"errmsg">> => <<"Class already selected.">>}
            end,
    
    jsx:encode(Packet).