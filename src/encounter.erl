%% Author: Peter
%% Created: Jan, 2016
%% Description: Encounter module
-module(encounter).

-include("schema.hrl").
-include("common.hrl").

-export([check/1, get_wildness/1, spawn_npc/2, generate_loot/1]).

check(Pos) ->
    [Tile] = map:get_tile(Pos),
    TileName = map:tile_name(Tile#map.tile),

    EncounterNum = get_num(Pos),
    NumMod = math:pow(0.5, EncounterNum),

    BaseSpawnRate = 0.25,
    EffectiveSpawnRate = NumMod * BaseSpawnRate,

    Random = util:rand(),

    case Random < EffectiveSpawnRate of
        true -> spawn_random_npc(TileName, Pos);
        false -> nothing
    end.

get_wildness(Pos) ->
    case db:read(encounter, Pos) of
        [Encounter] ->
            wildness(Encounter#encounter.num);
        [] ->
            wildness(0)
    end.

spawn_random_npc(TileName, Pos) ->
    NPCList = npc_list(TileName),
    Random = util:rand(length(NPCList)),
    NPCName = lists:nth(Random, NPCList),
    NPCType = obj_def:value(NPCName, <<"npc_type">>),
    NPCPlayerId = npc:get_player_id(NPCType),
    Tiles = game:get_valid_tiles(Pos),

    case Tiles of
        [] -> nothing; %No valid tiles
        Neighbours ->
            RandomPos = util:rand(length(Neighbours)),
            NPCPos = lists:nth(RandomPos, Neighbours),
            NPCId = obj:create(NPCPos, NPCPlayerId, unit, <<"npc">>, NPCName, none),
            sound:sound(NPCPos, 2, "You hear a rustling in the nearby bushes"),	
            
            generate_loot(NPCId),
            
            increase_num(NPCPos)
    end.

spawn_npc(NPCName, Pos) ->
    NPCType = obj_def:value(NPCName, <<"npc_type">>),
    NPCPlayerId = npc:get_player_id(NPCType),
    Tiles = game:get_valid_tiles(Pos),

    case Tiles of
        [] -> nothing; %No valid tiles
        Neighbours ->
            RandomPos = util:rand(length(Neighbours)),
            NPCPos = lists:nth(RandomPos, Neighbours),
            NPCId = obj:create(NPCPos, NPCPlayerId, unit, <<"npc">>, NPCName, none),
            sound:sound(NPCPos, 2, "You hear a rustling in the nearby bushes"),	

            generate_loot(NPCId)
    end.

generate_loot(NPCId) ->
    LootList = loot_list(),

    F = fun({Name, DropRate, Min, Max}, ItemList) ->
            case DropRate > util:rand() of
                true ->
                    Num = util:rand(Max - Min) + Min,
                    ItemMap = item:create(NPCId, Name, Num),
                    [ItemMap | ItemList];
                false ->
                    ItemList
            end
        end,
    
   lists:foldl(F, [], LootList).    

npc_list(TileName) ->
    case TileName of
        ?DECIDUOUS_FOREST -> [<<"Spider">>, <<"Wose">>, <<"Skeleton">>];
        ?SNOW -> [<<"Wolf">>, <<"Yeti">>];
        ?HILLS_SNOW -> [<<"Wolf">>, <<"Yeti">>];
        ?FROZEN_FOREST -> [<<"Wose">>, <<"Yeti">>, <<"Spider">>];
        ?DESERT -> [<<"Scorpion">>, <<"Giant Rat">>, <<"Skeleton">>];
        ?HILLS_DESERT -> [<<"Scorpion">>, <<"Giant Rat">>, <<"Skeleton">>];
        _ -> [<<"Giant Rat">>, <<"Wolf">>, <<"Skeleton">>]
    end.

loot_list() ->
    [{<<"Cragroot Popular">>, 0.2, 1, 5},
     {<<"Wrapwood Birch">>, 0.5, 1, 3},
     {<<"Skyshroud Oak">>, 0.02, 1, 2},
     {<<"Crimson Root">>, 0.99, 5, 10},
     {<<"Mana">>, 0.75, 1, 3},
     {<<"Gold Coins">>, 0.99, 1, 10}].

get_num(Pos) ->
    case effect:has_effect({tile, Pos}, [?SANCTUARY, ?FORTIFIED]) of
        true -> 0;
        false ->
            case db:read(encounter, Pos) of
                [] -> 0;            
                [Encounter] -> Encounter#encounter.num
            end
    end.

increase_num(Pos) ->
    NewEncounter = case db:read(encounter, Pos) of
                        [] -> #encounter {num = 1,
                                          pos = Pos,
                                          modtick = counter:value(tick)};
                        [Encounter] -> 
                            Num = Encounter#encounter.num,
                            Encounter#encounter {num = Num + 1}
                end,

    db:write(NewEncounter).

wildness(0) -> <<"Savage">>;
wildness(1) -> <<"Savage">>;
wildness(2) -> <<"Wild">>;
wildness(_) -> <<"Tamed">>.
