%% Author: Peter
%% Created: Jan, 2016
%% Description: Encounter module
-module(encounter).

-include("schema.hrl").
-include("common.hrl").

-export([check/1]).

check(Pos) ->
    [Tile] = map:get_tile(Pos),
    lager:info("Tile: ~p", [Tile#map.tile]),
    TileName = map:tile_name(Tile#map.tile),
    lager:info("TileName: ~p", [TileName]),

    EncounterNum = get_num(Pos),
    NumMod = math:pow(0.5, EncounterNum),

    BaseSpawnRate = 0.5,
    EffectiveSpawnRate = NumMod * BaseSpawnRate,

    Random = rand:uniform(),

    case Random < EffectiveSpawnRate of
        true -> spawn_npc(TileName, Pos);
        false -> nothing
    end.

spawn_npc(TileName, {X, Y}) ->
    NPCList = npc_list(TileName),
    Random = rand:uniform(length(NPCList)),
    NPCType = lists:nth(Random, NPCList),
    Neighbours = map:neighbours(X, Y),
    RandomPos = rand:uniform(length(Neighbours)),
    NPCPos = lists:nth(RandomPos, Neighbours),

    obj:create(NPCPos, ?UNDEAD, unit, <<"npc">>, NPCType, none).

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

get_num(Pos) ->
    case db:read(encounter, Pos) of
        [] -> 0;            
        [Encounter] -> Encounter#encounter.num
    end.
