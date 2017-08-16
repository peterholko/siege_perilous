-define(MAX_TIME, 2147483647).
-define(MAX_INT, 2147483647).

-define(ERR_UNKNOWN, 0).
-define(ERR_BAD_LOGIN, 1).
-define(ERR_ACCOUNT_DISABLED, 2).

-define(GAME_LOOP_TICK, 200).
-define(GAME_VISION_RANGE, 50).
-define(GAME_NUM_HOURS_PER_DAY, 6).
-define(TICKS_SEC, 5).
-define(TICKS_MIN, 300).

-define(MAP_WIDTH, 60).
-define(MAP_HEIGHT, 50).

-define(GRASSLANDS, <<"Grasslands">>).
-define(SNOW, <<"Snow">>).
-define(RIVER, <<"River">>).
-define(OCEAN, <<"Ocean">>).
-define(PLAINS, <<"Plains">>).
-define(HILLS_PLAINS, <<"Hills Plains">>).
-define(DESERT, <<"Desert">>).
-define(OASIS, <<"Oasis">>).
-define(HILLS_DESERT, <<"Hills Desert">>).
-define(HILLS_GRASSLANDS, <<"Hills Grasslands">>).
-define(SWAMP, <<"Swamp">>).
-define(HILLS_SNOW, <<"Hills Snow">>).
-define(DECIDUOUS_FOREST, <<"Deciduous Forest">>).
-define(RAINFOREST, <<"Rainforest">>).
-define(JUNGLE, <<"Jungle">>).
-define(SAVANNA, <<"Savanna">>).
-define(FROZEN_FOREST, <<"Frozen Forest">>).
-define(PINE_FOREST, <<"Pine Forest">>).
-define(PALM_FOREST, <<"Palm Forest">>).
-define(MOUNTAIN, <<"Mountain">>).
-define(VOLCANO, <<"Mountain">>).

-define(PLAINS_MC, 1).
-define(MOUNTAINS_MC, 5).
-define(FOREST_MC, 2).
-define(HILLS_MC, 2).

-define(NPC_ID, 1000).

-define(NONE, none).
-define(DEAD, dead).
-define(FOUNDED, founded).
-define(PROGRESSING, progressing).
-define(UPGRADING, upgrading).

-define(NATIVES, 98).
-define(UNDEAD, 99).
-define(ANIMAL, 100).
-define(EMPIRE, 101).
-define(MAX_ZOMBIES, 100).

-define(FOOD, <<"Food">>).
-define(MANA, <<"Mana">>).

-define(STARVING, <<"Starving">>).
-define(SANCTUARY, <<"Sanctuary">>).
-define(FORTIFIED, <<"Fortified">>).
-define(DECAYING, <<"Decaying">>).
-define(BLOODMOON, <<"Bloodmoon">>).

-define(MONOLITH, <<"monolith">>).
-define(NPC, <<"npc">>).
-define(VILLAGER, <<"villager">>).
-define(HERO, <<"hero">>).
-define(POI, <<"poi">>).

-define(WALL, <<"Wall">>).
-define(HARVESTER, <<"resource">>).
-define(CRAFT, <<"craft">>).
-define(STORAGE, <<"resource">>).
-define(SHELTER, <<"shelter">>).

-define(QUICK, <<"quick">>).
-define(PRECISE, <<"precise">>).
-define(FIERCE, <<"fierce">>).

-define(DODGE, <<"dodge">>).
-define(PARRY, <<"parry">>).
-define(BRACE, <<"brace">>).

-define(DEEP_WOUND, <<"deep wound">>).
-define(BLEED, <<"bleed">>).
-define(CONCUSS, <<"concuss">>).
-define(IMPALE, <<"impale">>).
-define(BACKSTAB, <<"backstab">>).
-define(DAZED, <<"dazed">>).
-define(DISARM, <<"disarm">>).
-define(DEMORALIZING_SHOUT, <<"demoralizing shout">>).
-define(EXPOSE_ARMOR, <<"expose armor">>).

-define(DAMAGE, <<"damage">>).
-define(ATTACK_SPEED, <<"attack_speed">>).
-define(FIERCE_DAMAGE, <<"fierce_damage">>).
-define(QUICK_SPEED, <<"quick_speed">>).

-define(DODGE_CHANCE, 15).
-define(PARRY_CHANCE, 10).
-define(PARRY_ATTACK_CHANCE, 15).
-define(PARRY_DISARM_CHANCE, 5).

-define(HAMSTRING, <<"Hamstring">>).
-define(GOUGE, <<"Gouge">>).
-define(IMTIMIDATING_SHOUT, <<"Imtimidating Shout">>).
-define(SHROUDED_SLASH, <<"Shrouded Slash">>).
-define(SHATTER_CLEAVE, <<"Shatter Cleave">>).
-define(MASSIVE_PUMMEL, <<"Massive Pummel">>).
-define(NIGHTMARE_STRIKE, <<"Nightmare Strike">>).

-define(AXE, <<"Axe">>).
-define(DAGGER, <<"Dagger">>).
-define(SWORD, <<"Sword">>).
-define(HAMMER, <<"Hammer">>).
-define(SPEAR, <<"Spear">>).

-define(DEEP_WOUND_CHANCE, 90).
-define(BLEED_CHANCE, 90).
-define(CONCUSS_CHANCE, 90).
-define(DISARM_CHANCE, 90).
-define(IMPALE_CHANCE, 5).

-define(CONCUSS_SPEED, -0.90).
-define(CONCUSS_DAMAGE, -0.90).
-define(CONCUSS_DEF, -0.25).

-define(DISARM_DAMAGE, -0.75).

-define(DEMORALIZING_SHOUT_DAMAGE, -0.05).

-define(EXPOSE_ARMOR_DEF, -0.05).

-define(ORDER_ATTACK, <<"Attack">>).
-define(ORDER_GUARD, <<"Guard">>).
-define(ORDER_FOLLOW, <<"Follow">>).
-define(ORDER_HARVEST, <<"Harvest">>).
-define(ORDER_REFINE, <<"Refine">>).

%STATS
-define(STRENGTH, <<"Strength">>).
-define(TOUGHNESS, <<"Toughness">>).
-define(ENDURANCE, <<"Endurance">>).
-define(DEXTERITY, <<"Dexterity">>).
-define(INTELLECT, <<"Intellect">>).
-define(FOCUS, <<"Focus">>).
-define(SPIRIT, <<"Spirit">>).
-define(CREATIVITY, <<"Creativity">>).

%RESOURCE TRAITS
-define(AXE_DMG_P, <<"Axe Damage %">>).
-define(HAMMER_DMG_P, <<"Hammer Damage %">>).

-define(INFO(MSG), log4erl:info("{~w} ~s", [?MODULE, MSG])).
-define(INFO(MSG, DATA), log4erl:info("{~w} ~s ~w", [?MODULE, MSG, DATA])).
-define(INFO2(MSG, DATA), io:fwrite("~s ~s~n", [MSG, DATA])).
-define(INFO(MSG1, DATA1, MSG2, DATA2), log4erl:info("{~w} ~s ~w ~s ~w", [?MODULE, MSG1, DATA1, MSG2, DATA2])).
-define(ERROR(MSG), log4erl:error("{~w:~w} ~s", [?MODULE, ?LINE, MSG])).
-define(ERROR(MSG, DATA), log4erl:error("{~w:~w} ~s: ~w", [?MODULE, ?LINE, MSG, DATA])).

-define(record_to_list(Record),
    fun(Val) ->
        Fields = record_info(fields, Record),
        [_Tag| Values] = tuple_to_list(Val),
        lists:zip(Fields, Values)
    end
).
