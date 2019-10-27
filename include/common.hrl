-define(MAX_TIME, 2147483647).
-define(MAX_INT, 2147483647).

-ifdef(TEST).
-define(FAST_EVENTS, true).
-else.
-define(FAST_EVENTS, false).
-endif.

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

%SLEEP%
-define(BASE_FOCUS_TIRED, 3000).
-define(BASE_FOCUS_EXHAUSTED, 6000).

-define(PLAYER, <<"player">>).

%STATES
-define(STATE, <<"state">>).
-define(NONE, none).
-define(DEAD, dead).
-define(DELETING, deleting).
-define(FOUNDED, founded).
-define(PROGRESSING, progressing).
-define(STALLED, stalled).
-define(EXPLORING, exploring).
-define(GATHERING, gathering).
-define(HARVESTING, harvesting).
-define(UPGRADING, upgrading).
-define(MOVING, moving).
-define(BUILDING, building).
-define(CASTING, casting).
-define(HIDING, hiding).
-define(SLEEPING, sleeping).
-define(EATING, eating).
-define(DRINKING, drinking).
-define(RESTING, resting).
-define(DISABLED, disabled).
-define(ABOARD, aboard).

-define(NATIVES, 98).
-define(UNDEAD, 99).
-define(ANIMAL, 100).
-define(EMPIRE, 101).
-define(GUARDIANS, 102).
-define(THE_LOST, 200).

-define(MAX_ZOMBIES, 100).


-define(STARVING, <<"Starving">>).
-define(SANCTUARY, <<"Sanctuary">>).
-define(FORTIFIED, <<"Fortified">>).
-define(DECAYING, <<"Decaying">>).
-define(BLOODMOON, <<"Bloodmoon">>).

%CLASS and SUBCLASS
-define(CORPSE, <<"corpse">>).
-define(BONES, <<"bones">>).
-define(MONOLITH, <<"monolith">>).
-define(NPC, <<"npc">>).
-define(VILLAGER, <<"villager">>).
-define(HERO, <<"hero">>).
-define(POI, <<"poi">>).
-define(STRUCTURE, <<"structure">>).

-define(WALL, <<"Wall">>).
-define(HARVESTER, <<"resource">>).
-define(CRAFT, <<"craft">>).
-define(STORAGE, <<"resource">>).
-define(SHELTER, <<"shelter">>).
-define(TRADE, <<"trade">>).

-define(QUICK, <<"quick">>).
-define(PRECISE, <<"precise">>).
-define(FIERCE, <<"fierce">>).

-define(DODGE, <<"dodge">>).
-define(PARRY, <<"parry">>).
-define(BRACE, <<"brace">>).

%EFFECTS

-define(DEEP_WOUND, <<"deep wound">>).
-define(BLEED, <<"bleed">>).
-define(CONCUSS, <<"concuss">>).
-define(IMPALE, <<"impale">>).
-define(BACKSTAB, <<"backstab">>).
-define(DAZED, <<"dazed">>).
-define(DISARM, <<"disarm">>).
-define(DEMORALIZING_SHOUT, <<"demoralizing shout">>).
-define(EXPOSE_ARMOR, <<"expose armor">>).

-define(HOLY_LIGHT, <<"holy light">>).
-define(HUNGRY, <<"hungry">>).
-define(STARVATION, <<"starvation">>).
-define(THIRSTY, <<"thirsty">>).
-define(DEHYDRATION, <<"dehydration">>).
-define(TIRED, <<"tired">>).
-define(EXHAUSTED, <<"exhausted">>).

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

-define(MERCHANT, <<"Merchant">>).

%ORDERS
-define(ORDER_ATTACK, <<"Attack">>).
-define(ORDER_GUARD, <<"Guard">>).
-define(ORDER_FOLLOW, <<"Follow">>).
-define(ORDER_EXPLORE, <<"Explore">>).
-define(ORDER_GATHER, <<"Gather">>).
-define(ORDER_HARVEST, <<"Harvest">>).
-define(ORDER_REFINE, <<"Refine">>).
-define(ORDER_CRAFT, <<"Craft">>).
-define(ORDER_BUILD, <<"Build">>).

%ORDERS NPC
-define(ORDER_TAX_COLLECT, <<"Tax Collect">>).


%ACTIVITIES
-define(ACTIVITY_ATTACK, <<"Attacking target">>).
-define(ACTIVITY_EXPLORE, <<"Exploring">>).
-define(ACTIVITY_GATHER, <<"Gathering">>).
-define(ACTIVITY_FOLLOW, <<"Following">>).
-define(ACTIVITY_HAUL, <<"Hauling">>).

-define(ACTIVITY_FLEE_SHELTER, <<"Fleeing to shelter">>).
-define(ACTIVITY_FLEE_HERO, <<"Fleeing to hero">>).
-define(ACTIVITY_FLEE_RANDOM, <<"Fleeing">>).

-define(ACTIVITY_FOOD_EAT, <<"Stopping for food">>).
-define(ACTIVITY_FOOD_STORAGE, <<"Getting food from storage">>).
-define(ACTIVITY_NO_FOOD, <<"Hungry...">>).

-define(ACTIVITY_WATER_DRINK, <<"Drink time">>).
-define(ACTIVITY_WATER_STORAGE, <<"Getting water from storage">>).
-define(ACTIVITY_NO_WATER, <<"Thirsty...">>).

-define(ACTIVITY_SLEEP_SHELTER, <<"Sleeping under a roof">>).
-define(ACTIVITY_SLEEP, <<"Sleeping under the stars">>).

-define(ACTIVITY_FIND_SHELTER, <<"Finding shelter">>).
-define(ACTIVITY_FIND_STORAGE, <<"Finding storage">>).

-define(ACTIVITY_STORAGE_FULL, <<"Cannot find any storage">>).


%STATS
-define(STRENGTH, <<"Strength">>).
-define(TOUGHNESS, <<"Toughness">>).
-define(ENDURANCE, <<"Endurance">>).
-define(DEXTERITY, <<"Dexterity">>).
-define(INTELLECT, <<"Intellect">>).
-define(FOCUS, <<"Focus">>).
-define(SPIRIT, <<"Spirit">>).
-define(CREATIVITY, <<"Creativity">>).

%OBJ ATTR
-define(WAGE, <<"wage">>).

%RESOURCE TYPES
-define(ORE, <<"Ore">>).
-define(WOOD, <<"Wood">>).
-define(STONE, <<"Stone">>).
-define(GAME, <<"Game">>).
-define(WATER, <<"Water">>).
-define(PLANT, <<"Plants">>).
-define(MANA, <<"Mana">>).
-define(FOOD, <<"Food">>).

%STRUCTURE RESOURCE TYPES
-define(MINE, <<"Mine">>).
-define(QUARRY, <<"Quarry">>).
-define(TRAPPER, <<"Trapper">>).
-define(LUMBERMILL, <<"Lumbermill">>).
-define(FARM, <<"Farm">>).

%SKILLS
-define(MINING, <<"Mining">>).
-define(WOODCUTTING, <<"Woodcutting">>).
-define(STONECUTTING, <<"Stonecutting">>).
-define(HUNTING, <<"Hunting">>).
-define(FORAGING, <<"Foraging">>).
-define(FARMING, <<"Farming">>).

%ITEM CLASSES
-define(GOLD_COINS, <<"Gold Coins">>).

%ITEM SUBCLASSES
-define(PICK_AXE, <<"Pick Axe">>).
-define(CHOPPING_AXE, <<"Chopping Axe">>).
-define(CHISEL, <<"Chisel">>).
-define(HUNTING_KNIFE, <<"Hunting Knife">>).
-define(WATER_FLASK, <<"Water Flask">>).
-define(GATHERING_KIT, <<"Gathering Kit">>).
-define(CONTAINER, <<"Container">>).

-define(AXE, <<"Axe">>).
-define(DAGGER, <<"Dagger">>).
-define(SWORD, <<"Sword">>).
-define(HAMMER, <<"Hammer">>).
-define(SPEAR, <<"Spear">>).

%RESOURCE TRAITS
-define(AXE_DMG_P, <<"Axe Damage %">>).
-define(SWORD_DMG_P, <<"Sword Damage %">>).
-define(HAMMER_DMG_P, <<"Hammer Damage %">>).
-define(DAGGER_DMG_P, <<"Dagger Damage %">>).
-define(SPEAR_DMG_P, <<"Spear Damage %">>).

-define(AXE_SPD_P, <<"Axe Speed %">>).
-define(SWORD_SPD_P, <<"Sword Speed %">>).
-define(HAMMER_SPD_P, <<"Hammer Speed %">>).
-define(DAGGER_SPD_P, <<"Dagger Speed %">>).
-define(SPEAR_SPD_P, <<"Spear Speed %">>).

%EVENTS
-define(NECRO_PHASE1, 1).
-define(NECRO_PHASE2, 2).
-define(NECRO_RAISE_MANA, 10).
-define(NECRO_NUM_MINIONS, 5).

%MAGIC
-define(RAISE_DEAD, <<"Raise Dead">>).
-define(SHADOW_BOLT, <<"Shadow Bolt">>).

%RELATIONS
-define(ALLIES, 10).
-define(NEUTRAL, 0).
-define(ENEMIES, -10).

%EMPIRE
-define(EMPIRE_POS, {-100, -50}).

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
