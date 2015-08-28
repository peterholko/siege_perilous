-define(MAX_TIME, 2147483647).
-define(MAX_INT, 2147483647).

-define(ERR_UNKNOWN, 0).
-define(ERR_BAD_LOGIN, 1).
-define(ERR_ACCOUNT_DISABLED, 2).

-define(MAP_FILE, "lib/sp-1/priv/10x10.csv").
-define(BATTLE_MAP_FILE, "lib/sp-1/priv/battle.csv").
-define(MAP_NUMTILES, 100).
-define(MAP_WIDTH, 10).
-define(MAP_HEIGHT, 10).

-define(BATTLE_WIDTH, 4).
-define(BATTLE_HEIGHT, 4).

-define(GAME_LOOP_TICK, 200).
-define(GAME_VISION_RANGE, 50).
-define(GAME_NUM_HOURS_PER_DAY, 6).

-define(LOS, 2).

-define(PLAINS, 0).
-define(MOUNTAINS, 1).
-define(FOREST, 2).
-define(HILLS, 3).

-define(PLAINS_MC, 1).
-define(MOUNTAINS_MC, 100).
-define(FOREST_MC, 2).
-define(HILLS_MC, 2).

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
