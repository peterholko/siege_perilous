-module(sp_SUITE).

-include("common.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1]).
-export([combat_test/1, combo_test/1, harvest_test/1, craft_test/1]).

all() ->
    [combo_test,
     combat_test,
     harvest_test,
     craft_test].

init_per_suite(Config) ->
    setup:start(),

    Config.

combat_test(_Config) ->
    %lager_common_test_backend:bounce(info),
    setup:login(<<"test1">>, <<"123123">>, self()),

    receive 
        Message1 ->
            ct:print("Receive: ~p", [Message1])
    after 5000 ->
        exit(timeout)
    end,

    HeroPos = {18,35},
    NPCId = npc:create(HeroPos, <<"Shadow">>),
    timer:sleep(1000),

    receive 
        Message2 ->
            ct:print("Receive: ~p", [Message2])
    after 5000 ->
        exit(timeout)
    end,

    player:combo(<<"5">>, ?QUICK),

    receive 
        Message3 ->
            ct:print("Receive: ~p", [Message3])
    after 5000 ->
        exit(timeout)
    end,

    player:attack(?FIERCE, <<"5">>, NPCId), 

    receive 
        Message4 ->
            ct:print("Receive: ~p", [Message4])
    after 5000 ->
        exit(timeout)
    end,

    player:attack(?FIERCE, <<"5">>, NPCId), 

    receive 
        Message5 ->
            ct:print("Receive: ~p", [Message5]),
            {broadcast, MessageMap} = Message5,
            ComboName = maps:get(<<"combo">>, MessageMap),
            ?assertEqual(ComboName, ?IMTIMIDATING_SHOUT)

    after 5000 ->
        exit(timeout)
    end,

    player:combo(<<"5">>, ?QUICK),
    player:attack(?FIERCE, <<"5">>, NPCId), 

    receive 
        Message6 ->
            ct:print("Receive: ~p", [Message6])
    after 5000 ->
        exit(timeout)
    end,

    combat:defend(?BRACE, NPCId),

    player:attack(?FIERCE, <<"5">>, NPCId), 

    receive 
        Message7 ->
            ct:print("Receive: ~p", [Message7]),
            {broadcast, MessageMap1} = Message7,
            CounteredName = maps:get(<<"countered">>, MessageMap1),
            ?assertEqual(CounteredName, ?BRACE)
    after 5000 ->
        exit(timeout)
    end,

    obj:remove(NPCId),

    receive 
        Message8 ->
            ct:print("Receive: ~p", [Message8])
    after 5000 ->
        exit(timeout)
    end.

harvest_test(_Config) ->
    setup:login(<<"test1">>, <<"123123">>, self()),
    timer:sleep(5000),
    Events = db:dump(event),
    ct:print("Events: ~p", [Events]),

    Survey = player:survey(<<"5">>),

    ct:print("Survey: ~p", [Survey]),

    Harvest = player:harvest(<<"5">>,  <<"Valleyrun Copper Ore">>),
    ct:print("Harvest ~p", [Harvest]),

    receive 
        Message1 ->
            ct:print("Receive: ~p", [Message1])
    after 5000 ->
        exit(timeout)
    end,

    timer:sleep(5000),

    receive 
        Message4 ->
            ct:print("Receive: ~p", [Message4]),
            {new_items, [ItemMap]} = Message4,
            Quantity = maps:get(<<"quantity">>, ItemMap),
            ?assertEqual(Quantity, 1)
    after 5000 ->
        exit(timeout)
    end.

craft_test(_Config) ->
    setup:login(<<"test1">>, <<"123123">>, self()),
    Build = player:build(<<"5">>, <<"Mine">>),
    ct:print("Build: ~p", [Build]),

    timer:sleep(1000),

    receive 
        Message1 ->
            ct:print("Receive: ~p", [Message1])
    after 5000 ->
        exit(timeout)
    end.

combo_test(_Config) ->
    Weight = item_def:value(<<"Salarian Wheat">>, <<"weight">>),
    ?assertEqual(1, Weight).
