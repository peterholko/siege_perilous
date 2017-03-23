-module(sp_SUITE).

-include("common.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1]).
-export([combat_test/1, combo_test/1, harvest_test/1, craft_test/1]).

all() ->
    [%combo_test,
     %combat_test,
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
    lager_common_test_backend:bounce(info),
    setup:login(<<"test1">>, <<"123123">>, self()),
    timer:sleep(5000),

    resource:create(<<"Valleyrun Copper Ore">>, 100, {18, 35}, false),

    Prospect = player:prospect(<<"5">>),
    ct:print("Prospect: ~p", [Prospect]),

    receive 
        Message0 ->
            ct:print("Receive: ~p", [Message0])
    after 5000 ->
        exit(timeout)
    end,

    timer:sleep(5000),

    receive 
        Message00 ->
            ct:print("Receive: ~p", [Message00])
    after 5000 ->
        exit(timeout)
    end,

    Survey = player:survey(<<"5">>),
    ct:print("Survey: ~p", [Survey]),

    Harvest = player:harvest(<<"5">>,  <<"Valleyrun Copper Ore">>),
    ct:print("Harvest ~p", [Harvest]),

    timer:sleep(1000),

    receive 
        Message1 ->
            ct:print("Receive: ~p", [Message1])
    after 5000 ->
        exit(timeout)
    end,

    timer:sleep(5000),

    receive 
        Message11 ->
            ct:print("Receive: ~p", [Message11])
    after 5000 ->
        exit(timeout)
    end,

    timer:sleep(500),

    receive 
        Message12 ->
            ct:print("Receive: ~p", [Message12])
    after 5000 ->
        exit(timeout)
    end,

    timer:sleep(500),

    receive 
        Message13 ->
            ct:print("Receive: ~p", [Message13])
    after 5000 ->
        exit(timeout)
    end,

    timer:sleep(500),

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
    %lager_common_test_backend:bounce(info),
    setup:login(<<"test1">>, <<"123123">>, self()),
    Build = player:build(<<"5">>, <<"Mine">>),
    ct:print("Build: ~p", [Build]),

    timer:sleep(1000),

    receive 
        Message1 ->
            ct:print("Receive: ~p", [Message1])
    after 5000 ->
        exit(timeout)
    end,

    TransferResult = player:item_transfer(<<"14">>, <<"9">>),

    ct:print("Transfer: ~p", [TransferResult]),

    FinishResult = player:finish_build(<<"5">>, <<"14">>),

    ct:print("Finish: ~p", [FinishResult]),

    receive 
        Message2 ->
            ct:print("Receive: ~p", [Message2])
    after 5000 ->
        exit(timeout)
    end,

    timer:sleep(10000),

    receive 
        Message3 ->
            ct:print("Receive: ~p", [Message3])
    after 5000 ->
        exit(timeout)
    end,

    AssignResult = player:assign(<<"6">>, <<"14">>),
    ct:print("Assign: ~p", [AssignResult]),

    BlackSmithPos = {18, 35},
    Id = obj:create(BlackSmithPos, 1001, structure, ?CRAFT, <<"Blacksmith">>, none),
    ct:print("Id: ~p", [Id]),

    timer:sleep(10000),

    receive 
        Message4 ->
            ct:print("Receive: ~p", [Message4])
    after 5000 ->
        exit(timeout)
    end,

    receive 
        Message5 ->
            ct:print("Receive: ~p", [Message5])
    after 5000 ->
        exit(timeout)
    end,

    timer:sleep(10000),

    receive 
        Message6 ->
            ct:print("Receive: ~p", [Message6])
    after 5000 ->
        exit(timeout)
    end,

    timer:sleep(15000),

    receive 
        Message7 ->
            ct:print("Receive: ~p", [Message7])
    after 5000 ->
        exit(timeout)
    end,
    

    TransferResult2 = player:item_transfer(<<"5">>, <<"9">>),
    TransferResult3 = player:item_transfer(<<"5">>, <<"17">>),

    timer:sleep(1000),
    
    InfoResult = player:get_info_unit(<<"5">>),
    ct:print("Info: ~p", [InfoResult]),

    TransferResult4 = player:item_transfer(<<"16">>, <<"13">>),
    TransferResult5 = player:item_transfer(<<"16">>, <<"15">>),

    InfoResult2 = player:get_info_unit(<<"16">>),
    ct:print("Info: ~p", [InfoResult2]),

    %ct:print("Transfer: ~p ~p", [TransferResult2, TransferResult3]),
    %ct:print("Transfer: ~p ~p", [TransferResult4, TransferResult5]),

    AssignResult2 = player:assign(<<"6">>, <<"16">>),
    ct:print("Assign: ~p", [AssignResult2]),

    ProcessResult = player:process_resource(<<"16">>),
    ct:print("Process: ~p", [ProcessResult]),

    timer:sleep(60000),

    InfoResult3 = player:get_info_unit(<<"16">>),
    ct:print("Info: ~p", [InfoResult3]),

    receive 
        Message8 ->
            ct:print("Receive: ~p", [Message8])
    after 5000 ->
        exit(timeout)
    end,

    CraftResult = player:craft(<<"16">>, <<"Dagger">>),
    ct:print("Craft: ~p", [CraftResult]),

    timer:sleep(60000),

    InfoResult4 = player:get_info_unit(<<"16">>),
    ct:print("Info: ~p", [InfoResult4]),

    Items = maps:get(<<"items">>, InfoResult4),
    F = fun(Item) -> maps:get(<<"subclass">>, Item, false) =:= ?DAGGER end,
    HasDagger = lists:any(F, Items),
    ?assertEqual(HasDagger, true).


combo_test(_Config) ->
    Weight = item_def:value(<<"Salarian Wheat">>, <<"weight">>),
    ?assertEqual(1, Weight).
