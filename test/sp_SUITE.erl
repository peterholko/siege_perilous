-module(sp_SUITE).

-include("schema.hrl").
-include("common.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([login/1, combat_test/1, combo_test/1, harvest_test/1, craft_test/1]).
-export([npc_move_to_pos/1, npc_move_random_pos/1, npc_necro_event/1]).

all() ->
    [{group, npc}].
     %harvest_test,
     %craft_test].

groups() ->
    [{npc,
      [],
      [%npc_move_to_pos, 
       %npc_move_random_pos, 
       npc_necro_event]}].

init_per_suite(Config) ->
    ct:print("Setting up server"),
    setup:start(),

    Config.

end_per_suite(Config) ->
    Config.

login(_Config) ->
    setup_login().

npc_move_to_pos(_Config) ->
    %lager_common_test_backend:bounce(info),
    NPCId = npc:create({15,35}, ?UNDEAD, <<"Necromancer">>),
    timer:sleep(1000),

    OrderData = #{<<"order_pos">> => {15, 37}},
    npc:set_order(NPCId, move_to_pos, OrderData),

    timer:sleep(2000),

    [NPCObj] = db:read(obj, NPCId),

    NPCObj#obj.pos =:= {15,37}.

npc_move_random_pos(_Config) ->
    %lager_common_test_backend:bounce(info),
    NPCId = npc:create({15,35}, ?UNDEAD, <<"Necromancer">>),
    timer:sleep(1000),

    [NPCObj] = db:read(obj, NPCId),

    npc:set_order(NPCId, wander),

    timer:sleep(1000),

    [NewObj] = db:read(obj, NPCId),
    ct:print("Prev Pos: ~p Curr Pos: ~p", [NPCObj#obj.pos, NewObj#obj.pos]),

    NPCObj#obj.pos =/= NewObj#obj.pos.

npc_necro_event(_Config) ->
    %lager_common_test_backend:bounce(info),
    MausoleumId = obj:create({15,34}, ?UNDEAD, <<"Mausoleum">>),    
    NPCId = npc:create({15,35}, ?UNDEAD, <<"Necromancer">>),
    obj:create({16,35}, ?UNDEAD, <<"Human Corpse">>, ?DEAD),

    GuardianId = npc:create({15,35}, ?GUARDIANS, <<"Wose">>, ?HIDING),    
    GuardianData = #{guard_pos => {14,35},
                     guarding_structure => MausoleumId},

    npc:set_order(GuardianId, guard_structure, GuardianData),

    OrderData = #{mausoleum => MausoleumId,
                  mausoleum_guard => GuardianId,
                  order_pos => {15,34}}, 
    npc:set_order(NPCId, necro_event, OrderData),
    
    timer:sleep(2000),
    item:create(MausoleumId, ?BONES, 10),

    Objs = obj:get_by_player(?UNDEAD),
    
    F = fun(Obj) ->
            obj:template(Obj) =:= <<"Zombie">>
        end,

    true = lists:any(F, Objs),

    obj_attr:set(NPCId, <<"hp">>, 10),

    timer:sleep(6000),

    [NPCObjH] = db:read(obj, NPCId),
    hiding = obj:state(NPCObjH),
   
    obj:trigger_inspect(MausoleumId),    

    GuardianState = npc:state(GuardianId),
    ct:print("~p", [GuardianState]),

    timer:sleep(5000),

    Inspected = obj_attr:value(MausoleumId, <<"inspected">>, false),
    ct:print("Inspected: ~p", [Inspected]),

    timer:sleep(10000),

    obj:update_dead(GuardianId), 
    npc:remove(GuardianId),

    timer:sleep(3000),

    timer:sleep(50000), 

    [NPCObj] = db:read(obj, NPCId),
    NPCState = npc:state(NPCId),
    
    ct:print("~p", [NPCObj]),
    ct:print("~p", [NPCState]),

    AllObjs = obj:get_by_player(obj:player(NPCObj)),

    ct:print("~p", [AllObjs]).



combat_test(_Config) ->
    setup_login(),

    HeroId = get_hero_id(get(player_id)),

    receive 
        Message1 ->
            ct:print("Receive: ~p", [Message1])
    after 5000 ->
        exit(timeout)
    end,

    NPCId = npc:generate({16,36}, ?UNDEAD, <<"Zombie">>),

    timer:sleep(1000),

    receive 
        Message2 ->
            ct:print("Receive: ~p", [Message2])
    after 5000 ->
        exit(timeout)
    end,

    player:combo(HeroId, ?QUICK),

    receive 
        Message3 ->
            ct:print("Receive: ~p", [Message3])
    after 5000 ->
        exit(timeout)
    end,

    player:attack(?FIERCE, HeroId, NPCId), 

    receive 
        Message4 ->
            ct:print("Receive: ~p", [Message4])
    after 5000 ->
        exit(timeout)
    end,

    receive 
        Message41 ->
            ct:print("Receive: ~p", [Message41])
    after 5000 ->
        exit(timeout)
    end,
 
    receive 
        Message42 ->
            ct:print("Receive: ~p", [Message42])
    after 5000 ->
        exit(timeout)
    end,

    receive 
        Message43 ->
            ct:print("Receive: ~p", [Message43])
    after 5000 ->
        exit(timeout)
    end,

    player:attack(?FIERCE, HeroId, NPCId), 

    receive 
        Message5 ->
            ct:print("Receive: ~p", [Message5]),
            {broadcast, MessageMap} = Message5,
            ComboName = maps:get(<<"combo">>, MessageMap),
            ?assertEqual(ComboName, ?IMTIMIDATING_SHOUT)

    after 5000 ->
        exit(timeout)
    end,

    player:combo(HeroId, ?QUICK),
    player:attack(?FIERCE, HeroId, NPCId), 

    receive 
        Message6 ->
            ct:print("Receive: ~p", [Message6])
    after 5000 ->
        exit(timeout)
    end,

    combat:defend(?BRACE, NPCId),

    player:attack(?FIERCE, HeroId, NPCId), 

    receive 
        Message7 ->
            ct:print("Receive: ~p", [Message7]),
            {broadcast, MessageMap1} = Message7,
            CounteredName = maps:get(<<"countered">>, MessageMap1),
            ?assertEqual(CounteredName, ?BRACE)
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



setup_login() ->
    case login:login(<<"test1">>, <<"123123">>, self()) of
        {firstlogin, PlayerId} ->
            put(player_id, PlayerId),

            HeroPos = {16,36},
            VillagerPos = {16,37},
            MonolithPos = {18,35},
            ShipwreckPos = {15,36},

            MonolithId = obj:create(MonolithPos, PlayerId, <<"Monolith">>),
            ShipwreckId = obj:create(ShipwreckPos, PlayerId, <<"Shipwreck">>),
            HeroId = obj:create(HeroPos, PlayerId, <<"Hero Mage">>),   
            
            [Player] = db:read(player, PlayerId),
            NewPlayer = Player#player {hero = HeroId},
            db:write(NewPlayer),

            VillagerId = villager:generate(0, PlayerId, VillagerPos),

            item:create(HeroId, <<"Crimson Root">>, 100),
            item:create(MonolithId, <<"Mana">>, 2500),
            item:create(ShipwreckId, <<"Cragroot Popular">>, 100),

            map:add_explored(PlayerId, HeroPos, 2),

            %Log player in
            game:add_event(self(), login, PlayerId, none, 2),
           
            % Equip food so it isn't dumped
            ItemMap = item:create(VillagerId, <<"Crimson Root">>, 100),
            ItemId = maps:get(<<"id">>, ItemMap),
            item:equip(ItemId),
            
            receive 
                Message1 ->
                    ct:print("Receive: ~p", [Message1])
            after 5000 ->
                exit(timeout)
            end,

            receive 
                Message2 ->
                    ct:print("Receive: ~p", [Message2])
            after 5000 ->
                exit(timeout)
            end,

            receive 
                Message3 ->
                    ct:print("Receive: ~p", [Message3])
            after 5000 ->
                exit(timeout)
            end,

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

            receive 
                Message6 ->
                    ct:print("Receive: ~p", [Message6])
            after 5000 ->
                exit(timeout)
            end,

            receive 
                Message7 ->
                    {perception, PerceptionData} = Message7,
                    Objs = maps:get(<<"objs">>, PerceptionData),
                    (length(Objs) > 0) =:= true
            after 5000 ->
                exit(timeout)
            end;
        {relogin, PlayerId} ->
            put(player_id, PlayerId),

            game:login(PlayerId)
    end.


get_hero_id(PlayerId) ->
    Perception = perception:get_by_player(PlayerId),

    F = fun(PerceptionMap) ->
            Subclass = maps:get(<<"subclass">>, PerceptionMap),
            Subclass =:= ?HERO
        end,

    [HeroMap] = lists:filter(F, Perception),
    HeroId = maps:get(<<"id">>, HeroMap),
    HeroId.

