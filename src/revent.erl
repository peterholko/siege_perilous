%% Author: Peter
%% Created: March, 2016
%% Description: Random events interface
-module(revent).

-include("schema.hrl").

-export([create/0, get/1, to_map/1]).
-export([apply_effect/3]).

create() ->
    AllREvents = ets:tab2list(revent),
    Num = length(AllREvents),
    Rand = util:rand(Num),
    REvent = lists:nth(Rand, AllREvents),
    REvent.

get(ObjId) ->
    [State] = db:read(state, ObjId),
    REventId = State#state.data,
    [REvent] = db:read(revent, REventId),
    REvent.

to_map(REvent) ->
    REvent0 = maps:put(<<"title">>, REvent#revent.title, #{}),
    REvent1 = maps:put(<<"text">>, REvent#revent.text, REvent0),
    REvent2 = maps:put(<<"responses">>, REvent#revent.responses, REvent1),
    REvent2.

apply_effect(ObjId, REvent, ResponseNum) ->
    Effect = lists:nth(ResponseNum, REvent#revent.effects),
    lager:info("Apply affect ~p", [Effect]),

    %Apply effect
    EffectsText = case Effect of
                    {attrmod, AttrMod} ->
                        lager:info("Apply attrmod: ~p", [AttrMod]),
                        [<<"Gained Stat">>];
                    {random, RandomList} ->
                        Rand = util:rand(100),
                        SelectedEffect = get_random(Rand, 0, RandomList, none),
                        lager:info("SelectedEffect: ~p", [SelectedEffect]),
                        apply_random(ObjId, SelectedEffect);
                    _ -> 
                        lager:info("Effect nothing"),
                        []
                  end,

    %Return resolution text
    ResolutionText = lists:nth(ResponseNum, REvent#revent.resolutions),

    {ResolutionText, EffectsText}.
            
apply_random(_ObjId, none) -> nothing;
apply_random(ObjId, SelectedEffect) ->
    EffectType = element(2, SelectedEffect),

    case EffectType of
        spawn ->
            lager:info("Spawning npc..."),
            SpawnType = element(3, SelectedEffect),
            [Obj] = db:read(obj, ObjId),            
            encounter:spawn_npc(SpawnType, Obj#obj.pos),
            
            effect_text(spawn, SpawnType); 
        loot ->
            lager:info("Generating loot"),
            Items = encounter:generate_loot(ObjId),
            
            effect_text(loot, Items);
        _ ->
            lager:info("Unknown effect type"),

            effect_text(unknown, none)
    end.
    
get_random(_RandNum, _SumChance, [], SelectedRandom) ->
    SelectedRandom;
get_random(RandNum, SumChance, [Random | Rest], none) ->
    Chance = element(1, Random),
    NewSumChance = Chance + SumChance,    
    lager:info("RandNum ~p Chance: ~p", [RandNum, NewSumChance]),

    NewSelectedRandom = case RandNum =< NewSumChance of
                            true -> Random;
                            false -> none
                        end,

    get_random(RandNum, NewSumChance, Rest, NewSelectedRandom);
get_random(_RandNum, _SumChance, _RandomList, SelectedRandom) ->
    SelectedRandom.

effect_text(spawn, SpawnType) ->
    [ iolist_to_binary([<<"A ">>, SpawnType, <<" appears!">>]) ];
effect_text(loot, Items) ->
    
    F = fun(Item, EffectsText) ->
           ItemName = maps:get(<<"name">>, Item),
           [ iolist_to_binary([<<"Received ">>, ItemName, <<".">>]) | EffectsText]
        end,

    lists:foldl(F, [], Items);
effect_text(_, _) -> 
    [].    
