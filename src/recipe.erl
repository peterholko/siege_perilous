%% Author: Peter
%% Created: Jan 17, 2017
%% Description: Recipe
-module(recipe).

-include("schema.hrl").

-export([craft/2, get_recipes/1, get_recipe/1, is_refine/1]).

is_refine(RecipeName) ->
    recipe_template:value(RecipeName, <<"class">>) =:= <<"Refine">>.

craft(ObjId, RecipeName) ->
    Items = item:get_by_owner(ObjId),

    Recipe = get_recipe(RecipeName),
    Class = maps:get(<<"class">>, Recipe),
    ReqList = maps:get(<<"req">>, Recipe),

    MatchReq = find_match_req(ReqList, Items),
    lager:info("MatchReq: ~p", [MatchReq]),

    consume_req(ReqList, MatchReq),
    craft_item(ObjId, Recipe, Class, lists:reverse(MatchReq)).

get_recipes(Structure) ->
    recipe_template:select(<<"structure">>, Structure).

get_recipe(ItemName) ->
    recipe_template:all_to_map(ItemName).

find_match_req(Reqs, Items) ->
    find_match_req(Reqs, Items, []).

find_match_req([], _Items, ReqMatchItems) ->
    ReqMatchItems;
find_match_req([#{<<"type">> := ReqType, <<"quantity">> := ReqQuantity} | Rest], Items, ReqMatchItems) ->
    F = fun(Item) ->
            item:match_req(Item, ReqType, ReqQuantity)
        end,

    AllReqMatchItems = lists:filter(F, Items),
    NewReqMatchItems = add_req_match(AllReqMatchItems, ReqMatchItems),  

    find_match_req(Rest, Items, NewReqMatchItems).

add_req_match([], ReqMatchItems) -> ReqMatchItems;
add_req_match([Head | _Rest], ReqMatchItems) -> [Head | ReqMatchItems].

consume_req([], _Items) ->
    lager:info("Completed consuming items");
consume_req([#{<<"type">> := ReqType, <<"quantity">> := ReqQuantity} | Rest], Items) ->
    F = fun(Item) ->
            ItemId = maps:get(<<"id">>, Item),
            ItemName = maps:get(<<"name">>, Item),
            ItemSubClass = maps:get(<<"subclass">>, Item),
            ItemQuantity = maps:get(<<"quantity">>, Item),

            QuantityMatch = ReqQuantity =< ItemQuantity,            
            ItemNameMatch = ReqType =:= ItemName,
            ItemSubClassMatch = ReqType =:= ItemSubClass,

            ItemMatch = ItemNameMatch or ItemSubClassMatch,
            Match = ItemMatch and QuantityMatch,
            NewQuantity = ItemQuantity - ReqQuantity,

            consume_item(Match, ItemId, NewQuantity)
       end,
    lists:foreach(F, Items),

    consume_req(Rest, Items).

consume_item(false, _ItemId, _Quantity) ->
    nothing;
consume_item(true, ItemId, Quantity) ->
    lager:info("Updating item ~p quantity ~p", [ItemId, Quantity]),
    item:update(ItemId, Quantity).


craft_item(OwnerId, Recipe, <<"Weapon">>, MatchReqList) ->
    RecipeName = maps:get(<<"name">>, Recipe),
    lager:debug("Crafting ~p", [RecipeName]),

    F = fun(MatchReq, AllEffects) ->
            Effects = maps:get(<<"effects">>, MatchReq, []),
            lists:merge(Effects, AllEffects)
        end,

    ItemEffects =  lists:foldl(F, [], MatchReqList),

    %TODO enhance the item name
    ItemName = RecipeName,

    BaseStats = #{<<"owner">> => OwnerId, 
                  <<"class">> => <<"Weapon">>,
                  <<"subclass">> => RecipeName,
                  <<"name">> => ItemName,
                  <<"quantity">> => 1,
                  <<"damage">> => maps:get(<<"damage">>, Recipe, 0),
                  <<"speed">> => maps:get(<<"speed">>, Recipe, 0),
                  <<"stamina_req">> => maps:get(<<"stamina_req">>, Recipe, 0),
                  <<"skill_req">> => maps:get(<<"skill_req">>, Recipe, 0),
                  <<"effects">> => ItemEffects
                 },
    
    CraftedItem = item:create(BaseStats),

    [CraftedItem];
craft_item(OwnerId, RecipeName, <<"Armor">>, MatchReqList) ->
     F = fun(MatchReq, ItemStats) ->
            Stats = [{<<"armor">>, maps:get(<<"armor">>, MatchReq, 0)},
                     {<<"durability">>, maps:get(<<"durability">>, MatchReq, 0)}],

            combine_stats(Stats, ItemStats)
        end,

    AllItemStats = lists:foldl(F, #{}, MatchReqList),
    
    ItemName = craft_item_name(RecipeName, MatchReqList),

    BaseStats = #{<<"owner">> => OwnerId, 
                  <<"class">> => <<"Armor">>,
                  <<"subclass">> => RecipeName,
                  <<"name">> => ItemName,
                  <<"quantity">> => 1},
    
    FinalItem = maps:merge(BaseStats, AllItemStats),
    CraftedItem = item:create(FinalItem),

    [CraftedItem];  
craft_item(_OwnerId, _RecipeName, <<"Material">>, _MatchReqList) ->
    nothing.
 
combine_stats([], Item) ->
    Item;
combine_stats([{StatName, StatValue} | Rest], Item) ->
    lager:info("StatName: ~p StatValue: ~p", [StatName, StatValue]),
    NewItem = add_stat(StatName, StatValue, Item),
    lager:info("NewItem: ~p", [NewItem]),
    combine_stats(Rest, NewItem).

add_stat(_StatName, 0, ItemStats) -> ItemStats;
add_stat(StatName, StatValue, ItemStats) -> maps:put(StatName, StatValue, ItemStats).

craft_item_name(RecipeName, MatchReqList) ->
    [FirstReq | _] = MatchReqList,
    PrimaryReqName = maps:get(<<"name">>, FirstReq),
    [FirstPart | _] = binary:split(PrimaryReqName, <<" ">>, []),
   
    <<FirstPart/binary, <<" ">>/binary, RecipeName/binary>>.



