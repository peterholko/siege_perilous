%% Author: Peter
%% Created: Feb, 2015
%% Description: Structure module
-module(structure).

-include_lib("stdlib/include/ms_transform.hrl").

-include("common.hrl").
-include("schema.hrl").

-export([start_build/3, valid_location/2]).
-export([list/0, recipe_list/1, process/1, craft/2]).
-export([check_req/1, has_process_res/1, check_recipe_req/2]).
-export([combine_stats/2, craft_item_name/2]).

start_build(PlayerId, Pos, StructureType) ->
    {Name} = bson:lookup(name, StructureType),
    {Subclass} = bson:lookup(subclass, StructureType),

    StructureId = obj:create(Pos, 
                             PlayerId,
                             structure,
                             Subclass,
                             Name, 
                             founded),
    StructureId.

check_req(Structure) ->
    {StructureId} = bson:lookup('_id', Structure),
    {ReqList} = bson:lookup(req, Structure),
    Items = item:get_by_owner(StructureId),
    has_req(ReqList, Items).

has_process_res(StructureId) ->
    StructureStats = obj:get(StructureId),
    {Process} = bson:lookup(process, StructureStats),
    Items = item:get_by_subclass(StructureId, Process),
    Items =/= [].

check_recipe_req(ObjId, RecipeName) ->
    Items = item:get_by_owner(ObjId),

    Recipe = get_recipe(RecipeName),
    {ReqList} = bson:lookup(req, Recipe),

    has_req(ReqList, Items).
 
list() ->
    Structures = find_type(level, 0),
    Structures.

recipe_list(Obj) ->
    Recipes = get_recipes(Obj#obj.name),
    Recipes.

process(StructureId) ->
    StructureStats = obj:get(StructureId),
    {Process} = bson:lookup(process, StructureStats),
    
    [Item | _Rest] = item:get_by_subclass(StructureId, Process),
    {Id} = bson:lookup('_id', Item),
    {Quantity} = bson:lookup(quantity, Item),
    {Produces} = bson:lookup(produces, Item),
    
    item:update(Id, Quantity - 1),

    F = fun(NewItemName) ->
            item:create(StructureId, NewItemName, 1)
        end,

    lists:foreach(F, Produces),

    %Requery items after new items and update
    item:get_by_owner(StructureId).

craft(ObjId, RecipeName) ->
    Items = item:get_by_owner(ObjId),

    Recipe = get_recipe(RecipeName),
    {RecipeItem} = bson:lookup(item, Recipe),
    {Class} = bson:lookup(class, Recipe),
    {ReqList} = bson:lookup(req, Recipe),

    MatchReq = find_match_req(ReqList, Items),
    lager:info("MatchReq: ~p", [MatchReq]),

    consume_req(ReqList, MatchReq),
    craft_item(ObjId, RecipeItem, Class, lists:reverse(MatchReq)).

valid_location(<<"wall">>, QueryPos) ->
    lager:info("Valid location for wall"),
    %TODO determine atom vs binary for class and subclass
    MS = ets:fun2ms(fun(N = #obj{pos = Pos, 
                                       class = structure,
                                       subclass = <<"wall">>}) when Pos =:= QueryPos -> N end),
    Objs = db:select(obj, MS),

    % True if no objs, False if Wall objs
    Objs =:= [];

valid_location(_, QueryPos) ->
    %TODO determine atom vs binary for class and subclass
    MS = ets:fun2ms(fun(N = #obj{pos = Pos, 
                                 class = structure,
                                 subclass = Subclass}) when Subclass =/= <<"wall">>,
                                                            Pos =:= QueryPos -> N end),
    Objs = db:select(obj, MS),
    lager:info("Objs: ~p", [Objs]),

    % True if no objs, False if Wall objs
    Objs =:= [].
%
% Internal functions
%

has_req(Reqs, Items) ->
    has_req(true, Reqs, Items).
has_req(false, _Reqs, _Items) ->
    false;
has_req(Result, [], _Items) ->
    Result;
has_req(Result, [{type, ReqType, quantity, ReqQuantity} | Rest], Items) ->
    F = fun(Item) ->
            match_req(Item, ReqType, ReqQuantity)
        end,

    ReqMatch = lists:any(F, Items),
    NewResult = Result and ReqMatch,

    has_req(NewResult, Rest, Items).

find_match_req(Reqs, Items) ->
    find_match_req(Reqs, Items, []).

find_match_req([], _Items, ReqMatchItems) ->
    ReqMatchItems;
find_match_req([{type, ReqType, quantity, ReqQuantity} | Rest], Items, ReqMatchItems) ->
    F = fun(Item) ->
            match_req(Item, ReqType, ReqQuantity)
        end,

    AllReqMatchItems = lists:filter(F, Items),
    NewReqMatchItems = add_req_match(AllReqMatchItems, ReqMatchItems),  

    find_match_req(Rest, Items, NewReqMatchItems).

add_req_match([], ReqMatchItems) -> ReqMatchItems;
add_req_match([Head | _Rest], ReqMatchItems) -> [Head | ReqMatchItems].

match_req(Item, ReqType, ReqQuantity) ->
    {ItemName} = bson:lookup(name, Item),
    {ItemSubClass} = bson:lookup(subclass, Item),
    {ItemQuantity} = bson:lookup(quantity, Item),

    QuantityMatch = ReqQuantity =< ItemQuantity,            
    ItemNameMatch = ReqType =:= ItemName,
    ItemSubClassMatch = ReqType =:= ItemSubClass,

    lager:info("NameMatch ~p ~p ~p", [ReqType, ItemName, ItemSubClass]),
    lager:info("QuantityMatch ~p ~p", [ReqQuantity, ItemQuantity]),

    ItemMatch = ItemNameMatch or ItemSubClassMatch,
    lager:info("ItemMatch: ~p", [ItemMatch]),
    ItemMatch and QuantityMatch.

craft_item(OwnerId, RecipeName, <<"Weapon">>, MatchReqList) ->
   F = fun(MatchReq, ItemStats) ->
            Stats = [{damage, bson:lookup(damage, MatchReq)},
                     {durability, bson:lookup(durability, MatchReq)},
                     {speed, bson:lookup(speed, MatchReq)}],

            combine_stats(Stats, ItemStats)
        end,

    AllItemStats = lists:foldl(F, {}, MatchReqList),
    
    ItemName = craft_item_name(RecipeName, MatchReqList),

    FinalItem = bson:merge({owner, OwnerId, class, <<"Weapon">>, name, ItemName, quantity, 1}, AllItemStats),
    InsertedItem = item:create(FinalItem),
    InsertedItem;
craft_item(_, _, _, _) ->
    nothing.
 
combine_stats([], Item) ->
    Item;
combine_stats([{StatName, StatValue} | Rest], Item) ->
    NewItem = add_stat(StatName, StatValue, Item),
    combine_stats(Rest, NewItem).

add_stat(_StatName, {}, ItemStats) -> ItemStats;
add_stat(StatName, {StatValue}, ItemStats) -> bson:update(StatName, StatValue, ItemStats).

craft_item_name(RecipeName, MatchReqList) ->
    [FirstReq | _] = MatchReqList,
    {PrimaryReqName} = bson:lookup(name, FirstReq),
    [FirstPart | _] = binary:split(PrimaryReqName, <<" ">>, []),
   
    <<FirstPart/binary, <<" ">>/binary, RecipeName/binary>>.

consume_req([], _Items) ->
    lager:info("Completed consuming items");
consume_req([{type, ReqType, quantity, ReqQuantity} | Rest], Items) ->
    F = fun(Item) ->
            {ItemId} = bson:lookup('_id', Item),
            {ItemName} = bson:lookup(name, Item),
            {ItemSubClass} = bson:lookup(subclass, Item),
            {ItemQuantity} = bson:lookup(quantity, Item),

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

find_type(Key, Value) ->
    Cursor = mongo:find(mdb:get_conn(), <<"obj_type">>, {Key, Value}),
    Structures = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Structures.

get_recipes(Structure) ->
    Cursor = mongo:find(mdb:get_conn(), <<"recipe_type">>, {structure, Structure}),
    Recipes = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Recipes.

get_recipe(ItemName) ->
    Cursor = mongo:find(mdb:get_conn(), <<"recipe_type">>, {item, ItemName}),
    [Recipe] = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Recipe.

