%% Author: Peter
%% Created: Feb, 2015
%% Description: Structure module
-module(structure).

-include_lib("stdlib/include/ms_transform.hrl").

-include("common.hrl").
-include("schema.hrl").

-export([start_build/4, valid_location/2]).
-export([list/0, recipe_list/1, process/1, craft/2]).
-export([check_req/1, has_process_res/1, check_recipe_req/2]).
-export([combine_stats/2, craft_item_name/2]).
-export([process_upkeep/1, process_upkeep_item/3]).
-export([get_harvesters/1, harvest/2]).
-export([get_craftable_recipe/1, get_recipes/1]).
-export([can_craft/1, can_process/1]).
-export([recipe_name/1]).

recipe_name(Recipe) -> maps:get(<<"item">>, Recipe).

get_harvesters(Player) ->
    Objs = db:index_read(obj, Player, #obj.player),

    F = fun(Obj) ->
            (Obj#obj.class =:= <<"structure">>) and
            (Obj#obj.subclass =:= <<"resource">>) and
            (Obj#obj.state =:= none)
        end,

    lists:filter(F, Objs).

harvest([_Villager], [Structure]) ->
    case resource:survey(Structure#obj.pos) of
        [Resource | _Rest] ->
            ResourceName = maps:get(<<"name">>, Resource),
            case resource:harvest(Structure#obj.id, ResourceName, Structure#obj.pos) of
                {error, ErrMsg} -> {error, ErrMsg};
                _ -> success
            end;
        [] ->
            {error, <<"Invalid resource">>}
    end;
harvest(_, _) -> 
    {error, <<"Invalid villager or structure">>}.

start_build(PlayerId, Pos, Name, Subclass) ->
    StructureId = obj:create(Pos, 
                             PlayerId,
                             structure,
                             Subclass,
                             Name, 
                             founded),
    obj_attr:set(StructureId, <<"hp">>, 1),
    StructureId.

check_req(StructureId) ->
    ReqList = obj_attr:value(StructureId, <<"req">>),
    Items = item:get_by_owner(StructureId),
    has_req(ReqList, Items).

has_process_res(StructureId) ->
    Process = obj_attr:value(StructureId, <<"process">>),
    Items = item:get_by_subclass(StructureId, Process),
    Items =/= [].

check_recipe_req(ObjId, RecipeName) ->
    Items = item:get_by_owner(ObjId),

    Recipe = get_recipe(RecipeName),
    ReqList = maps:get(<<"req">>, Recipe),
    lager:info("ReqList: ~p Items: ~p", [ReqList, Items]),
    has_req(ReqList, Items).
 
list() ->
    Structures = find_type(level, 0),
    Structures.

recipe_list(Obj) ->
    Recipes = get_recipes(Obj#obj.name),
    Recipes.

process(StructureId) ->
    Process = obj_attr:value(StructureId, <<"process">>),
    
    [Item | _Rest] = item:get_by_subclass(StructureId, Process),
    Id = maps:get(<<"id">>, Item),
    Quantity = maps:get(<<"quantity">>, Item),
    Produces = maps:get(<<"produces">>, Item),
    
    item:update(Id, Quantity - 1),

    F = fun(NewItemName, AllNewItems) ->
            NewItem = item:create(StructureId, NewItemName, 1),
            [NewItem | AllNewItems]
        end,

    NewItems = lists:foldl(F, [], Produces),
    NewItems.

can_process(StructureId) ->
    Process = obj_attr:value(StructureId, <<"process">>),
    item:get_by_subclass(StructureId, Process) =/= [].

can_craft(StructureId) ->
    [Structure] = db:read(obj, StructureId),
    Recipes = filter_process_res(get_recipes(Structure#obj.name)),

    F = fun(Recipe, Acc) ->
            RecipeName = maps:get(<<"item">>, Recipe),
            check_recipe_req(StructureId, RecipeName) or Acc
        end,

    lists:foldl(F, false, Recipes).

get_craftable_recipe(StructureId) ->
    [Structure] = db:read(obj, StructureId),
    Recipes = filter_process_res(get_recipes(Structure#obj.name)),

    F = fun(Recipe) ->
            RecipeName = maps:get(<<"item">>, Recipe),
            check_recipe_req(StructureId, RecipeName)
        end,

    Craftable = lists:filter(F, Recipes),

    %Pick one at random
    Random = util:rand(length(Craftable)),    
    lists:nth(Random, Craftable).

filter_process_res(Recipes) ->
    F = fun(Recipe) ->
            maps:get(<<"class">>, Recipe) =/= <<"process_res">>
        end,

    lists:filter(F, Recipes).

craft(ObjId, RecipeName) ->
    Items = item:get_by_owner(ObjId),

    Recipe = get_recipe(RecipeName),
    RecipeItem = maps:get(<<"item">>, Recipe),
    Class = maps:get(<<"class">>, Recipe),
    ReqList = maps:get(<<"req">>, Recipe),

    MatchReq = find_match_req(ReqList, Items),
    lager:info("MatchReq: ~p", [MatchReq]),

    consume_req(ReqList, MatchReq),
    craft_item(ObjId, RecipeItem, Class, lists:reverse(MatchReq)).

valid_location(?WALL, QueryPos) ->
    lager:info("Valid location for wall"),
    %TODO determine atom vs binary for class and subclass
    MS = ets:fun2ms(fun(N = #obj{pos = Pos, 
                                       class = structure,
                                       subclass = ?WALL}) when Pos =:= QueryPos -> N end),
    Objs = db:select(obj, MS),

    % True if no objs, False if Wall objs
    Objs =:= [];

valid_location(_, QueryPos) ->
    %TODO determine atom vs binary for class and subclass
    MS = ets:fun2ms(fun(N = #obj{pos = Pos, 
                                 class = Class,
                                 subclass = Subclass}) when (Class =:= structure orelse Class =:= poi),
                                                            Subclass =/= ?WALL,
                                                            Pos =:= QueryPos -> N end),
    Objs = db:select(obj, MS),
    lager:info("Objs: ~p", [Objs]),

    % True if no objs, False if Wall objs
    Objs =:= [].

process_upkeep(Structure) ->
    UpkeepList = obj_attr:value(Structure#obj.id, <<"upkeep">>, []),

    F = fun(UpkeepReq, PrevIsDecaying) ->
            Subclass = maps:get(<<"type">>, UpkeepReq),
            UpkeepQuantity = maps:get(<<"quantity">>, UpkeepReq),
            
            IsDecaying = process_upkeep_item(Structure, Subclass, UpkeepQuantity),
            IsDecaying or PrevIsDecaying
        end,

    Decaying = lists:foldl(F, false, UpkeepList),

    case Decaying of
        true ->
            effect:add(Structure#obj.id, ?DECAYING, none),
            obj:update_hp(Structure#obj.id, -1);
        false ->
            effect:remove(Structure#obj.id, ?DECAYING)
    end.

%
% Internal functions
%

has_req(Reqs, Items) ->
    has_req(true, Reqs, Items).
has_req(false, _Reqs, _Items) ->
    false;
has_req(Result, [], _Items) ->
    Result;
has_req(Result, [#{<<"type">> := ReqType, <<"quantity">> := ReqQuantity} | Rest], Items) ->
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
find_match_req([#{<<"type">> := ReqType, <<"quantity">> := ReqQuantity} | Rest], Items, ReqMatchItems) ->
    F = fun(Item) ->
            match_req(Item, ReqType, ReqQuantity)
        end,

    AllReqMatchItems = lists:filter(F, Items),
    NewReqMatchItems = add_req_match(AllReqMatchItems, ReqMatchItems),  

    find_match_req(Rest, Items, NewReqMatchItems).

add_req_match([], ReqMatchItems) -> ReqMatchItems;
add_req_match([Head | _Rest], ReqMatchItems) -> [Head | ReqMatchItems].

match_req(Item, ReqType, ReqQuantity) ->
    ItemName = maps:get(<<"name">>, Item),
    ItemSubClass = maps:get(<<"subclass">>, Item),
    ItemQuantity = maps:get(<<"quantity">>, Item),

    QuantityMatch = ReqQuantity =< ItemQuantity,            
    ItemNameMatch = ReqType =:= ItemName,
    ItemSubClassMatch = ReqType =:= ItemSubClass,

    lager:info("NameMatch ~p ~p ~p", [ReqType, ItemName, ItemSubClass]),
    lager:info("QuantityMatch ~p ~p", [ReqQuantity, ItemQuantity]),

    ItemMatch = ItemNameMatch or ItemSubClassMatch,
    lager:info("ItemMatch: ~p", [ItemMatch]),
    ItemMatch and QuantityMatch.

craft_item(OwnerId, RecipeName, <<"Weapon">>, MatchReqList) ->
    lager:debug("Crafting ~p", [RecipeName]),

    F = fun(MatchReq, ItemStats) ->
            Stats = [{<<"damage">>, maps:get(<<"damage">>, MatchReq, 0)},
                     {<<"durability">>, maps:get(<<"durability">>, MatchReq, 0)},
                     {<<"speed">>, maps:get(<<"speed">>, MatchReq, 0)}],

            combine_stats(Stats, ItemStats)
        end,

    AllItemStats = lists:foldl(F, #{}, MatchReqList),
    
    ItemName = craft_item_name(RecipeName, MatchReqList),

    BaseStats = #{<<"owner">> => OwnerId, 
                  <<"class">> => <<"Weapon">>,
                  <<"subclass">> => RecipeName,
                  <<"name">> => ItemName,
                  <<"quantity">> => 1},
    
    FinalItem = maps:merge(BaseStats, AllItemStats),
    CraftedItem = item:create(FinalItem),

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

process_upkeep_item(Structure, Subclass, UpkeepQuantity) ->
    lager:info("~p ~p ~p", [Structure, Subclass, UpkeepQuantity]),
    case item:get_by_subclass(Structure#obj.id, Subclass) of
        [Item | _Rest] ->
            ItemId = maps:get(<<"id">>, Item),
            Quantity = maps:get(<<"quantity">>, Item),

            case Quantity >= UpkeepQuantity of
               true ->
                   NewQuantity = Quantity - UpkeepQuantity,
                   item:update(ItemId, NewQuantity),
                   false;
               false ->
                   item:update(ItemId, 0),
                   true
            end;
        [] ->
            true
    end.

