%% Author: Peter
%% Created: Feb, 2015
%% Description: Structure module
-module(structure).

-include_lib("stdlib/include/ms_transform.hrl").

-include("common.hrl").
-include("schema.hrl").

-export([start_build/4, valid_location/3]).
-export([list/0, recipe_list/1, process/1, craft/2]).
-export([check_req/1, has_process_res/1]).

start_build(PlayerId, GlobalPos, LocalPos, StructureType) ->
    {Name} = bson:lookup(name, StructureType),
    {Subclass} = bson:lookup(subclass, StructureType),

    StructureId = local:create(GlobalPos, 
                               none,
                               LocalPos, 
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
    StructureStats = local_obj:get_stats(StructureId),
    {Process} = bson:lookup(process, StructureStats),
    Items = item:get_by_subclass(StructureId, Process),
    Items =/= [].

list() ->
    Structures = find_type(level, 0),
    Structures.

recipe_list(LocalObj) ->
    Recipes = get_recipes(LocalObj#local_obj.name),
    Recipes.

process(StructureId) ->
    StructureStats = local_obj:get_stats(StructureId),
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

craft(LocalObj, RecipeName) ->
    Items = item:get_by_owner(LocalObj#local_obj.id),

    Recipe = get_recipe(RecipeName),
    {NewItem} = bson:lookup(item, Recipe),
    {ReqList} = bson:lookup(req, Recipe),

    case has_req(ReqList, Items) of
        true ->
            MatchReq = find_match_req(ReqList, Items),
            lager:info("MatchReq: ~p", [MatchReq]);
        false ->
            lager:info("Do not have all reqs")
    end.

valid_location(<<"wall">>, GlobalPos, LocalPos) ->
    lager:info("Valid location for wall"),
    %TODO determine atom vs binary for class and subclass
    MS = ets:fun2ms(fun(N = #local_obj{global_pos = GPos, 
                                       pos = LPos, 
                                       class = structure,
                                       subclass = <<"wall">>}) when GPos =:= GlobalPos, 
                                                                    LPos =:= LocalPos -> N end),
    LocalObjs = db:select(local_obj, MS),

    % True if no local_objs, False if Wall local_objs
    LocalObjs =:= [];

valid_location(_, GlobalPos, LocalPos) ->
    %TODO determine atom vs binary for class and subclass
    MS = ets:fun2ms(fun(N = #local_obj{global_pos = GPos, 
                                       pos = LPos, 
                                       class = structure,
                                       subclass = Subclass}) when Subclass =/= <<"wall">>,
                                                                  GPos =:= GlobalPos, 
                                                                  LPos =:= LocalPos -> N end),
    LocalObjs = db:select(local_obj, MS),
    lager:info("LocalObjs: ~p", [LocalObjs]),

    % True if no local_objs, False if Wall local_objs
    LocalObjs =:= [].


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

find_match_req([], Items, ReqMatchItems) ->
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
    Cursor = mongo:find(mdb:get_conn(), <<"local_obj_type">>, {Key, Value}),
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

