%% Author: Peter
%% Created: Feb, 2015
%% Description: Structure module
-module(structure).

-include_lib("stdlib/include/ms_transform.hrl").

-include("common.hrl").
-include("schema.hrl").

-export([start_build/4, check_req/1, valid_location/3]).
-export([list/0, recipe_list/1, craft/2]).

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
                               building),
    StructureId.

check_req(Structure) ->
    {StructureId} = bson:lookup('_id', Structure),
    {ReqList} = bson:lookup(req, Structure),
    Items = item:get_by_owner(StructureId),

    process_req(true, ReqList, Items).

list() ->
    Structures = find_type(level, 1),
    Structures.

recipe_list(LocalObj) ->
    Recipes = get_recipes(LocalObj#local_obj.name),
    Recipes.

craft(LocalObj, RecipeName) ->
    Items = item:get_by_owner(LocalObj#local_obj.id),

    Recipe = get_recipe(RecipeName),
    {NewItem} = bson:lookup(item, Recipe),
    {ReqList} = bson:lookup(req, Recipe),

    Result = process_req(true, ReqList, Items),

    lager:info("process_req: ~p", [Result]),

    consume_req(ReqList, Items).

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



process_req(Result, [], _Items) ->
    Result;
process_req(Result, [{type, ReqType, quantity, ReqQuantity} | Rest], Items) ->
    F = fun(Item, HasReq) ->
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
            ItemMatch and QuantityMatch or HasReq
        end,

    ReqMatch = lists:foldl(F, false, Items),
    NewResult = Result and ReqMatch,

    process_req(NewResult, Rest, Items).

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
    Cursor = mongo:find(mdb:get_conn(), <<"local_obj_type">>, {Key, Value, class, <<"structure">>}),
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

   
