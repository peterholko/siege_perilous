%% Author: Peter
%% Created: Feb, 2015
%% Description: Structure module
-module(structure).

-include_lib("stdlib/include/ms_transform.hrl").

-include("common.hrl").
-include("schema.hrl").

-export([get_info_experiment/1]).
-export([create_foundation/3, valid_location/2, upgrade/1]).
-export([list/0, recipe_list/1, refine/1]).
-export([has_req/1, has_upgrade_req/1, has_refine_resources/1, check_recipe_req/2]).
-export([process_upkeep/1, process_upkeep_item/3]).
-export([get_harvesters/1]).
-export([get_craftable_recipe/1]).
-export([can_craft/1, can_refine/1, consume_req/1]).
-export([recipe_name/1]).
-export([get_nearby_bones/1]).
-export([resource_type/1, to_skill/1]).
-export([has_exp_item/1, set_exp_item/2, check_experiment_req/1, experiment/1]).

recipe_name(Recipe) -> maps:get(<<"item">>, Recipe).

check_experiment_req(StructureId) ->
    ExpReqList = obj_attr:value(StructureId, <<"exp_req">>),
    ResRateList = [{<<"Copper Ingot">>, 1.75},
                   {<<"Maple Timber">>, 0.35}],

    F = fun({ReqSubclass, _ReqQuantity}) ->
                ExpResources = item:get_exp_res_by_subclass(StructureId, ReqSubclass),
                case ExpResources of
                    [] ->
                        lager:info("No item of that subclass"), 
                        false; 
                    [ExpResource | _Rest] -> %Assume only 1 match 
                        {_, ResRate} = lists:keyfind(ReqSubclass, 1, ResRateList),
                        lager:info("ItemQuantity: ~p ResRate: ~p", [item:quantity(ExpResource), ResRate]),
                        item:quantity(ExpResource) > ResRate
                end
        end,

    lists:all(F, ExpReqList).

experiment(StructureId) ->
    lager:info("Processing experiment"),
    ResRateList = [{<<"Copper Ingot">>, 1.75},
                   {<<"Maple Timber">>, 0.35}],

    ExpReqList = obj_attr:value(StructureId, <<"exp_req">>),

    F = fun({ReqSubclass, ReqQuantity}, Acc) ->
            {_, ResRate} = lists:keyfind(ReqSubclass, 1, ResRateList),

            NewQuantity = ReqQuantity - ResRate,
            lager:info("NewQuantity: ~p ReqQuantity: ~p", [NewQuantity, ReqQuantity]),

            ExpResources = item:get_exp_res_by_subclass(StructureId, ReqSubclass),
            [ExpResource | _Rest] = ExpResources, %Assume only 1 match TODO reconsider

            %To deal with the fact that items are only integers 
            ResDecrease = trunc(ReqQuantity) - trunc(NewQuantity),

            NewExpResQuantity = item:quantity(ExpResource) - ResDecrease,
            lager:info("NewExpRes ~p: ~p ~p", [item:name(ExpResource), NewExpResQuantity, ResDecrease]),
            item:update(item:id(ExpResource), NewExpResQuantity),

            [{ReqSubclass, NewQuantity} | Acc]
        end,

    NewExpReqList = lists:foldl(F, [], ExpReqList),
    lager:info("NewExpMinReqList: ~p", [NewExpReqList]),

    G = fun({_ReqSubclass, ReqQuantity}) -> ReqQuantity =< 0 end,

    ReachedMinExpReq = lists:all(G, NewExpReqList),

    case ReachedMinExpReq of
        true ->
            lager:info("Chance at New Discovery!"),
            case util:rand(100) < 25 of 
                true -> lager:info("New Discovery!");
                false -> lager:info("Failed to discover this time...")
            end;
        false ->
            lager:info("Min Experimentation not reached yet")
    end,

    obj_attr:set(StructureId, <<"exp_req">>, NewExpReqList).

has_exp_item(StructureId) ->
    obj_attr:value(StructureId, <<"exp_item">>, false) =/= false.

set_exp_item(StructureId, _VillagerId) ->
    ExpReq = [{<<"Copper Ingot">>, 25.0},
              {<<"Maple Timber">>, 5.0}],

    obj_attr:set(StructureId, <<"exp_item">>, <<"Copper Training Axe">>),
    obj_attr:set(StructureId, <<"exp_req">>, ExpReq).

get_info_experiment(StructureId) ->
    Items = item:get_by_owner(StructureId),

    F = fun(Item) ->
            item_attr:value(item:id(Item), ?EXP_ITEM, none) =:= ?TRUE
        end,

    ExperimentItem = lists:filter(F, Items),

    G = fun(Item) ->
            item_attr:value(item:id(Item), ?EXP_RESOURCE_ITEM, none) =:= ?TRUE
        end,

    ExperimentResources = lists:filter(G, Items),

    H = fun(Item) ->
            IsNotItem = item_attr:value(item:id(Item), ?EXP_ITEM, none) =/= ?TRUE,
            IsNotResource = item_attr:value(item:id(Item), ?EXP_RESOURCE_ITEM, none) =/= ?TRUE,
            IsNotItem and IsNotResource
        end,

    ValidResources = lists:filter(H, Items),

    #{<<"id">> => StructureId,
      <<"expitem">> => ExperimentItem,
      <<"expresources">> => ExperimentResources,
      <<"validresources">> => ValidResources}.

get_harvesters(Player) ->
    Objs = db:index_read(obj, Player, #obj.player),

    F = fun(Obj) ->
            (Obj#obj.class =:= <<"structure">>) and
            (Obj#obj.subclass =:= <<"resource">>) and
            (Obj#obj.state =:= none)
        end,

    lists:filter(F, Objs).

create_foundation(PlayerId, Pos, Name) ->
    StructureId = obj:create(Pos, 
                             PlayerId,
                             Name, 
                             ?FOUNDED),
    obj_attr:set(StructureId, <<"hp">>, 1),
    StructureId.

upgrade(_StructureId) ->
    lager:info("Upgrade"). 

consume_req(StructureId) ->
    ReqList = obj_attr:value(StructureId, <<"req">>),
    Items = item:get_by_owner(StructureId),

    MatchReq = find_match_req(ReqList, Items),
    lager:info("MatchReq: ~p", [MatchReq]),

    consume_req(ReqList, MatchReq).

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

has_req(StructureId) ->
    ReqList = obj_attr:value(StructureId, <<"req">>),
    Items = item:get_by_owner(StructureId),
    lager:info("ReqList: ~p Items: ~p", [ReqList, Items]),
    has_req(ReqList, Items).

has_upgrade_req(StructureId) ->
    ReqList = obj_attr:value(StructureId, <<"upgrade_req">>),
    Items = item:get_by_owner(StructureId),
    lager:info("ReqList: ~p Items: ~p", [ReqList, Items]),
    has_req(ReqList, Items).

has_refine_resources(StructureId) ->
    case obj_attr:value(StructureId, <<"refine">>, none) of
        none -> false;
        Process -> 
            Items = item:get_by_class(StructureId, Process),
            Items =/= []
    end.

check_recipe_req(ObjId, RecipeName) ->
    Items = item:get_by_owner(ObjId),

    Recipe = recipe:get_recipe(RecipeName),
    ReqList = maps:get(<<"req">>, Recipe),
    lager:info("ReqList: ~p Items: ~p", [ReqList, Items]),
    has_req(ReqList, Items).
 
list() ->
    Structures = obj_template:select(<<"level">>, 0),
    Structures.

recipe_list(Obj) ->
    Recipes = recipe:get_recipes(Obj#obj.template),
    Recipes.

refine(StructureId) ->
    RefineClass = obj_attr:value(StructureId, <<"refine">>),
    
    [Item | _Rest] = item:get_by_class(StructureId, RefineClass),
    Id = maps:get(<<"id">>, Item),
    Quantity = maps:get(<<"quantity">>, Item),
    Produces = maps:get(<<"produces">>, Item),
    Effects = maps:get(<<"effects">>, Item, []),
    
    item:update(Id, Quantity - 1),

    F = fun(NewItemName, AllNewItems) ->
            NewItem = item:create(StructureId, NewItemName, 1),

            %Assign effects from resources to refined resource, 
            %might need to filter effects for specific type of resource
            item_attr:set(maps:get(<<"id">>, NewItem), <<"effects">>, Effects),
             
            MergedNewItem = maps:put(<<"effects">>, Effects, NewItem),

            [MergedNewItem | AllNewItems]
        end,

    NewItems = lists:foldl(F, [], Produces),
    NewItems.

can_refine(StructureId) ->
    RefineSubclass = obj_attr:value(StructureId, <<"refine">>),
    item:get_by_class(StructureId, RefineSubclass) =/= [].

can_craft(StructureId) ->
    [Structure] = db:read(obj, StructureId),
    AllRecipes = recipe:get_recipes(Structure#obj.template),
    Recipes = filter_refine(AllRecipes),

    F = fun(Recipe, Acc) ->
            RecipeName = maps:get(<<"item">>, Recipe),
            check_recipe_req(StructureId, RecipeName) or Acc
        end,

    lists:foldl(F, false, Recipes).

get_craftable_recipe(StructureId) ->
    [Structure] = db:read(obj, StructureId),
    AllRecipes = recipe:get_recipes(Structure#obj.template),
    Recipes = filter_refine(AllRecipes),

    F = fun(Recipe) ->
            RecipeName = maps:get(<<"item">>, Recipe),
            check_recipe_req(StructureId, RecipeName)
        end,

    Craftable = lists:filter(F, Recipes),

    %Pick one at random
    Random = util:rand(length(Craftable)),    
    lists:nth(Random, Craftable).

filter_refine(Recipes) ->
    F = fun(Recipe) ->
            maps:get(<<"class">>, Recipe) =/= <<"Refine">>
        end,

    lists:filter(F, Recipes).


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
            item:match_req(Item, ReqType, ReqQuantity)
        end,

    ReqMatch = lists:any(F, Items),
    lager:info("Structure ReqMatch: ~p", [ReqMatch]),
    NewResult = Result and ReqMatch,

    has_req(NewResult, Rest, Items).

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

get_nearby_bones(SourceObj) ->
    AllAdjPos = map:range(obj:pos(SourceObj), obj:vision(SourceObj)),
    
    F = fun(Pos, Acc) ->
            ObjsOnPos = obj:get_by_pos(Pos),
            lager:info("ObjsOnPos: ~p", [ObjsOnPos]),

            G = fun(ObjOnPos, Acc2) ->
                    case item:get_by_subclass(obj:id(ObjOnPos), ?BONES) of
                        [] -> Acc2;
                        [Bones | _] -> [Bones | Acc2]
                    end
                end,

            Bones = lists:foldl(G, [], ObjsOnPos),

            Bones ++ Acc
        end,

    [Bones | _] = lists:foldl(F, [], AllAdjPos),
    Bones.

resource_type(StructureId) when is_integer(StructureId) ->
    Structure = obj:get(StructureId),
    resource_type(Structure);
resource_type(Structure) when is_record(Structure, obj) ->
    case Structure#obj.template of
        ?MINE -> ?ORE;
        ?LUMBERMILL -> ?WOOD;
        ?QUARRY -> ?STONE;
        ?TRAPPER -> ?GAME;
        ?FARM -> ?FOOD
    end.
    
to_skill(Structure) ->
    case Structure#obj.template of
        ?MINE -> ?MINING;
        ?LUMBERMILL -> ?WOODCUTTING;
        ?QUARRY -> ?STONECUTTING;
        ?TRAPPER -> ?HUNTING;
        ?FARM -> ?FARMING
    end.


    