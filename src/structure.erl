%% Author: Peter
%% Created: Feb, 2015
%% Description: Structure module
-module(structure).

-include_lib("stdlib/include/ms_transform.hrl").

-include("common.hrl").
-include("schema.hrl").

-export([get_info_experiment/1]).
-export([create_foundation/3, valid_location/2, upgrade/1]).
-export([list/1, recipe_list/1, refine/1]).
-export([has_req/1, has_upgrade_req/1, has_refine_resources/1, check_recipe_req/2]).
-export([get_updated_req/1]).
-export([process_upkeep/1, process_upkeep_item/3]).
-export([get_harvesters/1]).
-export([get_craftable_recipe/1]).
-export([can_craft/1, consume_req/1]).
-export([recipe_name/1]).
-export([add_deed/4]).
-export([get_nearby_bones/1]).
-export([resource_type/1, to_skill/1]).
-export([has_experiment/1, has_exp_recipe/1,
         set_exp_item/2, check_experiment_req/1, 
         find_exp_recipe/3, experiment/1, remove_experiment/1]).

recipe_name(Recipe) -> maps:get(<<"item">>, Recipe).

has_experiment(StructureId) ->
    case db:read(experiment, StructureId) of
        [Experiment] -> 
            case Experiment#experiment.state of
                ?EXP_STATE_DISCOVERY ->
                    false;  % Return false if the experiment is done
                _ -> 
                    true
            end;
        _ -> 
            false
    end.

has_exp_recipe(StructureId) ->
    [Experiment] = db:read(experiment, StructureId),
    Experiment#experiment.recipe =/= none.
        
set_exp_item(StructureId, ItemName) ->
    NewExperiment = case db:read(experiment, StructureId) of
                        [Experiment] ->
                            %Set recipe to none to reset the experiment
                            Experiment#experiment {recipe = none, 
                                                   state = ?EXP_STATE_NONE,
                                                   exp_item = ItemName,
                                                   req = []};
                        _ ->
                            #experiment {structure = StructureId,
                                         recipe = none,
                                         state = ?EXP_STATE_NONE,
                                         exp_item = ItemName,
                                         req = []}
                    end,

    db:write(NewExperiment).

find_exp_recipe(PlayerId, StructureId, _VillagerId) ->
    [Experiment] = db:read(experiment, StructureId),
    lager:info("Experiment: ~p", [Experiment]),

    ExpItem = Experiment#experiment.exp_item,
    ExpResources = item:get_exp_resources(StructureId),

    StructureObj = obj:get(StructureId),
    StructureTemplate = obj:template(StructureObj),
    
    PlayerRecipes = recipe:get_by_structure(PlayerId, StructureTemplate),
    lager:info("Player Recipes: ~p", [PlayerRecipes]),

    ExpItemRecipe = recipe:get_recipe(ExpItem),
    ExpItemTier = maps:get(<<"tier">>, ExpItemRecipe),
    ExpItemSubclass = maps:get(<<"subclass">>, ExpItemRecipe),
    lager:info("ExpItem: ~p Tier: ~p, Subclass: ~p", [ExpItem, ExpItemTier, ExpItemSubclass]),

    Recipes = recipe:get_by_subclass_tier(StructureTemplate, ExpItemSubclass, ExpItemTier),
    lager:info("All Recipes: ~p", [Recipes]),

    UndiscoveredRecipes = undiscovered_recipes(Recipes, PlayerRecipes),
    lager:info("Undiscovered Recipes: ~p", [UndiscoveredRecipes]),

    MatchedRecipes = matched_undiscovered_reqs(UndiscoveredRecipes, ExpResources),
    lager:info("Matched Recipes: ~p", [MatchedRecipes]),

    NumRecipes = length(MatchedRecipes),

    NewExperiment = 
        case NumRecipes > 0 of
            true ->
                NewRecipe = lists:nth(util:rand(NumRecipes), MatchedRecipes),

                lager:info("New Recipe: ~p", [NewRecipe]),
                RecipeReq = maps:get(<<"req">>, NewRecipe),

                Experiment#experiment {recipe = NewRecipe,
                                       state = ?EXP_STATE_PROGRESS,
                                       req = RecipeReq};
            false ->
                Experiment#experiment {recipe = none,
                                       state = ?EXP_STATE_NEW_SOURCE,
                                       req = []}
        end,
            
    db:write(NewExperiment).

undiscovered_recipes(AllRecipes, PlayerRecipes) ->
    undiscovered_recipes(AllRecipes, PlayerRecipes, []).

undiscovered_recipes([], _, Undiscovered) ->
    Undiscovered;
undiscovered_recipes([Recipe | Rest], PlayerRecipes , Undiscovered) ->

    F = fun(PlayerRecipe) ->
            PlayerRecipeName = maps:get(<<"name">>, PlayerRecipe),
            RecipeName = maps:get(<<"name">>, Recipe),

            PlayerRecipeName =:= RecipeName
        end,

    IsDiscovered = lists:any(F, PlayerRecipes),

    NewUndiscovered = case IsDiscovered of
                        true -> Undiscovered;
                        false -> [Recipe | Undiscovered]
                      end,

    undiscovered_recipes(Rest, PlayerRecipes, NewUndiscovered).

matched_undiscovered_reqs(Recipes, ExpResources) ->
    matched_undiscovered_reqs(Recipes, ExpResources, []).

matched_undiscovered_reqs([], _ExpResources, MatchedRecipes) ->
    MatchedRecipes;
matched_undiscovered_reqs([Recipe | Rest], ExpResources, MatchedRecipes) ->
    lager:info("Recipe: ~p", [Recipe]),
    lager:info("ExpResources: ~p", [ExpResources]),

    F = fun(RecipeReq) ->
            ReqType = maps:get(<<"type">>, RecipeReq),

            G = fun(ExpResource) ->
                    lager:info("ReqType: ~p", [ReqType]),
                    ExpResSubclass = item:subclass(ExpResource),
                    lager:info("ExpResSubclass: ~p", [ExpResSubclass]),
                    R = ReqType =:= ExpResSubclass,
                    lager:info("R: ~p", [R]),
                    R
                end,

            Result = lists:any(G, ExpResources),
            lager:info("Result: ~p", [Result]),
            Result
        end,

    MatchedReqs = lists:all(F, maps:get(<<"req">>, Recipe)),

    NewMatchedRecipes = case MatchedReqs of
                            true -> [Recipe | MatchedRecipes];
                            false -> MatchedRecipes
                        end,

    matched_undiscovered_reqs(Rest, ExpResources, NewMatchedRecipes).

check_experiment_req(StructureId) ->
    [Experiment] = db:read(experiment, StructureId),

    ResRateList = [{<<"Copper Ingot">>, 1.75},
                   {<<"Maple Timber">>, 0.35}],

    F = fun(ReqMap) ->
            ReqSubclass = maps:get(<<"type">>, ReqMap),
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

    %Reqs are empty if no recipe was found
    (Experiment#experiment.req =/= []) andalso lists:all(F, Experiment#experiment.req).

experiment(StructureId) ->
    lager:info("Processing experiment"),
    [Experiment] = db:read(experiment, StructureId),

    %TODO Handle case where no recipe exists
    RecipeName = maps:get(<<"name">>, Experiment#experiment.recipe),

    ResRateList = [{<<"Copper Ingot">>, 1.75},
                   {<<"Maple Timber">>, 0.35}],

    ExpReqList = Experiment#experiment.req,
    lager:info("ExpReqList: ~p", [ExpReqList]),

    F = fun(ReqMap, Acc) ->
            ReqSubclass = maps:get(<<"type">>, ReqMap),
            ReqQuantity = maps:get(<<"quantity">>, ReqMap),

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

            NewReqMap = maps:update(<<"quantity">>, NewQuantity, ReqMap),
            lager:info("NewReqMap: ~p", [NewReqMap]),
            [NewReqMap | Acc]
        end,

    NewExpReqList = lists:foldl(F, [], ExpReqList),
    lager:info("NewExpReqList: ~p", [NewExpReqList]),

    %Chech if the mininum amount of experimenting has been reached
    G = fun(ReqMap) -> maps:get(<<"quantity">>, ReqMap) =< 0 end,

    ReachedMinExpReq = lists:all(G, NewExpReqList),

    {Result, NewExperiment} = 
        case ReachedMinExpReq of
            true ->
                lager:info("Chance at New Discovery!"),
                case util:rand(100) < 25 of 
                    true -> 
                        lager:info("New Discovered Recipe"),

                        StructureObj = obj:get(StructureId),
                        [ExpItem] = item:get_exp_item(StructureId),

                        %Create new recipe
                        recipe:create(obj:player(StructureObj), RecipeName),

                        %Consume experiment item TODO reconsider the deletion
                        item:update(item:id(ExpItem), 0),
                        
                        {true, Experiment#experiment{exp_item = none,
                                                     state = ?EXP_STATE_DISCOVERY}};
                    false -> 
                        lager:info("Failed to discover this time..."),
                        {false, Experiment#experiment{state = ?EXP_STATE_NEAR}}
                end;
            false ->
                lager:info("Min Experimentation not reached yet"),
                {false, Experiment}
        end,

    %Save updated experiment record
    NewExperiment2 = NewExperiment#experiment {req = NewExpReqList},
    db:write(NewExperiment2),

    %Get updated info experiment
    InfoExperiment = get_info_experiment(StructureId),

    %Send to client
    game:send_update_experiment(StructureId, InfoExperiment),

    %Return discovery result for event repeat
    Result.

remove_experiment(StructureId) ->
    db:delete(experiment, StructureId),

    ExpResources = item:get_exp_resources(StructureId),

    F = fun(ExpResource) ->
            item_attr:set(item:id(ExpResource), ?EXP_RESOURCE_ITEM, none)
        end,

    lists:foreach(F, ExpResources).

get_info_experiment(StructureId) ->
    lager:info("get_info_experiment"),
    Items = item:get_by_owner(StructureId),

    ExperimentItem = item:get_exp_item(Items),
    ExperimentResources = item:get_exp_resources(Items),

    %Filter for valid resources
    H = fun(Item) ->
            IsNotItem = item_attr:value(item:id(Item), ?EXP_ITEM, none) =/= ?TRUE,
            IsNotResource = item_attr:value(item:id(Item), ?EXP_RESOURCE_ITEM, none) =/= ?TRUE,
            IsNotItem and IsNotResource
        end,

    ValidResources = lists:filter(H, Items),

    ExperimentResult = db:read(experiment, StructureId),

    ExpState = case ExperimentResult of
                [Experiment] -> Experiment#experiment.state;
                _ -> ?EXP_STATE_NONE
               end,
    
    ExpRecipe = case ExperimentResult of
                    [Experiment2] -> 
                        case ExpState =:= ?EXP_STATE_DISCOVERY of
                            true -> Experiment2#experiment.recipe;
                            false -> ?EXP_RECIPE_NONE
                        end;
                    _ -> ?EXP_RECIPE_NONE
                end,

    #{<<"id">> => StructureId,
      <<"expitem">> => ExperimentItem,
      <<"expresources">> => ExperimentResources,
      <<"validresources">> => ValidResources,
      <<"expstate">> => ExpState,
      <<"recipe">> => ExpRecipe}.

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

get_updated_req(StructureId) ->
    lager:info("StructureId: ~p", [StructureId]),
    ReqList = obj_attr:value(StructureId, <<"req">>, []),
    Items = item:get_by_owner(StructureId),

    lager:info("ReqList: ~p", [ReqList]),
    updated_req(ReqList, Items).

updated_req([], _Items) ->
    [];
updated_req(Req, Items) ->
    updated_req(Req, Items, []).

updated_req([], _, UpdatedReqList) ->
    UpdatedReqList;
updated_req([Req | ReqRest], Items, UpdatedReqList) ->
    ReqType = maps:get(<<"type">>, Req),
    ReqQuantity = maps:get(<<"quantity">>, Req),
      
    F = fun(Item, AccReqQuantity) ->
            case item:match_req_type(Item, ReqType, ReqQuantity) of
                true -> 
                    util:subtract_until_zero(AccReqQuantity, item:quantity(Item));
                false ->
                    AccReqQuantity
            end
        end,

    NewReqQuantity = lists:foldl(F, ReqQuantity, Items),

    %Add current quantity to map
    NewReq = maps:put(<<"cquantity">>, NewReqQuantity, Req),
    NewUpdatedReqList = [NewReq | UpdatedReqList],

    updated_req(ReqRest, Items, NewUpdatedReqList).


check_recipe_req(ObjId, RecipeName) ->
    Items = item:get_by_owner(ObjId),

    Recipe = recipe:get_recipe(RecipeName),
    ReqList = maps:get(<<"req">>, Recipe),
    lager:info("ReqList: ~p Items: ~p", [ReqList, Items]),
    HasReq = has_req(ReqList, Items),
    lager:info("HasReq: ~p", [HasReq]),
    HasReq.
 
list(PlayerId) ->
    DeedRecList = db:read(deed, PlayerId),

    F = fun(A, B) ->
            {A#deed.structure, A#deed.tier} < {B#deed.structure, B#deed.tier}
        end,

    SortedDeedRecList = lists:sort(F, DeedRecList),

    G = fun(DeedRec, DeedList) ->
            Name = DeedRec#deed.structure,
            Template = obj_template:all_to_map(Name),
 
            Image= string:lowercase(re:replace(Name, 
                                               <<" ">>, <<"">>, 
                                               [{return, binary}])),

            TemplateWithImage = maps:put(<<"image">>, Image, Template),
            [TemplateWithImage | DeedList]
        end,
    
    Deeds = lists:reverse(lists:foldl(G, [], SortedDeedRecList)),

    lager:info("Deeds: ~p", [Deeds]),

    Deeds.

add_deed(PlayerId, Name, Level, Tier) ->
    Deed = #deed {player = PlayerId,
                  structure = Name,
                  level = Level,
                  tier = Tier},
    db:write(Deed).

recipe_list(Obj) ->
    Recipes = recipe:get_recipes(Obj#obj.template),
    Recipes.

has_refine_resources(StructureId) ->
    case obj_attr:value(StructureId, <<"refine">>, none) of
        none -> 
            false;
        RefineAttr when is_list(RefineAttr) ->
            F = fun(RefineClass) ->
                    item:get_by_class(StructureId, RefineClass) =/= []
                end,

            lists:any(F, RefineAttr);
        RefineAttr -> 
            item:get_by_class(StructureId, RefineAttr) =/= []
    end.

refine(StructureId) ->
    RefineAttr = obj_attr:value(StructureId, <<"refine">>),

    RefineClassList =
        case RefineAttr of
            Value when is_list(RefineAttr) -> Value;
            Value -> [Value]
        end,
    
    Item = select_refine_item(StructureId, RefineClassList),

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

select_refine_item(StructureId, RefineClassList) ->
    select_refine_item(StructureId, RefineClassList, false).

select_refine_item(StructureId, [RefineClass | Rest], false) ->
    Result = 
        case item:get_by_class(StructureId, RefineClass) of
            [] -> false;
            [Item | _] -> Item
        end,

    select_refine_item(StructureId, Rest, Result);
select_refine_item(_, _, SelectedItem) ->
    SelectedItem.

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


    