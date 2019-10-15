%% Author: Peter
%% Created: Jan, 2015
%% Description: Resource module
-module(resource).

-include("common.hrl").
-include("schema.hrl").

-export([gather_by_type/3, gather_by_type/4, harvest/3, 
         explore/2, survey/1, type_to_skill/1]).
-export([is_valid/2, is_valid_type/2, is_auto/2, quantity/1]).
-export([get_by_type/2, get_num_unrevealed/1]).
-export([create/4, generate_effects/0]).

gather_by_type(GathererId, ResourceType, Pos) ->
    gather_by_type(GathererId, GathererId, ResourceType, Pos).

gather_by_type(GathererId, DestId, ResourceType, Pos) ->
    Resources = get_by_type(Pos, ResourceType),

    F = fun(Resource) ->
            ResourceDef = resource_def:all_to_map(Resource#resource.name),
            ResourceSkillReq = maps:get(<<"skill_req">>, ResourceDef),
               
            SkillTypeReq = type_to_skill(ResourceType),
            SkillValue = skill:get_by_name(GathererId, SkillTypeReq),
            lager:info("Skill ~p Value: ~p", [SkillTypeReq, SkillValue]),
            
            GatherChance = gather_chance({SkillValue, ResourceSkillReq}), 

            Random = util:rand(),
            lager:info("Resource: ~p Gather Chance ~p Random: ~p", [Resource#resource.name, GatherChance, Random]),

            case Random < GatherChance of
                true ->
                    harvest(DestId, Resource, 1);
                false ->
                    nothing
            end
                    
        end, 

    lists:foreach(F, Resources).

harvest(ObjId, Resource, HarvestQuantity) ->
    lager:info("ObjId: ~p Resource: ~p", [ObjId, Resource]),
    ItemWeight = item:weight(Resource#resource.name, HarvestQuantity),

    case obj:has_space(ObjId, ItemWeight) of
        true ->
            %Decrement the resource quantity on the tile
            update_resource(Resource, HarvestQuantity),

            %Create new item
            NewItem = item:create(ObjId, Resource#resource.name, HarvestQuantity),
            lager:info("Creating Item ~p", [NewItem]),
        
            %Set quantity to 1, as the returns of this function 
            %should be only the new quantity not combined quantity 
            NewItemOnly = maps:update(<<"quantity">>, HarvestQuantity, NewItem),
            [NewItemOnly];
        false ->
            {error, <<"Not enough capacity">>}
    end.

get_by_type(Pos, Type) ->
    Resources = db:read(resource, Pos),

    F = fun(Resource) ->
            ResourceDef = resource_def:all_to_map(Resource#resource.name),
            ResourceType = maps:get(<<"type">>, ResourceDef, none),

            (Resource#resource.revealed =:= true) and (ResourceType =:= Type)             
        end,

    lists:filter(F, Resources).

get_num_unrevealed(Pos) ->
    Resources = db:read(resource, Pos),

    F = fun(Resource) ->
            Resource#resource.revealed =:= false
        end,

    UnrevealedList = lists:filter(F, Resources),
    length(UnrevealedList).

survey(Pos) ->
    lager:info("Survey ~p", [Pos]),
    Resources = db:read(resource, Pos),

    F = fun(Resource, ResourceList) ->
            case Resource#resource.revealed of
                true ->
                    ResourceMap = #{<<"name">> => Resource#resource.name,
                    <<"quantity">> => quantity(Resource#resource.quantity)},
                    [ResourceMap | ResourceList];
                false ->
                    ResourceList
            end
        end,

    lists:foldl(F, [], Resources).

explore(ObjId, Pos) ->
    lager:info("Explore ~p ~p", [ObjId, Pos]),

    ExploreSkill = 50,
    Resources = db:read(resource, Pos),

    F = fun(Resource, {ResourceList, FoundResource}) ->
            ResourceDef = resource_def:all_to_map(Resource#resource.name),
            ResourceSkillReq = maps:get(<<"skill_req">>, ResourceDef),
            QuantityList = maps:get(<<"quantity">>, ResourceDef),
            QuantitySkillReq = quantity_skill_req(Resource#resource.max, QuantityList),
            
            case ExploreSkill >= (ResourceSkillReq + QuantitySkillReq) of
                true ->
                    NewResource = reveal(Resource), 
                        
                    NewResourceList = [#{<<"name">> => NewResource#resource.name,
                                         <<"quantity">> => quantity(NewResource#resource.quantity)} 
                                       | ResourceList],
                    {NewResourceList, true};
                false ->
                    {ResourceList, FoundResource}
            end
        end,

    {FinalResourceList, RevealedResources} = lists:foldl(F, {[], false}, Resources),

    case RevealedResources of
       true ->
           game:send_tile_update(Pos, <<"resource">>, FinalResourceList);
       false -> 
           nothing
    end,

    %Return resource list
    FinalResourceList.

reveal(Resource) ->
    NewResource = Resource#resource {revealed = true},

    F = fun() ->

            mnesia:delete_object(Resource),
            mnesia:write(NewResource)
        end,

    mnesia:transaction(F),

    NewResource.

is_valid(ResourceName, Pos) ->
    Resources = db:read(resource, Pos),
    F = fun(Resource) -> 
            (Resource#resource.name =:= ResourceName) and 
            (Resource#resource.quantity > 0) and
            (Resource#resource.revealed =:= true)
        end,
    lists:any(F, Resources).

is_valid_type(Type, Pos) ->
    Resources = db:read(resource, Pos),
    F = fun(Resource) -> 
            ResourceDef = resource_def:all_to_map(Resource#resource.name),
            ResourceType = maps:get(<<"type">>, ResourceDef, none),

            (ResourceType =:= Type) and 
            (Resource#resource.quantity > 0) and
            (Resource#resource.revealed =:= true)
        end,
    lists:any(F, Resources).


is_auto(Objs, _Resource) ->
    
    F = fun(Obj) ->
            Obj#obj.template =:= <<"Lumbermill">>
        end,

    lists:any(F, Objs).

quantity(<<"high">>) -> 50;
quantity(<<"average">>) -> 25;
quantity(<<"low">>) -> 10;
quantity(Quantity) when Quantity > 25 -> <<"high">>;
quantity(Quantity) when Quantity > 10 -> <<"average">>;
quantity(Quantity) when Quantity >  0 -> <<"low">>;
quantity(0) -> <<"exhausted">>;
quantity(_) -> lager:info("Error converting quantity").

create(ResourceType, Quantity, Pos, WithObj) ->
    lager:debug("Creating resource: (~p / ~p / ~p)", [ResourceType, Quantity, Pos]), 
    ObjId = case WithObj of
                true ->    
                    obj:create(Pos, ResourceType, ResourceType);
                false ->
                    none
            end,

    Resource = #resource {index = Pos,
                          name = ResourceType,
                          max = Quantity,
                          quantity = Quantity,
                          obj = ObjId},

    db:write(Resource).

generate_effects() ->
    ResourceDefList = resource_def:list(),

    F = fun(ResourceName) ->
            ResourceDef = resource_def:all_to_map(ResourceName),
            ResourceType = maps:get(<<"type">>, ResourceDef, none),

            case ResourceType of
                <<"Ore">> ->
                    Effects = randomize_effects(ResourceType),
                    item_def:add(ResourceName, <<"effects">>, Effects);
                _ -> 
                    none
            end
        end, 

    lists:foreach(F, ResourceDefList).

randomize_effects(ResourceType) ->
    case ResourceType of
        <<"Ore">> ->
            OreEffects = ore_effects(),

            PosEffect = lists:nth(util:rand(length(OreEffects)), OreEffects),

            NewOreEffects = lists:delete(PosEffect, OreEffects),

            NegEffect = lists:nth(util:rand(length(NewOreEffects)), NewOreEffects),

            PosEffectMap = #{<<"type">> => PosEffect,
                             <<"value">> => (util:rand(40) + 10) / 100},

            NegEffectMap = #{<<"type">> => NegEffect,
                             <<"value">> => -1 * (util:rand(40) + 10) / 100},

            [PosEffectMap, NegEffectMap];
        _ ->
            []
    end.

ore_effects() ->
    [?AXE_DMG_P,
     ?SWORD_DMG_P,
     ?HAMMER_DMG_P,
     ?DAGGER_DMG_P,
     ?SPEAR_DMG_P,
     ?AXE_SPD_P,
     ?SWORD_SPD_P,
     ?HAMMER_SPD_P,
     ?DAGGER_SPD_P,
     ?SPEAR_SPD_P].

%
% Internal functions
%

name(Resource) when is_record(Resource, resource) -> Resource#resource.name;
name(_Resource) -> none.

get_resource(ResourceName, Pos) ->
    case db:read(resource, Pos) of
        [] -> false;
        Resources ->
            lists:keyfind(ResourceName, #resource.name, Resources)
    end.

update_resource(Resource, HarvestQuantity) ->
    Quantity = Resource#resource.quantity,
    case (Quantity - HarvestQuantity) > 0 of
        true ->          
            mnesia:dirty_delete_object(Resource),
            NewResource = Resource#resource {quantity = Quantity - HarvestQuantity},
            db:write(NewResource);
        false ->
            case Resource#resource.obj =/= none of
                true ->
                    obj:remove(Resource#resource.obj);
                false ->
                    nothing
            end

            %Leave resource at quantity zero
            %db:delete(resource, Resource#resource.index)
    end.

quantity_skill_req(Max, QuantityRate) ->
    lager:info("~p ~p", [Max, QuantityRate]),
    quantity_skill_req(util:index_of(Max, QuantityRate)).

quantity_skill_req(1) -> 0;
quantity_skill_req(2) -> 0;
quantity_skill_req(3) -> 10;
quantity_skill_req(4) -> 20;
quantity_skill_req(5) -> 30;
quantity_skill_req(6) -> 40;
quantity_skill_req(7) -> 50.




%{Explore Skill, Resource Req Skill}
explore_resource({0,0}) -> 0.1;
explore_resource({1,0}) -> 0.2;
explore_resource({2,0}) -> 0.3;
explore_resource({3,0}) -> 0.4;
explore_resource({4,0}) -> 0.5;
explore_resource({5,0}) -> 0.6;

explore_resource({0,25}) -> 0.00016;
explore_resource({1,25}) -> 0.00032;
explore_resource({2,25}) -> 0.00048;
explore_resource({3,25}) -> 0.00064;
explore_resource({4,25}) -> 0.0008;
explore_resource({5,25}) -> 0.00096;

explore_resource({0,50}) -> 0.00004;
explore_resource({1,50}) -> 0.00008;
explore_resource({2,50}) -> 0.00012;
explore_resource({3,50}) -> 0.00016;
explore_resource({4,50}) -> 0.0002;
explore_resource({5,50}) -> 0.00024.


%{Explore Skill, Resource Req Skill}
%TODO needs more values 
gather_chance({0,0}) -> 0.5;
gather_chance({1,0}) -> 0.2;
gather_chance({2,0}) -> 0.3;
gather_chance({3,0}) -> 0.4;
gather_chance({4,0}) -> 0.5;
gather_chance({5,0}) -> 0.6;
gather_chance({_,0}) -> 1.0;

gather_chance({0,25}) -> 0.00016;
gather_chance({1,25}) -> 0.00032;
gather_chance({2,25}) -> 0.00048;
gather_chance({3,25}) -> 0.00064;
gather_chance({4,25}) -> 0.0008;
gather_chance({5,25}) -> 0.00096;

gather_chance({0,50}) -> 0.00004;
gather_chance({1,50}) -> 0.00008;
gather_chance({2,50}) -> 0.00012;
gather_chance({3,50}) -> 0.00016;
gather_chance({4,50}) -> 0.0002;
gather_chance({5,50}) -> 0.00024;
gather_chance({_,_}) -> 1.0.





type_to_skill(?ORE) -> ?MINING;
type_to_skill(?WOOD) -> ?WOODCUTTING;
type_to_skill(?STONE) -> ?STONECUTTING;
type_to_skill(?WATER) -> ?GATHERING;
type_to_skill(?FOOD) -> ?FARMING.




