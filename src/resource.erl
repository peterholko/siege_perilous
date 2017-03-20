%% Author: Peter
%% Created: Jan, 2015
%% Description: Resource module
-module(resource).

-include("common.hrl").
-include("schema.hrl").

-export([harvest/3, prospect/2, survey/1, is_valid/2, is_auto/2, quantity/1]).
-export([create/4]).

harvest(ObjId, ResourceName, Pos) ->
    lager:info("Harvesting resource: ~p", [ResourceName]),
    Resource = get_resource(ResourceName, Pos),
    harvest(ObjId, Resource).

harvest(_ObjId, false) -> 
    {error, <<"Invalid resource">>};
harvest(ObjId, Resource) ->
    HarvestQuantity = 1, 
    ItemWeight = item:weight(name(Resource), HarvestQuantity),

    case obj:has_space(ObjId, ItemWeight) of
        true ->
            %Decrement the resource quantity on the tile
            update_resource(Resource, HarvestQuantity),

            %Create new item
            NewItem = item:create(ObjId, name(Resource), 1),
        
            %Set quantity to 1, as the returns of this function 
            %should be only the new quantity not combined quantity 
            NewItemOnly = maps:update(<<"quantity">>, 1, NewItem),
            [NewItemOnly];
        false ->
            {error, <<"Not enough capacity">>}
    end.


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

prospect(ObjId, Pos) ->
    lager:info("Prospect ~p ~p", [ObjId, Pos]),

    ProspectSkill = 50,
    Resources = db:read(resource, Pos),

    F = fun(Resource, NewResourceList) ->
            ResourceDef = resource_def:all_to_map(Resource#resource.name),
            ResourceSkillReq = maps:get(<<"skill_req">>, ResourceDef),
            QuantityList = maps:get(<<"quantity">>, ResourceDef),
            QuantitySkillReq = quantity_skill_req(Resource#resource.max, QuantityList),
            
            case ProspectSkill >= (ResourceSkillReq + QuantitySkillReq) of
                true ->
                    NewResource = Resource#resource {revealed = true},                  
                    db:write(NewResource),

                    [#{<<"name">> => Resource#resource.name,
                       <<"quantity">> => quantity(Resource#resource.quantity)} | NewResourceList];
                false ->
                    NewResourceList
            end
        end,

    lists:foldl(F, [], Resources).

is_valid(ResourceName, Pos) ->
    Resources = db:read(resource, Pos),
    F = fun(Resource) -> 
            (Resource#resource.name =:= ResourceName) and 
            (Resource#resource.quantity > 0) and
            (Resource#resource.revealed =:= true)
        end,
    lists:any(F, Resources).

is_auto(Objs, _Resource) ->
    
    F = fun(Obj) ->
            Obj#obj.name =:= <<"Lumbermill">>
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
                    obj:create(Pos, -1, resource, ResourceType, ResourceType, none);
                false ->
                    none
            end,

    Resource = #resource {index = Pos,
                          name = ResourceType,
                          max = Quantity,
                          quantity = Quantity,
                          obj = ObjId},

    db:write(Resource).

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
    lager:info("Resource: ~p HarvestQuantity: ~p", [Resource, HarvestQuantity]),
    Quantity = Resource#resource.quantity,
    case (Quantity - HarvestQuantity) > 0 of
        true ->          
            NewResource = Resource#resource {quantity = Quantity - HarvestQuantity},
            db:write(NewResource);
        false ->
            case Resource#resource.obj =/= none of
                true ->
                    obj:remove(Resource#resource.obj);
                false ->
                    nothing
            end,

            db:delete(resource, Resource#resource.index)
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

