%% Author: Peter
%% Created: Jan, 2015
%% Description: Resource module
-module(resource).

-include("common.hrl").
-include("schema.hrl").

-export([harvest/3, survey/1, is_valid/1, is_auto/2, quantity/1]).
-export([create/4]).

harvest(ObjId, ResourceType, Pos) ->
    [Resource] = db:read(resource, Pos),
    lager:info("Harvesting resource: ~p", [Resource]),

    HarvestQuantity = 1, 
    ItemWeight = item:weight(ResourceType, HarvestQuantity),

    case obj:has_space(ObjId, ItemWeight) of
        true ->
            %Decrement the resource quantity on the tile
            update_resource(Resource, HarvestQuantity),

            %Create new item
            NewItem = item:create(ObjId, ResourceType, 1),
        
            %Set quantity to 1, as the returns of this function 
            %should be only the new quantity not combined quantity 
            NewItemOnly = maps:update(<<"quantity">>, 1, NewItem),
            [NewItemOnly];
        false ->
            {error, <<"Not enough capacity">>}
    end.

update_resource(Resource, HarvestQuantity) ->
    Quantity = Resource#resource.quantity,
    case (Quantity - HarvestQuantity) > 0 of
        true ->          
            case Resource#resource.obj =/= none of
                true ->
                    obj:remove(Resource#resource.obj);
                false ->
                    nothing
            end,

            db:delete(resource, Resource#resource.index);
        false ->
            NewResource = Resource#resource {quantity = Quantity - HarvestQuantity},
            db:write(NewResource)
    end.

survey(Pos) ->
    lager:info("Survey ~p", [Pos]),
    Resources = db:read(resource, Pos),

    F = fun(Resource, ResourceList) ->
            ResourceMap = #{<<"name">> => Resource#resource.name,
                            <<"quantity">> => quantity(Resource#resource.quantity)},
            [ResourceMap | ResourceList]
        end,

    lists:foldl(F, [], Resources).

is_valid(Pos) ->
    [] =/= db:read(resource, Pos).

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

    
