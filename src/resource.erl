%% Author: Peter
%% Created: Jan, 2015
%% Description: Resource module
-module(resource).

-include("common.hrl").
-include("schema.hrl").

-export([harvest/3, survey/1, is_valid/2, is_auto/2, quantity/1]).
-export([create/3]).

harvest(ObjId, ResourceType, Pos) ->
    [Resource] = db:read(resource, Pos),
    Quantity = Resource#resource.quantity,
    NewResource = Resource#resource {quantity = Quantity - 1},
    db:write(NewResource),

    NewItem = item:create(ObjId, ResourceType, 1),
    [NewItem].

survey(Pos) ->
    lager:info("Survey ~p", [Pos]),
    Resources = db:read(resource, Pos),

    F = fun(Resource, ResourceList) ->
            ResourceMap = #{<<"name">> => Resource#resource.name,
                            <<"quantity">> => quantity(Resource#resource.quantity)},
            [ResourceMap | ResourceList]
        end,

    lists:foldl(F, [], Resources).

is_valid(Pos, Resource) ->
    Resources = db:read(resource, Pos),
    
    F = fun(TileResource) ->
            TileResource#resource.name =:= Resource
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
quantity(_) -> lager:info("Error converting quantity").

create(ResourceType, Quantity, Pos) ->
    Resource = #resource {index = Pos,
                          name = ResourceType,
                          max = Quantity,
                          quantity = Quantity},

    db:write(Resource).

%
% Internal functions
%

    
