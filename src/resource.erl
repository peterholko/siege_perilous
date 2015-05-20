%% Author: Peter
%% Created: Jan, 2015
%% Description: Resource module
-module(resource).

-include("common.hrl").
-include("schema.hrl").

-export([harvest/2, survey/1, quantity/1]).

harvest(ObjId, ResourceType) ->
    NewItem = item:create(ObjId, ResourceType, 1),
    [NewItem].

survey(Pos) ->
    lager:info("Survey ~p", [Pos]),
    Resources = db:read(resource, {1, Pos}),

    F = fun(Resource, ResourceList) ->
            ResourceMap = #{<<"name">> => Resource#resource.name,
                            <<"quantity">> => quantity(Resource#resource.quantity)},
            [ResourceMap | ResourceList]
        end,

    lists:foldl(F, [], Resources).

quantity(<<"high">>) -> 50;
quantity(<<"average">>) -> 25;
quantity(<<"low">>) -> 10;
quantity(Quantity) when Quantity > 25 -> <<"high">>;
quantity(Quantity) when Quantity > 10 -> <<"average">>;
quantity(Quantity) when Quantity >  0 -> <<"low">>;
quantity(_) -> lager:info("Error converting quantity").

%
% Internal functions
%

    
