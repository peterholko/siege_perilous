%% Author: Peter
%% Created: Jan, 2015
%% Description: Resource module
-module(resource).

-include("schema.hrl").

-export([harvest/2]).

has_resource(Pos, Type) ->
    {X, Y} = Pos,
    Tile = map:get_tile(X, Y).

harvest(ObjId, Type) ->
    item:create(ObjId, Type, 1).
