%% Author: Peter
%% Created: Jan, 2015
%% Description: Resource module
-module(resource).

-include("common.hrl").
-include("schema.hrl").

-export([harvest/2, contains/2]).

harvest(ObjId, ResourceType) ->
    NewItem = item:create(ObjId, ResourceType, 1),
    [NewItem].

contains(ResourceType, Pos) ->
    [Tile] = map:get_tile(Pos),
    TileType = Tile#global_map.tile,
    
    Resources = db:dirty_read(resource, ResourceType),
    lists:keymember(TileType, #resource.tile_type, Resources).
%
% Internal functions
%

