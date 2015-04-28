%% Author: Peter
%% Created: Feb, 2015
%% Description: Structure module
-module(structure).

-include("common.hrl").
-include("schema.hrl").

-export([start_build/4]).

start_build(PlayerId, GlobalPos, LocalPos, StructureName) ->
    lager:info("GlobalPos: ~p LocalPos: ~p StructureName: ~p", [GlobalPos, LocalPos, StructureName]), 
    StructureId = local:create(GlobalPos, 
                               none,
                               LocalPos, 
                               PlayerId,
                               structure, 
                               StructureName, 
                               building),
    StructureId.
%
% Internal functions
%

