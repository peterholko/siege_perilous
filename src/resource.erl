%% Author: Peter
%% Created: Jan, 2015
%% Description: Resource module
-module(resource).

-include("schema.hrl").

-export([harvest/3]).

harvest(Obj, Pos, Type) ->
    item:create(Obj, Type, 1).
