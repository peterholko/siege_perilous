%% Author: Peter
%% Created: Mar, 2018
%% Description: Magic module
-module(magic).

-include("schema.hrl").
-include("common.hrl").

-export([cast/3]).


cast(SourceObj, TargetObj, ?RAISE_DEAD) ->

    case obj:subclass(TargetObj) of
        ?CORPSE ->
            npc:generate(obj:pos(TargetObj), obj:player(SourceObj), <<"Zombie">>);
        ?BONES -> 
            npc:generate(obj:pos(TargetObj), obj:player(SourceObj), <<"Skeleton">>);
        _ ->
            lager:info("Raise dead failed, invalid target type")
    end.






