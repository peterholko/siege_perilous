%% Author: Peter
%% Created: Mar, 2018
%% Description: Magic module
-module(magic).

-include("schema.hrl").
-include("common.hrl").

-export([cast/3]).


cast(SourceId, {Type, TargetId}, ?RAISE_DEAD) ->

    %TODO check source obj
    SourceObj = obj:get(SourceId),

    {TargetPos, TargetType} = case Type of
                                  obj -> 
                                      TargetObj = obj:get(TargetId),
                                      obj:delete(obj:id(TargetObj)),
                                      {obj:pos(TargetObj), obj:subclass(TargetObj)};
                                  item ->
                                      Item = item:get_rec(TargetId),
                                      OwnerId = Item#item.owner,
                                      OwnerObj = obj:get(OwnerId),
                                      item:update(TargetId, Item#item.quantity - 1),
                                      {obj:pos(OwnerObj), Item#item.subclass}
                              end,

    case TargetType of
        ?CORPSE ->
            npc:create(TargetPos, obj:player(SourceObj), <<"Zombie">>);
        ?BONES -> 
            NPCId = npc:create(TargetPos, obj:player(SourceObj), <<"Skeleton">>),
            npc:set_order(NPCId, idle);
        _ ->
            lager:info("Raise dead failed, invalid target type")
    end;

cast(SourceId, TargetId, ?SHADOW_BOLT) ->
    lager:info("Shadow Bolt"),

    %TODO check source obj and target for invalid
    SourceObj = obj:get(SourceId),
    TargetObj = obj:get(TargetId),

    combat:spell(?SHADOW_BOLT, obj:id(SourceObj), obj:id(TargetObj)).
