%% Author: Peter
%% Created: Mar, 2018
%% Description: Magic module
-module(magic).

-include("schema.hrl").
-include("common.hrl").

-export([cast/4]).

cast(invalid, _, _, _) -> nothing;
cast(_, invalid, _, _) -> nothing;
cast(SourceObj, Target, TargetType, ?RAISE_DEAD) ->

    {TargetPos, CorpseOrBones} = case TargetType of
                                  obj -> 
                                      %Initiate deletion of the object
                                      obj:update_deleting(Target),
                                      {obj:pos(Target), obj:subclass(Target)};
                                  item ->
                                      Item = Target,
                                      OwnerId = Item#item.owner,
                                      OwnerObj = obj:get(OwnerId),
                                      item:update(Item#item.id, Item#item.quantity - 1),
                                      {obj:pos(OwnerObj), Item#item.subclass}
                              end,

    case CorpseOrBones of
        ?CORPSE ->
            npc:create(TargetPos, obj:player(SourceObj), <<"Zombie">>);
        ?BONES -> 
            NPCId = npc:create(TargetPos, obj:player(SourceObj), <<"Skeleton">>),
            npc:set_order(NPCId, idle);
        _ ->
            lager:info("Raise dead failed, invalid target type")
    end;

cast(SourceObj, TargetObj, _, ?SHADOW_BOLT) ->
    lager:info("Shadow Bolt"),

    combat:spell(?SHADOW_BOLT, obj:id(SourceObj), obj:id(TargetObj)).
