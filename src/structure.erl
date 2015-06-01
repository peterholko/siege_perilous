%% Author: Peter
%% Created: Feb, 2015
%% Description: Structure module
-module(structure).

-include_lib("stdlib/include/ms_transform.hrl").

-include("common.hrl").
-include("schema.hrl").

-export([start_build/4, check_req/1, valid_location/3]).
-export([is_wall/1, list/0, craft_list/1]).

start_build(PlayerId, GlobalPos, LocalPos, StructureType) ->
    {Name} = bson:lookup(name, StructureType),
    {Subclass} = bson:lookup(subclass, StructureType),

    StructureId = local:create(GlobalPos, 
                               none,
                               LocalPos, 
                               PlayerId,
                               structure,
                               Subclass,
                               Name, 
                               building),
    StructureId.

check_req(Structure) ->
    {StructureId} = bson:lookup('_id', Structure),
    {ReqList} = bson:lookup(req, Structure),
    Items = item:get_by_owner(StructureId),

    process_req(true, ReqList, Items).

list() ->
    Structures = find_type(level, 1),
    Structures.

craft_list(LocalObj) ->
    LocalObjType = local_obj:get_type(LocalObj#local_obj.name),
    {CraftList} = bson:lookup(craft, LocalObjType),

    F = fun(ItemName, CraftReqList) ->
            ItemType = item:get_info(ItemName),
            [ItemType | CraftReqList]
        end,

    lists:foldl(F, [], CraftList).


valid_location(<<"wall">>, GlobalPos, LocalPos) ->
    lager:info("Valid location for wall"),
    %TODO determine atom vs binary for class and subclass
    MS = ets:fun2ms(fun(N = #local_obj{global_pos = GPos, 
                                       pos = LPos, 
                                       class = structure,
                                       subclass = <<"wall">>}) when GPos =:= GlobalPos, 
                                                                    LPos =:= LocalPos -> N end),
    LocalObjs = db:select(local_obj, MS),

    % True if no local_objs, False if Wall local_objs
    LocalObjs =:= [];

valid_location(_, GlobalPos, LocalPos) ->
    %TODO determine atom vs binary for class and subclass
    MS = ets:fun2ms(fun(N = #local_obj{global_pos = GPos, 
                                       pos = LPos, 
                                       class = structure,
                                       subclass = Subclass}) when Subclass =/= <<"wall">>,
                                                                  GPos =:= GlobalPos, 
                                                                  LPos =:= LocalPos -> N end),
    LocalObjs = db:select(local_obj, MS),
    lager:info("LocalObjs: ~p", [LocalObjs]),

    % True if no local_objs, False if Wall local_objs
    LocalObjs =:= [].

%
% Internal functions
%

process_req(Result, [], _Items) ->
    Result;
process_req(Result, [{type, ReqType, quantity, ReqQuantity} | Rest], Items) ->
    F = fun(Item, HasReq) ->
            ItemStats = item:get_stats(Item),

            {ItemName} = bson:lookup(name, ItemStats),
            {ItemSubClass} = bson:lookup(subclass, ItemStats),
            {ItemQuantity} = bson:lookup(quantity, ItemStats),

            QuantityMatch = ReqQuantity =< ItemQuantity,            
            ItemNameMatch = ReqType =:= ItemName,
            ItemSubClassMatch = ReqType =:= ItemSubClass,

            lager:info("NameMatch ~p ~p ~p", [ReqType, ItemName, ItemSubClass]),
            lager:info("QuantityMatch ~p ~p", [ReqQuantity, ItemQuantity]),

            ItemMatch = ItemNameMatch or ItemSubClassMatch,
            lager:info("ItemMatch: ~p", [ItemMatch]),
            ItemMatch and QuantityMatch or HasReq
        end,

    ReqMatch = lists:foldl(F, false, Items),
    NewResult = Result and ReqMatch,

    process_req(NewResult, Rest, Items).

is_wall(StructureName) ->
    {Structure} = local_obj:get_type(StructureName),
    Result = case bson:lookup(is_wall, Structure) of
                {} ->
                    false;
                {true} ->
                    true
             end,
    Result.

find_type(Key, Value) ->
    Cursor = mongo:find(mdb:get_conn(), <<"local_obj_type">>, {Key, Value, class, <<"structure">>}),
    Structures = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Structures.


