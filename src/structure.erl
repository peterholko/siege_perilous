%% Author: Peter
%% Created: Feb, 2015
%% Description: Structure module
-module(structure).

-include("common.hrl").
-include("schema.hrl").

-export([start_build/4, check_req/1]).
-export([is_wall/1]).

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

check_req(Structure) ->
    {StructureId} = bson:lookup('_id', Structure),
    {ReqList} = bson:lookup(req, Structure),
    Items = item:get_by_owner(StructureId),

    process_req(false, ReqList, Items).
%
% Internal functions
%

process_req(Result, [], _Items) ->
    Result;
process_req(Result, [{type, ReqType, quantity, ReqQuantity} | Rest], Items) ->
    
    F = fun(Item, HasReq) ->
            {ItemName} = bson:lookup(name, Item),
            {ItemQuantity} = bson:lookup(quantity, Item),
            lager:info("Item: ~p ~p", [ItemName, ItemQuantity]),        
            ItemMatch = match_req({ReqType, ReqQuantity}, {ItemName, ItemQuantity}),
            lager:info("ItemMatch: ~p", [ItemMatch]),
            HasReq or ItemMatch
        end,

    ReqMatch = lists:foldl(F, false, Items),
    NewResult = Result or ReqMatch,

    process_req(NewResult, Rest, Items).

match_req({ReqType, ReqQuantity}, {ReqType, ItemQuantity}) when ReqQuantity =< ItemQuantity -> true;
match_req({_, _}, {_, _}) -> false.

is_wall(StructureName) ->
    {Structure} = local_obj:get_type(StructureName),
    Result = case bson:lookup(is_wall, Structure) of
                {} ->
                    false;
                {true} ->
                    true
             end,
    Result.
    
