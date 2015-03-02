%% Author: Peter
%% Created: Feb, 2015
%% Description: Local map module
-module(local).

-include("common.hrl").
-include("schema.hrl").

-export([init_perception/2, create/6, update_state/2]).

init_perception(Pos, TileType) ->
    LocalObjList = db:read(local_obj, Pos),

    LocalMap = get_map(TileType),
    LocalObjData = get_obj_data(LocalObjList, []),
    {LocalMap, LocalObjData}.

create(Global, Id, Pos, Class, Type,  State) ->
    LocalObj = #local_obj {global = Global,
                           id = Id,
                           pos = Pos,
                           class = Class,
                           type = Type,
                           state = State},

    db:write(LocalObj).    

update_state(Id, State) ->
    lager:info("Update state: ~p ~p", [Id, State]),
    %TODO make transaction
    [LocalObj] = db:index_read(local_obj, Id, #local_obj.id),
    NewLocalObj = LocalObj#local_obj {state = State},
    db:write(NewLocalObj).
 
%
% Internal functions
%

get_map(TileType) ->
    LocalMap = db:read(local_map, TileType),

    F = fun(TileData, MsgTiles) ->
            {local_map, _LocalType, Pos, Type} = TileData,
            {X, Y} = Pos,
            [#{<<"x">> => X,
               <<"y">> => Y,
               <<"t">> => Type} | MsgTiles]
        end,

    lists:foldl(F, [], LocalMap).

get_obj_data([], ObjData) ->
    ObjData;
get_obj_data([Obj | Rest], ObjData) ->
    {X, Y} = Obj#local_obj.pos,
    NewObjData = [ #{<<"id">> => Obj#local_obj.id,
                     <<"x">> => X,
                     <<"y">> => Y,
                     <<"type">> => Obj#local_obj.type,
                     <<"state">> => Obj#local_obj.state} | ObjData],

    get_obj_data(Rest, NewObjData).


