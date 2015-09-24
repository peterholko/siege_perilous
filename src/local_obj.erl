%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to local obj mongodb data
-module(local_obj).

-include_lib("stdlib/include/ms_transform.hrl").

-include("schema.hrl").
-include("common.hrl").

-export([get/1, get_info/1, get_type/1, get_stats/1, units_from_obj/1, units_stats_from_obj/1]).
-export([create/3, update/3, remove/1]).
-export([find_type/1, is_nearby_hero/2, readtest/1]).
-export([get_by_pos/2, get_unit_by_pos/2, get_wall/2]).

get(Id) ->
    LocalObj = find(Id),
    LocalObj.

get_info(Id) ->
    LocalObjInfo = case find(Id) of
                [LocalObj] ->
                    %TODO compare requester id to unit player id
                    info(LocalObj);
                _ ->
                    none
               end,
    LocalObjInfo.

units_from_obj(ObjId) ->
    find_units(ObjId).

units_stats_from_obj(ObjId) ->
    Units = find_units(ObjId),

    F = fun(Unit, UnitStats) ->
                UnitWithStats = stats(Unit),
                [UnitWithStats | UnitStats]
        end,
    lager:info("units: ~p", [Units]),
    lists:foldl(F, [], Units).

get_type(TypeName) ->
    {LocalObjType} = find_type(TypeName),
    LocalObjType.

get_stats(Id) ->
    LocalObj = find(Id),
    stats(LocalObj).

create(ObjId, structure, TypeName) ->
    {LocalObjType} = find_type(TypeName),
    [LocalObj] = insert(ObjId, LocalObjType),
    {LocalObjId} = bson:lookup('_id', LocalObj),
    update(LocalObjId, hp, 1),
    [LocalObj];

create(ObjId, unit, TypeName) ->
    {LocalObjType} = find_type(TypeName),
    [LocalObj] = insert(ObjId, LocalObjType),
    {LocalObjId} = bson:lookup('_id', LocalObj),
    {Stamina} = bson:lookup(base_stamina, LocalObjType),
    {LocalObjId} = bson:lookup('_id', LocalObj),
    update(LocalObjId, stamina, Stamina),
    [LocalObj];

create(ObjId, _Class, TypeName) ->
    {LocalObjType} = find_type(TypeName),
    insert(ObjId, LocalObjType).

update(Id, Attr, Val) ->
    mdb:update(<<"local_obj">>, Id, {Attr, Val}).

remove(Id) ->
    mdb:delete(<<"local_obj">>, Id).

get_by_pos(GlobalPos, LocalPos) ->
    MS = ets:fun2ms(fun(N = #local_obj{global_pos = GPos,
                                       pos = LPos}) when GPos =:= GlobalPos,
                                                         LPos =:= LocalPos -> N end),
    LocalObjs = db:select(local_obj, MS),
    LocalObjs.

get_unit_by_pos(GlobalPos, LocalPos) ->
    MS = ets:fun2ms(fun(N = #local_obj{global_pos = GPos,
                                       pos = LPos,
                                       class = Class}) when GPos =:= GlobalPos,
                                                            LPos =:= LocalPos,
                                                            Class =:= unit -> N end),
    LocalObjs = db:select(local_obj, MS),
    LocalObjs.

get_wall(GlobalPos, LocalPos) ->
    MS = ets:fun2ms(fun(N = #local_obj{global_pos = GPos,
                                       pos = LPos,
                                       subclass = SubClass}) when GPos =:= GlobalPos,
                                                                  LPos =:= LocalPos,
                                                                  SubClass =:= <<"wall">> -> N end),
    [Wall] = db:select(local_obj, MS),
    Wall.

is_nearby_hero(_Target = #local_obj{subclass = Subclass}, _HeroPlayer) when Subclass =:= <<"hero">> ->
    true;
is_nearby_hero(Target, HeroPlayer) ->
    MS = ets:fun2ms(fun(N = #local_obj{player = Player,
                                       subclass = Subclass}) when Player =:= HeroPlayer,
                                                                  Subclass =:= <<"hero">> -> N end),
    [Hero] = db:select(local_obj, MS),
    Distance = map:distance(Hero#local_obj.pos, Target#local_obj.pos),
    Distance =< ?LOS.

%%Internal function
%%

find(Id) ->
    Cursor = mongo:find(mdb:get_conn(), <<"local_obj">>, {'_id', Id}),
    LocalObj = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    LocalObj.

find_type(Name) ->
    mongo:find_one(mdb:get_conn(), <<"local_obj_type">>, {'name', Name}).

find_units(ObjId) ->
    lager:info("ObjId: ~p", [ObjId]), 
    Cursor = mongo:find(mdb:get_conn(), <<"local_obj">>, {'obj_id', ObjId, 'class', <<"unit">>}),
    Units = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    lager:info("find_units: ~p", [Units]),
    Units.

insert(ObjId, Type) ->
    {TypeName} = bson:lookup('name', Type),
    {BaseHp} = bson:lookup(base_hp, Type),
    {Class} = bson:lookup(class, Type),
    Unit = {obj_id, ObjId, hp, BaseHp, type_name, TypeName, class, Class},
    LocalObj = mongo:insert(mdb:get_conn(), <<"local_obj">>, [Unit]),
    LocalObj.

stats([]) ->
    false;

stats([LocalObj]) ->
    stats(LocalObj);

stats(LocalObj) ->
    {TypeName} = bson:lookup(type_name, LocalObj),
    {LocalObjType} = find_type(TypeName),
    bson:merge(LocalObj, LocalObjType).

info(LocalObjM) ->
    LocalObjStats = stats(LocalObjM),
    {Id} = bson:lookup('_id', LocalObjStats),
    Items = item:get_by_owner(Id),

    %Get state from local obj table
    [LocalObj] = db:read(local_obj, Id),

    LocalObjStats2 = bson:update(items, Items, LocalObjStats),
    LocalObjStats3 = bson:update(state, LocalObj#local_obj.state, LocalObjStats2),
    LocalObjStats3.

test() ->
    {Result, _} = timer:tc(local_obj, readtest, [100]),
    lager:info("~p", [Result]).

readtest(0) ->
    done;
readtest(N) ->
    Id = {<<84,219,228,71,15,76,42,231,31,114,99,13>>},
    lager:info("~p", [get_stats(Id)]),

    readtest(N - 1).
