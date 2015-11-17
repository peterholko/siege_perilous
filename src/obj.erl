%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to local obj mongodb data
-module(obj).

-include_lib("stdlib/include/ms_transform.hrl").

-include("schema.hrl").
-include("common.hrl").

-export([get/1, get_info/1, get_type/1, get_stats/1]).
-export([create/2, update/3, remove/1]).
-export([find_type/1, is_nearby_hero/2, readtest/1]).
-export([get_by_pos/1, get_unit_by_pos/1, get_wall/1]).

get(Id) ->
    Obj = find(Id),
    Obj.

get_info(Id) ->
    ObjInfo = case find(Id) of
                [Obj] ->
                    %TODO compare requester id to unit player id
                    info(Obj);
                _ ->
                    none
               end,
    ObjInfo.

get_type(TypeName) ->
    {ObjType} = find_type(TypeName),
    ObjType.

get_stats(Id) ->
    Obj = find(Id),
    stats(Obj).

create(structure, TypeName) ->
    {_ObjType} = find_type(TypeName),
    ObjType = bson:update(hp, 1, _ObjType),

    insert(ObjType); %Returns [Obj]

create(unit, TypeName) ->
    {ObjType} = find_type(TypeName),
    insert(ObjType); %Returns [Obj]

create(_Class, TypeName) ->
    {ObjType} = find_type(TypeName),
    insert(ObjType).

update(Id, Attr, Val) ->
    mdb:update(<<"obj">>, Id, {Attr, Val}).

remove(Id) ->
    mdb:delete(<<"obj">>, Id).

get_by_pos(QueryPos) ->
    MS = ets:fun2ms(fun(N = #obj{pos = Pos}) when Pos =:= QueryPos -> N end),
    Objs = db:select(obj, MS),
    Objs.

get_unit_by_pos(QueryPos) ->
    MS = ets:fun2ms(fun(N = #obj{pos = Pos,
                                 class = Class}) when Pos =:= QueryPos,
                                                      Class =:= unit -> N end),
    Objs = db:select(obj, MS),
    Objs.

get_wall(QueryPos) ->
    MS = ets:fun2ms(fun(N = #obj{pos = Pos,
                                 subclass = SubClass}) when Pos =:= QueryPos,
                                                            SubClass =:= <<"wall">> -> N end),
    [Wall] = db:select(obj, MS),
    Wall.

is_nearby_hero(_Target = #obj{subclass = Subclass}, _HeroPlayer) when Subclass =:= <<"hero">> ->
    true;
is_nearby_hero(Target, HeroPlayer) ->
    MS = ets:fun2ms(fun(N = #obj{player = Player,
                                 subclass = Subclass}) when Player =:= HeroPlayer,
                                                            Subclass =:= <<"hero">> -> N end),
    [Hero] = db:select(obj, MS),
    Distance = map:distance(Hero#obj.pos, Target#obj.pos),
    Distance =< ?LOS.

%%Internal function
%%

find(Id) ->
    Cursor = mongo:find(mdb:get_conn(), <<"obj">>, {'_id', Id}),
    Obj = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Obj.

find_type(Name) ->
    mongo:find_one(mdb:get_conn(), <<"obj_type">>, {name, Name}).

insert(Type) ->
    NewType = bson:exclude(['_id'], Type),
    Obj = mongo:insert(mdb:get_conn(), <<"obj">>, [NewType]),
    Obj.

stats([]) ->
    false;

stats([Obj]) ->
    stats(Obj);

stats(Obj) ->
    {TypeName} = bson:lookup(type_name, Obj),
    {ObjType} = find_type(TypeName),
    bson:merge(Obj, ObjType).

info(ObjM) ->
    ObjStats = stats(ObjM),
    {Id} = bson:lookup('_id', ObjStats),

    %Get state from local obj table
    [Obj] = db:read(obj, Id),
    Stats1 = bson:update(state, Obj#obj.state, ObjStats),

    %Get items & skills
    Items = item:get_by_owner(Id),
    Skills = skill:get_by_owner(Id),

    Stats2 = bson:update(items, Items, Stats1),
    Stats3 = bson:update(skills, Skills, Stats2),
    Stats3.

readtest(0) ->
    done;
readtest(N) ->
    Id = {<<84,219,228,71,15,76,42,231,31,114,99,13>>},
    lager:info("~p", [get_stats(Id)]),

    readtest(N - 1).
