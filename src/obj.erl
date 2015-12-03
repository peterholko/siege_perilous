%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to obj mongodb data
-module(obj).

-include_lib("stdlib/include/ms_transform.hrl").

-include("schema.hrl").
-include("common.hrl").

-export([init_perception/1]).
-export([create/6, remove/1, move/2]).
-export([set_wall_effect/1]).
-export([update_state/2, update_dead/1]).
-export([add_effect/3, remove_effect/2, has_effect/2]).
-export([is_empty/1, movement_cost/2]).
-export([get_by_pos/1, get_unit_by_pos/1]).
-export([is_hero_nearby/2, is_monolith_nearby/1]).
-export([item_transfer/2]).

%% MongoDB functions
-export([get/1, get_info/1, get_type/1]).
-export([update/3]).

init_perception(PlayerId) ->
    PlayerUnits = db:index_read(obj, PlayerId, #obj.player),

    ExploredMap = map:get_explored(PlayerId, all),
    ObjData = util:unique_list(get_visible_objs(PlayerUnits, [])),

    lager:info("ExploredMap: ~p", [ExploredMap]), 
    lager:info("ObjData: ~p", [ObjData]), 
    {ExploredMap, ObjData}.

create(Pos, PlayerId, Class, Subclass, Name, State) ->
    %Create mongodb obj
    [ObjM] = create(Class, Name),
    Id = maps:get(<<"_id">>, ObjM),
    Vision = maps:get(<<"vision">>, ObjM, 0),

    %Create mnesia obj
    Obj = #obj {id = Id,
                pos = Pos,
                player = PlayerId,
                class = Class,
                subclass = Subclass,
                name = Name,
                state = State,
                vision = Vision}, 
    db:write(Obj),

    lager:info("Triggering perception"),
    %Trigger perception to be recalculated
    game:trigger_perception(),

    %Check subclass for any other post creation tasks
    process_subclass(Id, Subclass),

    %Return ID
    Id.

move(Id, Pos) ->
    [Obj] = db:read(obj, Id),

    NewObj = Obj#obj {pos = Pos,
                      state = none},
    db:write(NewObj),

    %Update wall effect
    IsBehindWall = is_behind_wall(Pos),
    apply_wall(IsBehindWall, NewObj),

    %Update sanctuary effect
    IsNearbyMonolith = is_monolith_nearby(Pos),
    apply_sanctuary(IsNearbyMonolith, NewObj),

    %Add explored if object is granted vision
    case Obj#obj.vision > 0 of
        true ->
            map:add_explored(Obj#obj.player, Pos),
            game:trigger_explored(Obj#obj.player);
        false ->
            nothing
    end.

update_state(Id, State) ->
    lager:info("Update state: ~p ~p", [Id, State]),
    %TODO make transaction
    [Obj] = db:read(obj, Id),
    NewObj = Obj#obj {state = State},
    db:write(NewObj),

    %Trigger new perception
    game:trigger_perception(),

    NewObj.

update_dead(Id) ->
    [Obj] = db:read(obj, Id),
    NewObj = Obj#obj {class = corpse,
                      state = dead,
                      vision = 0},
    db:write(NewObj),

    %Trigger new perception
    game:trigger_perception(),

    NewObj.

add_effect(Id, EffectType, EffectData) ->
    Effect = #effect {key = {Id, EffectType},
                      type = EffectType,
                      data = EffectData},
    db:write(Effect).
   
remove_effect(Id, EffectType) ->
    db:delete(effect, {Id, EffectType}).

has_effect(Id, EffectType) ->
    Result = db:dirty_read(effect, {Id, EffectType}),
    length(Result) > 0.

is_empty(Pos) ->
    Objs = db:dirty_index_read(obj, Pos, #obj.pos),
    Units = filter_units(Objs),
    Units =:= [].

set_wall_effect(_ = #obj{id = _Id,
                         subclass = Subclass,
                         state = State,
                         pos = Pos}) when Subclass =:= <<"wall">> ->
    lager:debug("Set wall effect"),
    Objs = get_by_pos(Pos),
    AddOrRemove = is_add_remove_wall(State),

    F = fun(Obj) ->
            update_wall_effect(AddOrRemove, Obj)
        end,

    lists:foreach(F, Objs);
      
set_wall_effect(_) ->
    nothing.

item_transfer(#obj {id = Id, 
                    subclass = Subclass, 
                    state = State}, Item) when (Subclass =:= <<"Monolith">>) and 
                                               (State =:= disabled)  ->
    Name = maps:get(<<"name">>, Item),

    case Name of
        <<"Mana">> -> update_state(Id, none);
        _ -> nothing
    end.

%%
%% MongoDB Functions
%%

get(Id) ->
    find_one(<<"_id">>, Id).

get_info(Id) ->
    info(Id).

get_type(TypeName) ->
    find_one_type(<<"name">>, TypeName).

update(Id, Attr, Val) ->
    mdb:update(<<"obj">>, Id, {Attr, Val}).

%%
%% Internal Functions
%%

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

is_hero_nearby(_Target = #obj{subclass = Subclass}, _HeroPlayer) when Subclass =:= <<"hero">> ->
    true;
is_hero_nearby(Target, HeroPlayer) ->
    MS = ets:fun2ms(fun(N = #obj{player = Player,
                                 subclass = Subclass}) when Player =:= HeroPlayer,
                                                            Subclass =:= <<"hero">> -> N end),
    [Hero] = db:select(obj, MS),
    Distance = map:distance(Hero#obj.pos, Target#obj.pos),
    Distance =< Hero#obj.vision.

is_behind_wall(QueryPos) ->
    MS = ets:fun2ms(fun(N = #obj{pos = Pos, 
                                 class = structure,
                                 state = State,
                                 subclass = <<"wall">>}) when Pos =:= QueryPos,
                                                              State =/= dead -> N end),
    Objs = db:select(obj, MS),
    Objs =/= [].

is_monolith_nearby(QueryPos) ->
    Monoliths = db:index_read(obj, <<"monolith">>, #obj.subclass),

    F = fun(Monolith) ->            
            Radius = get_monolith_radius(Monolith#obj.name),
            Distance = map:distance(Monolith#obj.pos, QueryPos),
            (Distance =< Radius) and (Monolith#obj.state =/= disabled)
        end,

    lists:any(F, Monoliths).


apply_wall(false, #obj {id = Id}) ->
    case has_effect(Id, <<"wall">>) of
        true -> remove_effect(Id, <<"wall">>);
        false -> nothing
    end;
apply_wall(true, #obj {id = Id}) ->
    add_effect(Id, <<"wall">>, none).

apply_sanctuary(false, #obj {id = Id}) ->
    case has_effect(Id, <<"sanctuary">>) of
        true -> remove_effect(Id, <<"sanctuary">>);
        false -> nothing
    end;
apply_sanctuary(true, #obj{id = Id}) ->
    add_effect(Id, <<"sanctuary">>, none).

process_subclass(Id, <<"npc">>) ->
    NPC = #npc {id = Id},
    db:write(NPC);
process_subclass(_, _) ->
    nothing.

is_add_remove_wall(true) -> add;
is_add_remove_wall(false) -> remove;
is_add_remove_wall(_State = none) -> add;
is_add_remove_wall(_State) -> remove.

update_wall_effect(add, #obj{id = Id, pos = Pos, class = Class}) when Class =:= unit ->
    Wall = get_wall(Pos),
    add_effect(Id, <<"wall">>, Wall#obj.id);
update_wall_effect(remove, #obj{id = Id, class = Class}) when Class =:= unit ->
    remove_effect(Id, <<"wall">>);
update_wall_effect(_, _Obj) ->
    lager:info("Not applying wall effect to non-unit").

movement_cost(_Obj, NextPos) ->
    %Check unit skills 
    lager:info("NextPos: ~p", [NextPos]),
    map:movement_cost(NextPos) * 8.

remove(Id) ->
    ok = db:delete(obj, Id),
    mdb:delete(<<"obj">>, Id),
    game:trigger_perception().

get_visible_objs([], Objs) ->
    Objs;
get_visible_objs([Obj | Rest], Objs) ->
    NearbyObjs = map:get_nearby_objs(Obj#obj.pos, 2),
    NewObjs = Objs ++ NearbyObjs,
    get_visible_objs(Rest, NewObjs).

filter_units(Objs) ->
    F = fun(Obj) -> Obj#obj.class =:= unit end,
    lists:filter(F, Objs).

%get_direction(-1, 1, 0) -> se;
%get_direction(0, 1, -1) -> s;
%get_direction(1, 0, -1) -> sw;
%get_direction(-1, 0, 1) -> ne;
%get_direction(0, -1, 1) -> n;
%get_direction(1, -1, 0) -> nw;
%get_direction(_, _, _) -> nw.

get_monolith_radius(<<"Monolith">>) -> 1;
get_monolith_radius(<<"Greater Monolith">>) -> 2.

%%
%% Internal MongoDB functions
%%
create(structure, TypeName) ->
    lager:info("Creating structure ~p", [TypeName]),
    ObjType = find_one_type(<<"name">>, TypeName),
    UpdatedObjType = maps:update(<<"hp">>, 1, ObjType),
    insert(UpdatedObjType);

create(Class, TypeName) ->
    lager:info("Creating obj (~p / ~p)", [Class, TypeName]),
    ObjType = find_one_type(<<"name">>, TypeName),
    insert(ObjType).

insert(Type) ->
    NewType = maps:remove(<<"_id">>, Type),
    Obj = mongo:insert(mdb:get_conn(), <<"obj">>, [NewType]),
    Obj.

info(Id) ->
    %Get Mongo obj
    ObjM = find_one(<<"_id">>, Id),

    %Get Mnesia obj
    [Obj] = db:read(obj, Id),

    %Get items & skills
    Items = item:get_by_owner(Id),
    Skills = skill:get_by_owner(Id),

    %Build stats
    Stats1 = maps:put(<<"state">>, atom_to_binary(Obj#obj.state, latin1), ObjM), 
    Stats2 = maps:put(<<"items">>, Items, Stats1),
    Stats3 = maps:put(<<"skills">>, Skills, Stats2),
    Stats3.


find_one(Key, Value) ->
    mdb:find_one(<<"obj">>, Key, Value).
find(Key, Value) ->
    mdb:find(<<"obj">>, Key, Value).
%find(Tuple) ->
%    mdb:find(<<"obj">>, Tuple).
find_type(Key, Value) ->
    mdb:find(<<"obj_type">>, Key, Value).
find_one_type(Key, Value) ->
    mdb:find_one(<<"obj_type">>, Key, Value).
