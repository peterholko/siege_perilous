%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to obj mongodb data
-module(obj).

-include_lib("stdlib/include/ms_transform.hrl").

-include("schema.hrl").
-include("common.hrl").

-export([init_perception/1]).
-export([create/6, remove/1, move/2]).
-export([update_state/2, update_hp/2, update_stamina/2, update_dead/1]).
-export([add_effect/3, remove_effect/2, has_effect/2, trigger_effects/2]).
-export([is_empty/1, movement_cost/2]).
-export([get_by_pos/1, get_unit_by_pos/1]).
-export([is_hero_nearby/2, is_monolith_nearby/1, is_not_npc/1]).
-export([item_transfer/2]).
-export([get/1, get_stats/1, get_info/1, get_info_other/1]).

init_perception(PlayerId) ->
    PlayerUnits = db:index_read(obj, PlayerId, #obj.player),

    ExploredMap = map:get_explored(PlayerId, all),
    ObjData = util:unique_list(get_visible_objs(PlayerUnits, [])),

    lager:info("ExploredMap: ~p", [ExploredMap]), 
    lager:info("ObjData: ~p", [ObjData]), 
    {ExploredMap, ObjData}.

create(Pos, PlayerId, Class, Subclass, Name, State) ->
    Id = util:get_id(),
    
    %Create obj attr entries from obj def entries
    create_obj_attr(Id, Name),

    %Get base attributes
    Vision = obj_attr:value(Id, <<"base_vision">>, 0),

    %Create mnesia obj
    Obj = #obj {id = Id,
                pos = Pos,
                player = PlayerId,
                class = Class,
                subclass = Subclass,
                name = Name,
                state = State,
                vision = Vision,
                modtick = counter:value(tick)}, 
    db:write(Obj),

    lager:debug("Triggering perception"),
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
    case Obj#obj.vision > -1 of
        true ->
            map:add_explored(Obj#obj.player, Pos, Obj#obj.vision),
            game:trigger_explored(Obj#obj.player);
        false ->
            nothing
    end,

    %Check if player triggered encounter
    case is_not_npc(Obj) of
        true -> encounter:check(Pos);
        false -> nothing
    end.

update_state(Id, State) ->
    lager:debug("Update state: ~p ~p", [Id, State]),
    [Obj] = db:read(obj, Id),
    NewObj = Obj#obj {state = State},
    db:write(NewObj),

    %Trigger new perception
    game:trigger_perception(),

    NewObj.

update_hp(Id, Value) ->
    Hp = obj_attr:value(Id, <<"hp">>),
    BaseHp = obj_attr:value(Id, <<"base_hp">>),
    lager:info("Hp: ~p, BaseHp: ~p Value: ~p", [Hp, BaseHp, Value]),
    NewHp = set_attr(Hp, BaseHp, Value),
    lager:info("NewHp: ~p", [NewHp]),

    case NewHp =< 0 of
        true -> update_dead(Id);
        false -> nothing
    end,

    obj_attr:set(Id, <<"hp">>, NewHp).

update_stamina(Id, Value) ->
    Stamina = obj_attr:value(Id, <<"stamina">>),
    BaseStamina = obj_attr:value(Id, <<"base_stamina">>),
    NewStamina = set_attr(Stamina, BaseStamina, Value),

    obj_attr:set(Id, <<"stamina">>, NewStamina).

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
                      id = Id,
                      type = EffectType,
                      data = EffectData},
    db:write(Effect).

remove_effect(Id, EffectType) ->
    db:delete(effect, {Id, EffectType}).

has_effect(Id, EffectType) ->
    Result = db:dirty_read(effect, {Id, EffectType}),
    length(Result) > 0.

get_effects(Id) ->
    Effects = db:index_read(effect, Id, #effect.id),
    
    F = fun(Effect, Acc) ->
            EffectMap = #{<<"name">> => Effect#effect.type},
            [EffectMap | Acc]
        end,

    lists:foldl(F, [], Effects).

trigger_effects(add, #obj{id = WallId, pos = Pos, subclass = Subclass}) when Subclass =:= ?WALL ->
    Objs = get_by_pos(Pos),

    F = fun(Obj) ->
            case Obj#obj.class =:= unit of
                true -> add_effect(Obj#obj.id, ?FORTIFIED, WallId);
                false -> nothing
            end
        end,
    lists:foreach(F, Objs);

trigger_effects(remove, #obj{pos = Pos, subclass = Subclass}) when Subclass =:= ?WALL ->
    Objs = get_by_pos(Pos),

    F = fun(Obj) ->
            case Obj#obj.class =:= unit of 
                true -> remove_effect(Obj#obj.id, ?FORTIFIED);
                false -> nothing
            end
        end,
    lists:foreach(F, Objs);
trigger_effects(_, _) -> nothing.

is_empty(Pos) ->
    Objs = db:dirty_index_read(obj, Pos, #obj.pos),
    Units = filter_units(Objs),
    Units =:= [].

item_transfer(#obj {id = Id, 
                    subclass = Subclass, 
                    state = State}, Item) when (Subclass =:= ?MONOLITH) and 
                                               (State =:= disabled)  ->
    Name = maps:get(<<"name">>, Item),

    case Name of
        <<"Mana">> -> update_state(Id, none);
        _ -> nothing
    end;
item_transfer(#obj {id = Id, class = Class}, Item) when Class =:= unit ->
    Subclass = maps:get(<<"subclass">>, Item, none),
    
    case Subclass of
        ?FOOD -> remove_effect(Id, <<"starving">>);
        _ -> nothing
     end;
item_transfer(_Obj, _Item) -> nothing.

%Get obj or return false
get(Id) ->
    case db:read(obj, Id) of
        [Obj] -> Obj;
        _ -> false
    end.

%Get vital stats
get_stats(Id) ->
    stats(Id).

%Get all stats, items, skills, effects for player owned unit
get_info(Id) ->
    info(Id).

%Get info for all other objects
get_info_other(Id) ->
    info_other(Id).

%%
%% Internal Functions
%%

get_by_pos(Pos) ->
    db:index_read(obj, Pos, #obj.pos).

get_unit_by_pos(QueryPos) ->
    MS = ets:fun2ms(fun(N = #obj{pos = Pos,
                                 class = Class}) when Pos =:= QueryPos,
                                                      Class =:= unit -> N end),
    Objs = db:select(obj, MS),
    Objs.

get_wall(QueryPos) ->
    MS = ets:fun2ms(fun(N = #obj{pos = Pos,
                                 subclass = SubClass}) when Pos =:= QueryPos,
                                                            SubClass =:= ?WALL -> N end),
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
                                 subclass = ?WALL}) when Pos =:= QueryPos,
                                                         State =/= dead -> N end),
    Objs = db:select(obj, MS),
    Objs =/= [].

is_monolith_nearby(QueryPos) ->
    Monoliths = db:index_read(obj, ?MONOLITH, #obj.subclass),

    F = fun(Monolith) ->            
            Radius = get_monolith_radius(Monolith#obj.name),
            Distance = map:distance(Monolith#obj.pos, QueryPos),
            (Distance =< Radius) and (Monolith#obj.state =/= disabled)
        end,

    lists:any(F, Monoliths).

is_not_npc(#obj {player = Player}) when Player >= 1000 -> true;
is_not_npc(_) -> false.

apply_wall(false, #obj {id = Id}) ->
    case has_effect(Id, ?FORTIFIED) of
        true -> remove_effect(Id, ?FORTIFIED);
        false -> nothing
    end;
apply_wall(true, #obj {id = Id, pos = Pos}) ->
    Wall = get_wall(Pos),
    add_effect(Id, ?FORTIFIED, Wall#obj.id).

apply_sanctuary(false, #obj {id = Id}) ->
    case has_effect(Id, ?SANCTUARY) of
        true -> remove_effect(Id, ?SANCTUARY);
        false -> nothing
    end;
apply_sanctuary(true, #obj{id = Id}) ->
    add_effect(Id, ?SANCTUARY, none).

process_subclass(Id, <<"npc">>) ->
    %Add npc entry
    NPC = #npc {id = Id},
    db:write(NPC);
process_subclass(_, _) ->
    nothing.

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

stats(Id) ->
    Stats = maps:new(),
   
    Hp = obj_attr:value(Id, <<"hp">>),
    BaseHp = obj_attr:value(Id, <<"base_hp">>),
    Stamina = obj_attr:value(Id, <<"stamina">>),
    BaseStamina = obj_attr:value(Id, <<"base_stamina">>),

    Effects = get_effects(Id),

    Stats0 = maps:put(<<"_id">>, Id, Stats),
    Stats1 = maps:put(<<"hp">>, Hp, Stats0),
    Stats2 = maps:put(<<"base_hp">>, BaseHp, Stats1),
    Stats3 = maps:put(<<"stamina">>, Stamina, Stats2),
    Stats4 = maps:put(<<"base_stamina">>, BaseStamina, Stats3),
    Stats5 = maps:put(<<"effects">>, Effects, Stats4),
    Stats5.

info(Id) ->
    %Get Mnesia obj
    [Obj] = db:read(obj, Id),

    %Get items & skills & effects
    Items = item:get_by_owner(Id),
    Skills = skill:get_by_owner(Id),
    Effects = get_effects(Id),

    %Get attrs
    Attrs = obj_attr:all_to_map(Id),

    %State, items, skills, effects stats
    Info1 = maps:put(<<"state">>, atom_to_binary(Obj#obj.state, latin1), Attrs), 
    Info2 = maps:put(<<"items">>, Items, Info1),
    Info3 = maps:put(<<"skills">>, Skills, Info2),
    Info4 = maps:put(<<"effects">>, Effects, Info3),
    Info4.

info_other(Id) ->
    %Get Mnesia obj
    [Obj] = db:read(obj, Id),
    
    Items = item:get_by_owner(Id),    
    Effects = get_effects(Id),

    Info1 = maps:put(<<"_id">>, Id, #{}),
    Info2 = maps:put(<<"name">>, Obj#obj.name, Info1),
    Info3 = maps:put(<<"state">>, atom_to_binary(Obj#obj.state, latin1), Info2),
    Info4 = maps:put(<<"effects">>, Effects, Info3),

    % Add items if obj is dead to info
    case Obj#obj.state of
        dead -> maps:put(<<"items">>, Items, Info4);
        _ -> Info4
    end.

create_obj_attr(Id, Name) ->
    AllObjDef = obj_def:all(Name),
    
    F = fun(ObjDef) -> 
            {Name, Attr} = ObjDef#obj_def.key,
            AttrKey = {Id, Attr},
            ObjAttr = #obj_attr {key = AttrKey, 
                                 value = ObjDef#obj_def.value},
            db:dirty_write(ObjAttr)
        end,

    lists:foreach(F, AllObjDef).

set_attr(Current, _Base, Change) when (Current + Change) =< 0 -> 0;
set_attr(Current, Base, Change) when (Current + Change) > Base -> Base;
set_attr(Current, _Base, Change) -> Current + Change.


