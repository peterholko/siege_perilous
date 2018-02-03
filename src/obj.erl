%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to obj data
-module(obj).

-include_lib("stdlib/include/ms_transform.hrl").

-include("schema.hrl").
-include("common.hrl").

-export([create/3, create/4, create/5, update_state/2]).
-export([remove/1]).
-export([process_create/1, process_update_state/2, process_move/2]).
-export([update_hp/2, update_stamina/2, update_dead/1]).
-export([is_empty/1, is_empty/2, movement_cost/2]).
-export([get_by_pos/1, get_unit_by_pos/1, get_hero/1, get_assignable/1, get_wall/1]).
-export([is_hero_nearby/2, is_monolith_nearby/1, is_subclass/2, is_player/1, is_blocking/2]).
-export([has_vision/1]).
-export([trigger_effects/1]).
-export([item_transfer/2, has_space/2]).
-export([get/1, get_by_attr/1, get_by_attr/2, get_stats/1, get_info/1, get_info_other/1, get_capacity/1]).
-export([id/1, player/1, class/1, subclass/1, template/1, state/1, pos/1]).
-export([rec_to_map/1]).

create(Pos, PlayerId, Template) ->
    create(Pos, PlayerId, Template, none, none).

create(Pos, PlayerId, Template, State) ->
    create(Pos, PlayerId, Template, none, State).

create(Pos, PlayerId, Template, UniqueName, State) ->
    Id = util:get_id(),
    lager:info("Creating object ~p", [Template]),

    %Create obj attr entries from obj def entries
    create_obj_attr(Id, Template),

    %Get base attributes
    BaseHp = obj_attr:value(Id, <<"base_hp">>, 0),
    BaseStamina = obj_attr:value(Id, <<"base_stamina">>, 0),

    %TODO reconsider if class should not be binary
    Class = binary_to_atom(obj_attr:value(Id, <<"class">>, none), latin1),
    Subclass = obj_attr:value(Id, <<"subclass">>, none),

    Image = obj_attr:value(Id, <<"image">>, Template),
    HSL = obj_attr:value(Id, <<"hsl">>, []),

    %Set Unique Name or use unit template
    Name = case UniqueName of
               none -> obj_attr:value(Id, <<"name">>);
               _ -> UniqueName
           end,

    

    %Add attributes from base
    obj_attr:set(Id, <<"hp">>, BaseHp),
    obj_attr:set(Id, <<"stamina">>, BaseStamina),

    %Create obj
    Obj = #obj {id = Id,
                pos = Pos,
                player = PlayerId,
                name = Name,
                template = Template,
                class = Class,
                subclass = Subclass,
                state = State,
                image = Image,
                hsl = HSL, 
                modtick = counter:value(tick)},

    %Save obj
    save(Obj),

    %Add obj create event
    game:add_obj_create(self(), Id, 1),

    %Return Obj ID
    Id.

process_create(ObjId) ->
    [Obj] = db:read(obj, ObjId),

    %Set vision to complete the obj setup
    Vision = obj_attr:value(ObjId, <<"base_vision">>, 0),

    NewObj = Obj#obj{vision = Vision},

    %Save obj
    save(NewObj),

    case Vision > 0 of
        true ->
            %Create init perception 
            lager:info("Creating perception..."),
            perception:create(NewObj),

            map:add_explored(NewObj#obj.player, NewObj#obj.pos, NewObj#obj.vision),
            game:trigger_explored(NewObj#obj.player);
        false ->
            nothing
    end,

    %Check subclass for any other post creation tasks
    process_subclass(NewObj),

    %Return NewObj
    NewObj.

update_state(Obj, State) when is_record(Obj, obj) ->
    game:add_obj_update(self(), Obj#obj.id, ?STATE, State);
update_state(ObjId, State) ->
    [Obj] = db:read(obj, ObjId),
    update_state(Obj, State).

process_move(Obj, Pos) when is_record(Obj, obj) ->
    %Check if move can be completed
    case is_empty(Obj, Pos) of
        true -> do_move(Obj, Pos);
        false -> process_update_state(Obj, none)
    end;

process_move(Id, Pos) ->
    [Obj] = db:read(obj, Id),
    process_move(Obj, Pos).

do_move(Obj, Pos) ->
    NewObj = Obj#obj {pos = Pos, 
                      state = ?NONE},

    %Update wall effect
    IsBehindWall = is_behind_wall(Pos),
    apply_wall(IsBehindWall, NewObj),

    %Update sanctuary effect
    IsNearbyMonolith = is_monolith_nearby(Pos),
    apply_sanctuary(IsNearbyMonolith, NewObj),

    %Add explored if object is granted vision
    case NewObj#obj.vision > 0 of
        true ->
            map:add_explored(NewObj#obj.player, Pos, NewObj#obj.vision),
            game:trigger_explored(NewObj#obj.player);
        false ->
            nothing
    end,

    %Check if player triggered encounter
    case is_player(NewObj) of
        true -> encounter:check(Pos);
        false -> nothing
    end,

    %Save new obj
    save(NewObj),

    %Return new obj
    NewObj.

process_update_state(Obj, State) when is_record(Obj, obj) ->
    process_update_state(Obj, State, none);

process_update_state(ObjId, State) ->
    [Obj] = db:read(obj, ObjId),
    process_update_state(Obj, State, none).

process_update_state(Obj, State, StateData) when is_record(Obj, obj) ->
    NewObj = Obj#obj {state = State},

    save(NewObj, StateData).

update_hp(Id, Value) ->
    Hp = obj_attr:value(Id, <<"hp">>),
    BaseHp = obj_attr:value(Id, <<"base_hp">>),
    NewHp = set_attr(Hp, BaseHp, Value),

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

    NewObj = case Obj#obj.class of
                unit ->
                     case Obj#obj.subclass of
                         ?HERO -> game:hero_dead(Obj#obj.player, Obj#obj.id); 
                         ?VILLAGER -> villager:remove(Obj#obj.id);
                         _ -> nothing
                     end, 

                     Obj#obj {class = corpse,
                              state = dead,
                              vision = 0};
                structure ->
                     %Remove structure reference from villagers
                     villager:remove_structure(Id),
 
                     Obj#obj {class = ruins,
                              state = dead,
                              vision = 0};
                _ ->
                     Obj#obj {state = dead}
             end,

    %Save object
    save(NewObj),

    NewObj.

trigger_effects(#obj{id = WallId, pos = Pos, subclass = Subclass, state = State}) when (Subclass =:= ?WALL) and
                                                                                       (State =/= dead) ->
    Objs = get_by_pos(Pos),

    F = fun(Obj) ->
            case Obj#obj.class =:= unit of
                true -> effect:add(Obj#obj.id, ?FORTIFIED, WallId);
                false -> nothing
            end
        end,
    lists:foreach(F, Objs);

trigger_effects(#obj{pos = Pos, subclass = Subclass, state = State}) when (Subclass =:= ?WALL) and
                                                                          (State =:= dead) ->
    Objs = get_by_pos(Pos),

    F = fun(Obj) ->
            case Obj#obj.class =:= unit of 
                true -> effect:remove(Obj#obj.id, ?FORTIFIED);
                false -> nothing
            end
        end,
    lists:foreach(F, Objs);

trigger_effects(#obj{pos = Pos, name = Name, subclass = Subclass, state = State}) when (Subclass =:= ?MONOLITH) ->
    Radius = get_monolith_radius(Name),
    AllAdjPos = map:range(Pos, Radius),

    F = fun(AdjPos) ->
            %Update Sanctuary on objs
            Objs = get_by_pos(AdjPos),
            G = fun(Obj) -> 
                    case State of
                        none -> effect:add(Obj#obj.id, ?SANCTUARY, none);
                        disabled -> effect:remove(Obj#obj.id, ?SANCTUARY)
                    end
                end,

            lists:foreach(G, Objs),

            %Update Sanctuary on tiles
            case State of
                none -> effect:add({tile, AdjPos}, ?SANCTUARY, none);
                disabled -> effect:remove({tile, AdjPos}, ?SANCTUARY)
            end
        end,

    lists:foreach(F, AllAdjPos);

trigger_effects(_) -> nothing.

is_empty(Pos) ->
    Objs = db:dirty_index_read(obj, Pos, #obj.pos),
    Units = filter_units(Objs),
    Units =:= [].

is_empty(SourceObj, Pos) when is_record(SourceObj, obj) ->
    Objs = db:dirty_index_read(obj, Pos, #obj.pos),

    F = fun(Obj) ->
            case is_player(Obj) of
                true ->
                    (SourceObj#obj.player =/= Obj#obj.player) and (Obj#obj.class =:= unit);
                false ->
                    Obj#obj.class =:= unit
            end
        end,

    lists:filter(F, Objs) =:= [];
is_empty(SourceId, Pos) ->
    [Obj] = db:read(obj, SourceId),
    is_empty(Obj, Pos).

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
        ?FOOD -> effect:remove(Id, <<"starving">>);
        _ -> nothing
     end;
item_transfer(_Obj, _Item) -> nothing.

%Get obj or return false
get(Id) ->
    case db:read(obj, Id) of
        [Obj] -> Obj;
        _ -> invalid
    end.

get_by_attr(AttrList) ->
    Objs = ets:tab2list(obj),
    F = fun(Obj) ->
            process_attrlist(Obj, AttrList)
        end,

    lists:filter(F, Objs).

get_by_attr(Player, AttrList) ->
    Objs = db:index_read(obj, Player, #obj.player),

    F = fun(Obj) ->
            process_attrlist(Obj, AttrList)
        end,

    lists:filter(F, Objs).

process_attrlist(Obj, AttrList) ->
    F = fun({class, Value}, Acc) when is_list(Value) -> lists:member(class(Obj), Value) and Acc;
           ({class, Value}, Acc) -> (class(Obj) =:= Value) and Acc;
           ({subclass, Value}, Acc) when is_list(Value) -> lists:member(subclass(Obj), Value) and Acc;
           ({subclass, Value}, Acc) -> (subclass(Obj) =:= Value) and Acc;
           ({state, Value}, Acc) -> (state(Obj) =:= Value) and Acc
        end,

    lists:foldl(F, true, AttrList).

id(Obj = #obj{}) -> Obj#obj.id.
player(Obj = #obj{}) -> Obj#obj.player.
template(Obj = #obj{}) -> Obj#obj.template.
class(Obj = #obj{}) -> Obj#obj.class.
subclass(Obj = #obj{}) -> Obj#obj.subclass.
state(Obj = #obj{}) -> Obj#obj.state.
pos(Obj = #obj{}) -> Obj#obj.pos.

%Get vital stats
get_stats(Id) ->
    stats(Id).

%Get all stats, items, skills, effects for player owned unit
get_info(Id) ->
    info(Id).

%Get info for all other objects
get_info_other(Id) ->
    info_other(Id).

get_by_pos(Pos) ->
    db:index_read(obj, Pos, #obj.pos).

get_unit_by_pos(QueryPos) ->
    MS = ets:fun2ms(fun(N = #obj{pos = Pos,
                                 class = Class}) when Pos =:= QueryPos,
                                                      Class =:= unit -> N end),
    Objs = db:select(obj, MS),
    Objs.

get_hero(HeroPlayer) ->
    MS = ets:fun2ms(fun(N = #obj{player = Player,
                                 subclass = Subclass}) when Player =:= HeroPlayer,
                                                            Subclass =:= <<"hero">> -> N end),
    [Hero] = db:select(obj, MS),
    Hero.

get_wall(QueryPos) ->
    MS = ets:fun2ms(fun(N = #obj{pos = Pos,
                                 subclass = Subclass}) when Pos =:= QueryPos,
                                                            Subclass =:= ?WALL -> N end),
    [Wall] = db:select(obj, MS),
    Wall.

get_assignable(QueryPos) ->
    MS = ets:fun2ms(fun(N = #obj{pos = Pos,
                                 subclass = Subclass}) when Pos =:= QueryPos,
                                                      Subclass =:= ?CRAFT;
                                                      Pos =:= QueryPos, 
                                                      Subclass =:= ?HARVESTER -> N end),

    case db:select(obj, MS) of
        [] -> [];
        [Structure] -> Structure
    end.

get_capacity(ObjId) ->
    obj_attr:value(ObjId, <<"capacity">>, 0).

is_hero_nearby(_Target = #obj{subclass = Subclass}, _HeroPlayer) when Subclass =:= <<"hero">> ->
    true;
is_hero_nearby(Target, HeroPlayerId) when is_record(Target, obj) ->
    [Player] = db:read(player, HeroPlayerId),
    [Hero] = db:read(obj, Player#player.hero),
    Distance = map:distance(Hero#obj.pos, Target#obj.pos),
    Distance =< Hero#obj.vision;
is_hero_nearby(_Target, _HeroPlayer) -> false.

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
            Radius = get_monolith_radius(Monolith#obj.template),
            Distance = map:distance(Monolith#obj.pos, QueryPos),
            (Distance =< Radius) and (Monolith#obj.state =/= disabled)
        end,

    lists:any(F, Monoliths).

is_subclass(IsSubclass, #obj{subclass = Subclass}) when Subclass =:= IsSubclass -> true;
is_subclass(_, _) -> false.

is_player(#obj{player = Player}) when Player > ?NPC_ID -> true;
is_player(_) -> false.

is_blocking(SourcePlayer, #obj{player = Player, state = State}) ->
    case SourcePlayer =/= Player of
       true -> is_blocking_state(State);
       false -> false
    end.

has_space(ObjId, NewItemWeight) ->
    Capacity = get_capacity(ObjId),
    TotalWeight = item:get_total_weight(ObjId),
    lager:info("TotalWeight: ~p NewItemWeight: ~p Capacity: ~p", [TotalWeight, NewItemWeight, Capacity]),
    (TotalWeight + NewItemWeight) =< Capacity.

apply_wall(false, #obj{id = Id}) ->
    case effect:has_effect(Id, ?FORTIFIED) of
        true -> effect:remove(Id, ?FORTIFIED);
        false -> nothing
    end;
apply_wall(true, #obj{id = Id, pos = Pos}) ->
    Wall = get_wall(Pos),
    effect:add(Id, ?FORTIFIED, Wall#obj.id).

apply_sanctuary(false, #obj{id = Id}) ->
    case effect:has_effect(Id, ?SANCTUARY) of
        true -> effect:remove(Id, ?SANCTUARY);
        false -> nothing
    end;
apply_sanctuary(true, #obj{id = Id}) ->
    effect:add(Id, ?SANCTUARY, none).

process_subclass(#obj{id = Id, player = Player, subclass = Subclass}) when Subclass =:= ?HERO ->
    combat:init_combos(Id),
    Hero = #hero{player = Player, obj = Id},
    db:write(Hero);
process_subclass(#obj{id = Id, subclass = Subclass}) when Subclass =:= ?NPC ->
    combat:init_combos(Id),
    NPC = #npc{id = Id},
    db:write(NPC);
process_subclass(#obj{id = Id, player = Player, subclass = Subclass}) when Subclass =:= ?VILLAGER ->
    Villager = #villager{id = Id, player = Player},
    db:write(Villager);
process_subclass(#obj{pos = Pos, name = Name, subclass = Subclass}) when Subclass =:= ?MONOLITH ->
    Radius = get_monolith_radius(Name),
    NearbyPosList = map:range(Pos, Radius), 

    F = fun(NearbyPos) ->
            effect:add({tile, NearbyPos}, ?SANCTUARY, none)
        end,

    lists:foreach(F, NearbyPosList);
process_subclass(_) ->
    nothing.

movement_cost(_Obj, NextPos) ->
    %Check unit skills 
    lager:debug("NextPos: ~p", [NextPos]),
    map:movement_cost(NextPos) * 8.

remove(Id) ->
    db:delete(villager, Id),
    db:delete(npc, Id).

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

    Effects = effect:get_effects(Id),

    Stats0 = maps:put(<<"id">>, Id, Stats),
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
    Effects = effect:get_effects(Id),

    %Get attrs
    Attrs = obj_attr:all_to_map(Id),

    %State, items, skills, effects stats
    Info0 = maps:put(<<"id">>, Id, Attrs),
    Info1 = maps:put(<<"state">>, atom_to_binary(Obj#obj.state, latin1), Info0), 
    Info2 = maps:put(<<"items">>, Items, Info1),
    Info3 = maps:put(<<"skills">>, Skills, Info2),
    Info4 = maps:put(<<"effects">>, Effects, Info3),

    %Check if any subclass specific info should be added
    AllInfo = info_subclass(Obj#obj.subclass, Obj, Info4),
    AllInfo.

info_other(Id) ->
    [Obj] = db:read(obj, Id),
    
    Items = item:get_by_owner(Id),    
    Effects = effect:get_effects(Id),

    Info0 = maps:put(<<"id">>, Id, #{}),
    Info1 = maps:put(<<"class">>, atom_to_binary(Obj#obj.class, latin1), Info0),
    Info2 = maps:put(<<"subclass">>, Obj#obj.subclass, Info1),
    Info3 = maps:put(<<"name">>, Obj#obj.name, Info2),
    Info4 = maps:put(<<"template">>, Obj#obj.template, Info3),
    Info5 = maps:put(<<"state">>, atom_to_binary(Obj#obj.state, latin1), Info4),
    Info6 = maps:put(<<"effects">>, Effects, Info5),

    % Add items if obj is dead to info
    case Obj#obj.state of
        dead -> maps:put(<<"items">>, Items, Info6);
        _ -> Info6
    end.

info_subclass(<<"villager">>, Obj, Info) ->
    [Villager] = db:read(villager, Obj#obj.id),
    
    TotalWeight = item:get_total_weight(Obj#obj.id),
    Capacity = obj:get_capacity(Obj#obj.id),
    Morale = Villager#villager.morale,
    Order = Villager#villager.order,
    DwellingId = Villager#villager.shelter,
    StructureId = Villager#villager.structure,

    CurrentTask = case Villager#villager.task_index > 0 of
                      true ->
                          atom_to_binary(lists:nth(Villager#villager.task_index, Villager#villager.plan), latin1);
                      false ->
                          <<"idle">>
                  end,

    DwellingName = case db:read(obj, DwellingId) of
                       [Dwelling] -> Dwelling#obj.name;
                       [] -> <<"none">>
                   end,

    StructureName = case db:read(obj, StructureId) of
                        [Structure] -> Structure#obj.name;
                        [] -> <<"none">>
                    end,

    Info0 = maps:put(<<"total_weight">>, TotalWeight, Info),
    Info1 = maps:put(<<"capacity">>, Capacity, Info0),
    Info2 = maps:put(<<"morale">>, Morale, Info1),
    Info3 = maps:put(<<"shelter">>, DwellingName, Info2),
    Info4 = maps:put(<<"structure">>, StructureName, Info3),
    Info5 = maps:put(<<"order">>, Order, Info4),
    Info6 = maps:put(<<"action">>, CurrentTask, Info5),
    Info6;
info_subclass(<<"hero">>, Obj, Info) -> 
    TotalWeight = item:get_total_weight(Obj#obj.id),
    Capacity = obj:get_capacity(Obj#obj.id),

    Info0 = maps:put(<<"total_weight">>, TotalWeight, Info),
    Info1 = maps:put(<<"capacity">>, Capacity, Info0),
    Info1;
info_subclass(<<"resource">>, Obj, Info) -> 
    TotalWeight = item:get_total_weight(Obj#obj.id),
    Capacity = obj:get_capacity(Obj#obj.id),
    
    Info0 = maps:put(<<"total_weight">>, TotalWeight, Info),
    Info1 = maps:put(<<"capacity">>, Capacity, Info0),
    Info1;
info_subclass(<<"storage">>, Obj, Info) -> 
    TotalWeight = item:get_total_weight(Obj#obj.id),
    Capacity = obj:get_capacity(Obj#obj.id),
    
    Info0 = maps:put(<<"total_weight">>, TotalWeight, Info),
    Info1 = maps:put(<<"capacity">>, Capacity, Info0),
    Info1;
info_subclass(_, _Obj, Info) -> Info.

create_obj_attr(Id, Name) ->
    AllObjTemplate = obj_template:all(Name),
    
    F = fun(ObjTemplate) -> 
            {Name, Attr} = ObjTemplate#obj_template.key,
            case ObjTemplate#obj_template.key of
                {Name, Attr} ->
                    AttrKey = {Id, Attr},
                    ObjAttr = #obj_attr {key = AttrKey, 
                                         value = ObjTemplate#obj_template.value},
                    db:dirty_write(ObjAttr)
            end
        end,

    lists:foreach(F, AllObjTemplate).

set_attr(Current, _Base, Change) when (Current + Change) =< 0 -> 0;
set_attr(Current, Base, Change) when (Current + Change) > Base -> Base;
set_attr(Current, _Base, Change) -> Current + Change.

is_blocking_state(?DEAD) -> false;
is_blocking_state(?FOUNDED) -> false;
is_blocking_state(?PROGRESSING) -> false;
is_blocking_state(_) -> true.

has_vision(Obj) -> Obj#obj.vision > 0.

rec_to_map(Obj) ->
    {X, Y} = Obj#obj.pos,
    #{<<"id">> => Obj#obj.id, 
      <<"player">> => Obj#obj.player, 
      <<"x">> => X,
      <<"y">> => Y,
      <<"name">> => Obj#obj.name,
      <<"template">> => Obj#obj.template, %TODO convert to binary everywhere
      <<"class">> => Obj#obj.class,
      <<"subclass">> => Obj#obj.subclass,
      <<"vision">> => Obj#obj.vision,
      <<"state">> => Obj#obj.state,
      <<"image">> => Obj#obj.image,
      <<"hsl">> => Obj#obj.hsl}.
       
save(NewObj) ->
    save(NewObj, none).

save(NewObj, StateData) when is_record(NewObj, obj) ->
    F = fun() ->
            %Update state table
            NewState = #state {id = NewObj#obj.id, 
                               state = NewObj#obj.state, 
                               data = StateData,
                               modtick = counter:value(tick)},            
            mnesia:write(NewState),

            %Write obj
            mnesia:write(NewObj),

            %Return new obj
            NewObj   
        end,

    {atomic, NewObj} = mnesia:transaction(F),

    %Trigger any new effects
    obj:trigger_effects(NewObj),

    %Return new obj
    NewObj.
