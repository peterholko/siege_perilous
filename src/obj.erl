%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to obj data
-module(obj).

-include_lib("stdlib/include/ms_transform.hrl").

-include("schema.hrl").
-include("common.hrl").

-export([create/3, create/4, create/5, update_state/2, remove/1]).
-export([process_create/1, process_update_state/2, process_move/2, process_sleep/1, 
         process_deleting/1, process_obj_stats/0]).
-export([update_hp/2, update_stamina/2, update_thirst/2, update_hunger/2, update_focus/2, 
         update_dead/1, update_deleting/1]).
-export([set_thirst/2, set_hunger/2]).
-export([is_empty/1, is_empty/2, movement_cost/2]).
-export([get_by_pos/1, get_unit_by_pos/1, get_hero/1, get_assignable/1, get_wall/1]).
-export([is_hero_nearby/2, is_monolith_nearby/1, is_subclass/2, is_player/1, is_blocking/2]).
-export([has_vision/1]).
-export([trigger_effects/1, trigger_inspect/1]).
-export([item_transfer/2, has_space/2]).
-export([get/1, get_by_attr/1, get_by_attr/2, get_stats/1, get_info/1, get_info_other/1, get_capacity/1]).
-export([get_nearby_corpses/1, get_by_player/1, get_name_by_id/1, get_info_inventory/2,
         get_info_attrs/1, get_info_skills/1]).
-export([id/1, player/1, class/1, subclass/1, template/1, state/1, pos/1, 
         name/1, image/1, vision/1, modtick/1]).
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

    Images = obj_attr:value(Id, <<"images">>, []),
    HSL = obj_attr:value(Id, <<"hsl">>, []),

    %Set Unique Name or use unit template
    Name = case UniqueName of
               none -> obj_attr:value(Id, <<"name">>);
               _ -> UniqueName
           end,
    
    %Pick image
    NumImages = length(Images),
    
    Image = case NumImages > 0 of
                true ->
                    Index = util:rand(NumImages),
                    lists:nth(Index, Images);
                false ->
                    %Strip out spaces and lowercase
                    string:lowercase(re:replace(Template, <<" ">>, <<"">>, [{return, binary}]))                    
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
    case Obj#obj.state =/= State of
        true ->
            update_state(Obj, State);
        false ->
            nothing
    end.

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

process_sleep(ObjId) ->
    [Obj] = db:read(obj, ObjId),

    %TODO explore traits for faster sleep
    ModValue = -750,

    NewFocus = Obj#obj.focus + ModValue,
    NewObj = Obj#obj{focus = NewFocus},

    {FinalObj, MoreSleep} = case Obj#obj.focus > ?BASE_FOCUS_TIRED of
                 true -> 
                    {NewObj, more_sleep};
                 false ->
                     NewObj2 = NewObj#obj{state = none},
                     {NewObj2, rested}
             end,
  
    
    db:write(FinalObj),

    MoreSleep.

process_update_state(Obj, State) when is_record(Obj, obj) ->
    process_update_state(Obj, State, none);

process_update_state(ObjId, State) ->
    [Obj] = db:read(obj, ObjId),
    process_update_state(Obj, State, none).

process_update_state(Obj, State, StateData) when is_record(Obj, obj) ->
    NewObj = Obj#obj {state = State},

    save(NewObj, StateData),

    %Return new obj
    NewObj.

process_obj_stats() ->
    F = fun() ->
            mnesia:write_lock_table(obj),
            Villagers = mnesia:dirty_index_read(obj, ?VILLAGER, #obj.subclass),
            Heros = mnesia:dirty_index_read(obj, ?HERO, #obj.subclass),

            G = fun(Obj) ->
                    NewObj = process_obj_stat(Obj),
                    mnesia:dirty_write(NewObj)
                end,

            lists:foreach(G, Villagers ++ Heros) 
        end,

    mnesia:transaction(F).

process_obj_stat(Obj) ->
    case Obj#obj.hunger of
        Hunger when Hunger > (?TICKS_SEC * 30) -> 
            case effect:has_effect(Obj#obj.id, ?HUNGRY) of
                false -> effect:add(Obj#obj.id, ?HUNGRY);
                true -> none
            end;
        Hunger when Hunger > (?TICKS_MIN * 24) ->
            case effect:has_effect(Obj#obj.id, ?STARVATION) of
                false -> effect:add(Obj#obj.id, ?STARVATION);
                true -> none
            end;
        _ ->
            none
    end,

    case Obj#obj.thirst of
        Thirst when Thirst > (?TICKS_MIN * 4) ->
            case effect:has_effect(Obj#obj.id, ?THIRSTY) of
                false -> effect:add(Obj#obj.id, ?THIRSTY);
                true -> none
            end;
        Thirst when Thirst > (?TICKS_MIN * 12) ->
            case effect:has_effect(Obj#obj.id, ?DEHYDRATION) of
                false -> effect:add(Obj#obj.id, ?DEHYDRATION);
                true -> none
            end;
        _ ->
            none
    end,

    case Obj#obj.focus of
        Focus when Focus > ?BASE_FOCUS_TIRED ->
            case effect:has_effect(Obj#obj.id, ?TIRED) of
                false -> effect:add(Obj#obj.id, ?TIRED);
                true -> none
            end;
        Focus when Focus > ?BASE_FOCUS_EXHAUSTED ->
            case effect:has_effect(Obj#obj.id, ?EXHAUSTED) of
                false -> effect:add(Obj#obj.id, ?EXHAUSTED);
                true -> none
            end;
        _ -> 
            none
    end,

    NewHunger = case obj:state(Obj) =:= ?EATING of
                    true ->
                        Obj#obj.hunger;
                    false ->
                        Obj#obj.hunger + 1
                end,

    NewThirst = case obj:state(Obj) =:= ?DRINKING of
                    true ->
                        Obj#obj.thirst;
                    false ->
                        Obj#obj.thirst + 1
                end,

    NewFocus = case obj:state(Obj) =:= ?SLEEPING of
                   true ->
                       Obj#obj.focus;
                   false ->
                       Obj#obj.focus + 1
               end,

    NewObj = Obj#obj{hunger = NewHunger,
                     thirst = NewThirst,
                     focus = NewFocus},
    NewObj.

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

update_thirst(Id, ModValue) ->
    [Obj] = db:read(obj, Id),
    NewHunger = Obj#obj.thirst + ModValue,
    NewObj = Obj#obj{thirst = NewHunger},

    %TODO replace db:write with save
    db:write(NewObj).

set_thirst(Id, Value) ->
    [Obj] = db:read(obj, Id),
    NewObj = Obj#obj{thirst = Value},

    %TODO replace db:write with save
    db:write(NewObj).

update_hunger(Id, ModValue) ->
    [Obj] = db:read(obj, Id),
    NewHunger = Obj#obj.hunger + ModValue,
    NewObj = Obj#obj{hunger = NewHunger},

    %TODO replace db:write with save
    db:write(NewObj).

set_hunger(Id, Value) ->
    [Obj] = db:read(obj, Id),
    NewObj = Obj#obj{hunger = Value},

    %TODO replace db:write with save
    db:write(NewObj).

update_focus(Id, ModValue) ->
    [Obj] = db:read(obj, Id),
    NewFocus = Obj#obj.focus + ModValue,
    NewObj = Obj#obj{focus = NewFocus},

    %TODO replace db:write with save
    db:write(NewObj).




update_dead(Id) ->
    [Obj] = db:read(obj, Id),

    NewObj = case Obj#obj.class of
                unit ->
                     case Obj#obj.subclass of
                         ?HERO -> game:hero_dead(Obj#obj.player, Obj#obj.id); 
                         ?VILLAGER -> villager:remove(Obj#obj.id);
                         _ -> nothing
                     end, 
                    %TODO resolve string vs atom for class
                     Obj#obj {class = ?CORPSE, 
                              state = ?DEAD,
                              vision = 0};
                structure ->
                     %Remove structure reference from villagers
                     villager:remove_structure(Id),
 
                     Obj#obj {class = ruins,
                              state = dead,
                              vision = 0};
                _ ->
                     Obj#obj {state = ?DEAD}
             end,

    %Save object
    save(NewObj),

    %Return new obj
    NewObj.

update_deleting(Obj) ->
    subclass_delete(Obj),
    game:add_obj_delete(self(), Obj#obj.id, 1).

trigger_effects(#obj{state = State}) when (State =:= ?DELETING) -> nothing;
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

trigger_inspect(Obj) ->
    %Only trigger on structures and poi
    %TODO FIX the structure macros
    Result = (Obj#obj.class =:= structure) orelse (Obj#obj.class =:= poi),
    lager:info("Inspect Obj: ~p ~p", [Obj, Result]),
    
    case Result of
        true ->
            apply_trigger(Obj);
        false ->
            nothing
    end.

apply_trigger(#obj{player = Player, subclass = Subclass, 
                   state = State}) when (Subclass =:= ?MONOLITH) and
                                        (State =/= ?DISABLED) ->
    lager:info("Applying Holy Light"),    
    AllObjs = get_by_player(Player),

    F = fun(Obj) ->
            effect:add(Obj#obj.id, ?HOLY_LIGHT, none, ?TICKS_SEC * 5)
        end,

    lists:foreach(F, AllObjs);
apply_trigger(Obj) ->    
    obj_attr:set(Obj#obj.id, <<"inspected">>, true).

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
                                               (State =:= ?DISABLED)  ->
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

get_by_player(PlayerId) ->
    db:index_read(obj, PlayerId, #obj.player).

get_name_by_id(ObjId) ->
    Name = case db:read(obj, ObjId) of
               [Obj] -> Obj#obj.name;
               [] -> <<"none">>
           end,
    Name.

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
name(Obj = #obj{}) -> Obj#obj.name.
image(Obj = #obj{}) -> Obj#obj.image.
vision(Obj = #obj{}) -> Obj#obj.vision.
modtick(Obj = #obj{}) -> Obj#obj.modtick.

%Get vital stats
get_stats(Id) ->
    stats(Id).

%Get all stats, items, skills, effects for player owned unit
get_info(Id) ->
    info(Id).

%Get info for all other objects
get_info_other(Id) ->
    info_other(Id).

get_info_inventory(Id, Obj) ->
    info_inventory(Id, Obj).

get_info_attrs(Obj) ->
    info_attrs(Obj).

get_info_skills(Obj) ->
    info_skills(Obj).

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

get_nearby_corpses(SourceObj) ->
    Corpses = db:index_read(obj, ?CORPSE, #obj.subclass),

    F = fun(Corpse) ->
            Distance = map:distance(SourceObj#obj.pos, Corpse#obj.pos),
            Distance =< SourceObj#obj.vision    
        end,

    lists:filter(F, Corpses).

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
    combat:init_combos(Id);
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

subclass_delete(Id) when is_integer(Id) ->
    [Obj] = db:read(obj, Id),
    subclass_delete(Obj);
subclass_delete(Obj) ->

    case Obj#obj.subclass of
        ?VILLAGER ->
            villager:remove(Obj#obj.id);
        ?NPC -> 
            npc:remove(Obj#obj.id);
        ?HERO ->
            process_hero_delete(Obj),
            %TODO probably shouldn't go into the login module
            login:remove(Obj#obj.player);
        _ ->
            nothing
    end.

process_deleting(Obj) ->
    lager:info("processing_deleting obj: ~p", [Obj]),
    NewObj = Obj#obj {class = ?DELETING,
                      subclass = ?DELETING,
                      state = ?DELETING},
    save(NewObj).

remove(Id) ->
    lager:info("Removing obj ~p", [Id]),
    db:delete(obj, Id).

process_hero_delete(HeroObj) ->
    PlayerObjs = db:index_read(obj, HeroObj#obj.player, #obj.player),

    F = fun(Obj) ->
            case HeroObj#obj.id =/= Obj#obj.id of
                true ->
                    case Obj#obj.subclass =:= ?VILLAGER of
                        true ->
                            villager:set_behavior(Obj#obj.id, lost_villager);
                        false ->
                            nothing
                    end, 

                    NewObj = Obj#obj {player = ?THE_LOST},
                    save(NewObj);
                false ->
                    nothing
            end
        end,

    lists:foreach(F, PlayerObjs).
    
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
%% Internal functions
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

    %Get attrs and remove unnecessary ones
    AllAttrs = obj_attr:all_to_map(Id),
    Attrs0 = maps:remove(<<"name">>, AllAttrs),
    Attrs1 = maps:remove(<<"images">>, Attrs0),

    %State, items, skills, effects stats
    Info0 = maps:put(<<"id">>, Id, Attrs1),
    Info1 = maps:put(<<"name">>, Obj#obj.name, Info0),
    Info2 = maps:put(<<"image">>, Obj#obj.image, Info1),

    %TODO Decide on binary string vs atom, skills are bianry strings right now
    Info3 = maps:put(<<"state">>, atom_to_binary(Obj#obj.state, latin1), Info2), 
    Info4 = maps:put(<<"items">>, Items, Info3),
    Info5 = maps:put(<<"skills">>, Skills, Info4),
    Info6 = maps:put(<<"effects">>, Effects, Info5),

    %Check if any subclass specific info should be added
    AllSubClassInfo = info_subclass(Obj#obj.subclass, Obj, Info6),

    %Check if any states specific info should be added
    lager:info("Info obj state: ~p", [Obj#obj.state]),
    AllInfo = info_states(Obj#obj.state, Obj, AllSubClassInfo),

    AllInfo.

info_other(Id) ->
    [Obj] = db:read(obj, Id),
    
    Items = item:get_by_owner(Id),    
    Effects = effect:get_effects(Id),

    %Temp fix until string vs atom is resolved for class
    Class = case is_atom(Obj#obj.class) of
                true -> atom_to_binary(Obj#obj.class, latin1);
                false -> Obj#obj.class
            end,    

    Info0 = maps:put(<<"id">>, Id, #{}),
    Info1 = maps:put(<<"class">>, Class, Info0),
    Info2 = maps:put(<<"subclass">>, Obj#obj.subclass, Info1),
    Info3 = maps:put(<<"name">>, Obj#obj.name, Info2),
    Info4 = maps:put(<<"template">>, Obj#obj.template, Info3),
    Info5 = maps:put(<<"image">>, Obj#obj.image, Info4),
    Info6 = maps:put(<<"state">>, atom_to_binary(Obj#obj.state, latin1), Info5),
    Info7 = maps:put(<<"effects">>, Effects, Info6),

    % Add items if obj is dead to info
    case Obj#obj.state of
        dead -> maps:put(<<"items">>, Items, Info7);
        _ -> Info7
    end.

info_subclass(<<"villager">>, Obj, Info) ->
    Villager = villager:info(Obj#obj.id),
    
    TotalWeight = item:get_total_weight(Obj#obj.id),
    Capacity = obj:get_capacity(Obj#obj.id),
    Morale = Villager#villager.morale,
    Order = Villager#villager.order,
    ShelterId = Villager#villager.shelter,
    StructureId = Villager#villager.structure,

    ShelterName = get_name_by_id(ShelterId),
    StructureName = get_name_by_id(StructureId),

    Info0 = maps:put(<<"total_weight">>, TotalWeight, Info),
    Info1 = maps:put(<<"capacity">>, Capacity, Info0),
    Info2 = maps:put(<<"morale">>, Morale, Info1),
    Info3 = maps:put(<<"shelter">>, ShelterName, Info2),
    Info4 = maps:put(<<"structure">>, StructureName, Info3),
    Info5 = maps:put(<<"order">>, Order, Info4),
    Info6 = maps:put(<<"action">>, villager:action(Villager), Info5),
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

info_states(?PROGRESSING, #obj{id = Id}, Info) ->
    BuildTime = obj_attr:value(Id, <<"build_time">>, 0),
    EndTime = obj_attr:value(Id, <<"end_time">>, 0),
    CurrentTime = game:get_tick(),
    
    Progress = round((1 - ((EndTime - CurrentTime) / BuildTime)) * 100),

    Info0 = maps:put(<<"progress">>, Progress, Info),
    Info0;

info_states(?STALLED, #obj{id = Id}, Info) ->
    Progress = obj_attr:value(Id, <<"progress">>) * 100,
    Info0 = maps:put(<<"progress">>, Progress, Info),
    Info0;

info_states(State, _, Info) ->
    lager:info("Info States passthrough: ~p", [State]),
    Info.

info_inventory(PlayerId, Obj) ->
    Items = case PlayerId =:= Obj#obj.player of
                true ->
                    item:get_by_owner(Obj#obj.id);
                false ->
                    case Obj#obj.state =:= ?DEAD of
                        true ->
                            item:get_by_owner(Obj#obj.id);
                        false ->
                            []
                    end
            end,
    
    Info = #{<<"id">> => Obj#obj.id,
             <<"items">> => Items},
    Info.

info_attrs(Obj) ->
    Attrs = #{?STRENGTH => obj_attr:value(Obj#obj.id, ?STRENGTH, 0),
              ?TOUGHNESS => obj_attr:value(Obj#obj.id, ?TOUGHNESS, 0),
              ?ENDURANCE => obj_attr:value(Obj#obj.id, ?ENDURANCE, 0),
              ?DEXTERITY => obj_attr:value(Obj#obj.id, ?DEXTERITY, 0),
              ?INTELLECT => obj_attr:value(Obj#obj.id, ?INTELLECT, 0),
              ?FOCUS => obj_attr:value(Obj#obj.id, ?FOCUS, 0),
              ?SPIRIT => obj_attr:value(Obj#obj.id, ?SPIRIT, 0),
              ?CREATIVITY => obj_attr:value(Obj#obj.id, ?CREATIVITY, 0)},

    #{<<"id">> => Obj#obj.id,
      <<"attrs">> => Attrs}.

info_skills(Obj) ->
    Skills = skill:get_by_owner(Obj#obj.id),
    Skills,
    
    #{<<"id">> => Obj#obj.id,
      <<"skills">> => Skills}.

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

save(Obj) ->
    save(Obj, none).

save(Obj, StateData) when is_record(Obj, obj) ->
    F = fun() ->
            %Update state table
            State = #state {id = Obj#obj.id, 
                            state = Obj#obj.state, 
                            data = StateData,
                            modtick = counter:value(tick)},            
            mnesia:write(State),

            %Write obj
            mnesia:write(Obj)

        end,

    mnesia:transaction(F),

    %Trigger any new effects
    obj:trigger_effects(Obj).
