%% Author: Peter
%% Created: Feb, 2015
%% Description: Local map module
-module(local).

-include_lib("stdlib/include/ms_transform.hrl").

-include("common.hrl").
-include("schema.hrl").

-export([init_perception/1, enter_map/4, exit_map/1]).
-export([create/8, remove/1, move/2, set_wall_effect/1, is_behind_wall/2]).
-export([update_state/2, update_dead/1]).
-export([is_exit_valid/1, is_empty/1]).
-export([movement_cost/2]).

init_perception(PlayerId) ->
    PlayerUnits = db:index_read(obj, PlayerId, #obj.player),

    ExploredMap = map:get_explored(PlayerId, all),
    ObjData = util:unique_list(get_visible_objs(PlayerUnits, []),

    lager:info("LocalExploredMap: ~p", [ExploredMap]), 
    lager:info("LocalObjData: ~p", [ObjData]), 
    {ExploredMap, ObjData}.

enter(PlayerId, Pos) ->
    map:add_local_explored(PlayerId, GlobalPos, EnterPos),
 
    F = fun(Unit) ->
                lager:info("enter_map unit: ~p", [Unit]),
                {Id} = bson:lookup('_id', Unit), 
                {TypeName} = bson:lookup(type_name, Unit),
                {Subclass} = bson:lookup(subclass, Unit),
                {Vision} = bson:lookup(vision, Unit),
                enter_obj(GlobalPos, GlobalObjId, Id, EnterPos, PlayerId, unit, Subclass, TypeName, none, Vision)
        end,

    lists:foreach(F, Units).

exit_map(GlobalObjId) ->
    lager:info("Exit map: ~p", [GlobalObjId]),
    LocalObjs = db:index_read(local_obj, GlobalObjId, #local_obj.global_obj_id),

    %Remove any local objs owned by global obj id
    case LocalObjs of
        [] ->            
            false;
        LocalObjs ->
            remove_all_objs(LocalObjs),
            true
    end.

enter_obj(GlobalPos, GlobalObjId, Id, Pos, PlayerId, Class, Subclass, Name, State, Vision) ->
    lager:info("Enter obj ~p", [Pos]),

    LocalObj = #local_obj {id = Id,
                           global_obj_id = GlobalObjId,
                           global_pos = GlobalPos,
                           pos = Pos,
                           player = PlayerId,
                           class = Class,
                           subclass = Subclass,
                           name = Name,
                           state = State,
                           vision = Vision},

    db:write(LocalObj),
    game:trigger_local(GlobalPos).

create(GlobalPos, GlobalObjId, Pos, PlayerId, Class, Subclass, Name, State) ->
    lager:info("Creating ~p", [Name]),

    %Create mongo db local obj
    [LocalObjM] = local_obj:create(Class, Name),
    {Id} = bson:lookup('_id', LocalObjM),
    {Vision} = bson:lookup(vision, LocalObjM),

    %Create mnesia local obj
    LocalObj = #local_obj {id = Id,
                           global_obj_id = GlobalObjId,
                           global_pos = GlobalPos,
                           pos = Pos,
                           player = PlayerId,
                           class = Class,
                           subclass = Subclass,
                           name = Name,
                           state = State,
                           vision = Vision}, 
    db:write(LocalObj),
    game:trigger_local(GlobalPos),

    %Check subclass for any other post creation tasks
    create_subclass(Id, Subclass),

    %Return ID
    Id.
create_subclass(Id, <<"npc">>) ->
    NPC = #npc {id = Id},
    db:write(NPC);
create_subclass(_, _) ->
    nothing.

remove(LocalObj) ->
    db:delete(local_obj, LocalObj#local_obj.id),
    game:trigger_local(LocalObj#local_obj.global_pos).

move(Id, Pos) ->
    %TODO convert to transaction
    [LocalObj] = db:read(local_obj, Id),
    GlobalPos = LocalObj#local_obj.global_pos,

    NewLocalObj = LocalObj#local_obj {pos = Pos,
                                      state = none},
    db:write(NewLocalObj),

    %Update wall effect
    lager:debug("Updating wall effect"),
    IsBehindWall = is_behind_wall(GlobalPos, Pos),
    AddOrRemove = is_add_remove_wall(IsBehindWall),
    update_wall_effect(AddOrRemove, NewLocalObj),

    %Add explored if object is granted vision
    lager:debug("Adding explored tiles"),
    case LocalObj#local_obj.vision of
        true ->
            map:add_local_explored(LocalObj#local_obj.player, 
                                   LocalObj#local_obj.global_pos,
                                   Pos),
            game:trigger_explored(LocalObj#local_obj.player, LocalObj#local_obj.global_pos);
        false ->
            nothing
    end.

update_state(Id, State) ->
    lager:info("Update state: ~p ~p", [Id, State]),
    %TODO make transaction
    [LocalObj] = db:read(local_obj, Id),
    NewLocalObj = LocalObj#local_obj {state = State},
    db:write(NewLocalObj),

    %Trigger new perception
    game:trigger_local(LocalObj#local_obj.global_pos),

    NewLocalObj.

update_dead(Id) ->
    [LocalObj] = db:read(local_obj, Id),
    NewLocalObj = LocalObj#local_obj {class = corpse,
                                      state = dead,
                                      vision = false},
    db:write(NewLocalObj),

    %Trigger new perception
    game:trigger_local(LocalObj#local_obj.global_pos),

    NewLocalObj.

is_empty(LocalPos) ->
    LocalObjs = db:dirty_index_read(local_obj, LocalPos, #local_obj.pos),
    Units = filter_units(LocalObjs),
    Units =:= [].

is_behind_wall(GlobalPos, LocalPos) ->
    MS = ets:fun2ms(fun(N = #local_obj{global_pos = GPos, 
                                       pos = LPos, 
                                       class = structure,
                                       state = State,
                                       subclass = <<"wall">>}) when GPos =:= GlobalPos, 
                                                                    LPos =:= LocalPos,
                                                                    State =/= dead -> N end),
    LocalObjs = db:select(local_obj, MS),
    LocalObjs =/= [].

set_wall_effect(_ = #local_obj{id = Id,
                               subclass = Subclass,
                               state = State,
                               global_pos = GlobalPos,
                               pos = Pos}) when Subclass =:= <<"wall">> ->
    lager:debug("Set wall effect"),
    LocalObjs = local_obj:get_by_pos(GlobalPos, Pos),
    AddOrRemove = is_add_remove_wall(State),

    F = fun(LocalObj) ->
            update_wall_effect(AddOrRemove, LocalObj)
        end,

    lists:foreach(F, LocalObjs);
      
set_wall_effect(_) ->

    nothing.

is_add_remove_wall(true) -> add;
is_add_remove_wall(false) -> remove;
is_add_remove_wall(_State = none) -> add;
is_add_remove_wall(_State) -> remove.

update_wall_effect(add, #local_obj{id = Id,
                                   global_pos = GPos,
                                   pos = Pos,
                                   class = Class}) when Class =:= unit ->
    Wall = local_obj:get_wall(GPos, Pos),

    Effect = #effect {key = {Id, <<"wall">>},
                      type = <<"wall">>,
                      data = Wall#local_obj.id},
                            
    db:write(Effect);

update_wall_effect(remove, #local_obj{id = Id,
                                      class = Class}) when Class =:= unit ->
    db:delete(effect, {Id, <<"wall">>});

update_wall_effect(_, _LocalObj) ->
    lager:info("Not applying wall effect to non-unit").

movement_cost(_LocalObj, NextPos) ->
    %Check unit skills 
    map:movement_cost(NextPos) * 8.

%
% Internal functions
%

remove_obj(LocalObj) ->
    lager:info("Removing local obj: ~p", [LocalObj]),
    db:delete(local_obj, LocalObj#local_obj.id).

remove_all_objs([]) ->
    done;
remove_all_objs([LocalObj | Rest]) ->
    remove_obj(LocalObj),
    remove_all_objs(Rest).

get_visible_objs([], Objs, _GlobalPos) ->
    Objs;
get_visible_objs([Obj | Rest], Objs, GlobalPos) ->
    NearbyObjs = map:get_nearby_objs(Obj#local_obj.pos, {local_map, GlobalPos}, 2),
    NewObjs = Objs ++ NearbyObjs,
    get_visible_objs(Rest, NewObjs, GlobalPos).

get_direction(-1, 1, 0) -> se;
get_direction(0, 1, -1) -> s;
get_direction(1, 0, -1) -> sw;
get_direction(-1, 0, 1) -> ne;
get_direction(0, -1, 1) -> n;
get_direction(1, -1, 0) -> nw;
get_direction(_, _, _) -> nw.

enter_pos(nw) -> {0,0};
enter_pos(n) -> {8,0};
enter_pos(ne) -> {16,0};
enter_pos(sw) -> {0,12};
enter_pos(s) -> {8,12};
enter_pos(se) -> {16,12}. 

is_on_edge({X, _Y}) when X =:= 0 -> true;
is_on_edge({_X, Y}) when Y =:= 0 -> true;
is_on_edge({X, Y}) when X =:= 0, Y =:= 0 -> true;
is_on_edge({_X, _Y}) -> false.

filter_units(LocalObjs) ->
    F = fun(LocalObj) -> LocalObj#local_obj.class =:= unit end,
    lists:filter(F, LocalObjs).

has_vision(<<"hero">>) -> true;
has_vision(<<"npc">>) -> true;
has_vision(_) -> false.
