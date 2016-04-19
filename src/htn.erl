%% Author: Peter
%% Created: Sept 10, 2015
%% Description: Hierarchical Task Network for NPC AI
-module(htn).
-export([new/1,
         add_select_one/4,
         add_select_all/4,
         add_primitive/5,
         add_goto/5]).
-export([load/0, plan/2]).

-include("schema.hrl").

load() ->
    wander(),
    guard(),
    villager().

wander() ->
    new(wander),
    add_select_one(attack_enemy, wander, [target_visible], []),
    add_select_all(do_attack, attack_enemy, [], []),
    add_primitive(move_to_target, do_attack, [], [], move_to_target),
    add_primitive(melee_attack, do_attack, [], [], melee_attack),
    add_select_all(do_wander, wander, [], []),
    add_primitive(move_random_pos, do_wander, [], [], move_random_pos).

guard() ->
    new(guard),
    add_select_one(attack_enemy, guard, [target_visible], []),
    add_select_all(return_to_guard, attack_enemy, [max_guard_dist], []),
    add_primitive(move_guard_pos1, return_to_guard, [], [], move_guard_pos),
    add_select_all(do_attack, attack_enemy, [], []),
    add_primitive(move_to_target, do_attack, [], [], move_to_target),
    add_primitive(melee_attack, do_attack, [], [], melee_attack),
    add_select_all(do_guard, guard, [], []),
    add_primitive(move_guard_pos2, do_guard, [], [], move_guard_pos).

villager() ->
    new(villager),
    add_select_one(nearby_enemy, villager, [enemy_visible], []),
        add_select_all(do_flee, nearby_enemy, [], []),
            add_primitive(move_to_dwelling, do_flee, [], [], move_to_dwelling),
    add_select_one(process_task, villager, [morale_normal], []),
        add_select_one(do_follow, process_task, [task_follow], []),
            add_primitive(follow_hero, do_follow, [], [], follow_hero),
    add_select_one(process_forage, villager, [morale_low], []),
        add_primitive(do_forage, process_forage, [], [], do_forage),
    add_select_one(process_abandon, villager, [morale_very_low], []),
        add_primitive(do_abandon, process_abandon, [], [], do_abandon).

plan(PlanName, NPC) ->
    [Parent] = db:dirty_read(htn, {PlanName, PlanName}),
    Children = db:dirty_index_read(htn, {PlanName, PlanName}, #htn.parent),
    SortedChildren = lists:keysort(#htn.index, Children),

    erase(plan),
    
    process_child(SortedChildren, Parent#htn.type, NPC),

    Plan = get(plan),
    Plan.

process_child([], primitive_task, _NPC) ->
    done;
process_child(Children, select_one, NPC) ->
    NextChild = process_child_selectone(Children, {false, none}, NPC),
    add_to_plan(NextChild),

    NextChildren = db:dirty_index_read(htn, NextChild#htn.label, #htn.parent),
    SortedChildren = lists:keysort(#htn.index, NextChildren),
    process_child(SortedChildren, NextChild#htn.type, NPC);

process_child(Children, select_all, NPC) ->
    Result = process_child_selectall(Children, true, NPC),
    
    case Result of
        true ->
            add_to_plan(Children);
        false ->
            nothing
    end.

process_child_selectone(_Children, {true, Child}, _NPC) ->
    Child;
process_child_selectone([Child | Rest], _Result, NPC) ->
    Conditions = Child#htn.conditions,
    lager:debug("Conditions: ~p", [Conditions]),
    NewResult = eval(Conditions, NPC),
    process_child_selectone(Rest, {NewResult, Child}, NPC).

process_child_selectall([], Result, _NPC) ->
    Result;
process_child_selectall([Child | Rest], _Result, NPC) ->
    Conditions = Child#htn.conditions,
    NewResult = eval(Conditions, NPC),

    process_child_selectall(Rest, NewResult, NPC).

eval(Conditions, NPC) ->
    eval(Conditions, true, NPC).
eval(_Conditions, false, _NPC) ->
    false;
eval([], Result, _NPC) ->
    Result;
eval([Condition | Rest], Result, NPC) ->
    lager:debug("Condition: ~p", [Condition]),
    NewResult = erlang:apply(npc, Condition, [NPC]),
    eval(Rest, Result and NewResult, NPC).

add_to_plan(Children) when is_list(Children) ->
    F = fun(Child) ->
            add_to_plan(Child)
        end,

    lists:foreach(F, Children);
add_to_plan(#htn{task = Task, type = Type}) when Type =:= primitive ->
    Plan = get_plan(get(plan)),    
    put(plan, Plan ++ [Task]);
add_to_plan(_) ->
    nothing.

get_plan(Plan) when is_list(Plan) ->
    Plan;
get_plan(_) ->
    [].

new(Label) ->
    put(plan_name, Label),
    Htn = #htn{label = {Label, Label}, index = 0, type = select_one},
    db:dirty_write(Htn).

add_select_one(Label, Parent, Conditions, Effects) ->
    write(Label, Parent, Conditions, Effects, select_one, none).
add_select_all(Label, Parent, Conditions, Effects) ->
    write(Label, Parent, Conditions, Effects, select_all, none).
add_primitive(Label, Parent, Conditions, Effects, Task) ->
    write(Label, Parent, Conditions, Effects, primitive, Task).
add_goto(Label, Parent, Conditions, Effects, GotoTask) ->
    write(Label, Parent, Conditions, Effects, goto, GotoTask).

write(Label, Parent, Conditions, Effects, Type, Task) ->
    PlanName = get(plan_name),
    Children = db:dirty_index_read(htn, {PlanName, Parent}, #htn.parent),

    Htn = #htn{label = {PlanName, Label}, 
               index = length(Children),
               parent = {PlanName, Parent},
               conditions = Conditions,
               effects = Effects,
               type = Type,
               task = Task},

    db:dirty_write(Htn).
