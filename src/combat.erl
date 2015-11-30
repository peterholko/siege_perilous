%%% -------------------------------------------------------------------
%%% Author  : Peter Holko
%%% Description : Combat manager
%%%
%%% Created : Dec 15, 2014
%%% -------------------------------------------------------------------
-module(combat).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([do_action/1, attack/3, guard/1, dodge/1]).
-export([has_stamina/2, stamina_cost/1, add_stamina/2, sub_stamina/2, num_ticks/1]).

%-ifdef(TEST).
%-compile(export_all).
%-endif.

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, combat}, combat, [], []).

do_action(Action) ->
    gen_server:cast({global, combat}, {do_action, Action}).

attack(AttackType, SourceId, TargetId) ->
    gen_server:cast({global, combat}, {attack, AttackType, SourceId, TargetId}).

guard(SourceId) ->
    gen_server:cast({global, combat}, {guard, SourceId}).

dodge(SourceId) ->
    gen_server:cast({global, combat}, {dodge, SourceId}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast({do_action, Action}, Data) ->
    process_action(Action),
    {noreply, Data};

handle_cast({attack, AttackType, SourceId, TargetId}, Data) ->
    lager:info("Attack ~p", [AttackType]), 
    [SourceObj] = db:read(obj, SourceId),
    [TargetObj] = db:read(obj, TargetId),

    Result = is_state_not(dead, SourceObj#obj.state) andalso
             is_state_not(dead, TargetObj#obj.state),

    set_attack(Result, AttackType, SourceObj, TargetObj),

    {noreply, Data};

handle_cast({guard, SourceId}, Data) ->
    lager:info("Guard ~p", [SourceId]),
    set_guard(SourceId),
    {noreply, Data};

handle_cast({dodge, SourceId}, Data) ->
    lager:info("dodge ~p", [SourceId]),
    set_dodge(SourceId),
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Event}, 
                              {from, From}
                             ]),
    {noreply, Data}.

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

terminate(_Reason, _) ->
    ok.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

set_attack(false, _, _, _) ->
    lager:info("set_attack_unit failed");
set_attack(true, AttackType, SourceObj, TargetObj) ->
    set_combat_state(SourceObj),
    set_combat_state(TargetObj),

    Action = #action {source_id = SourceObj#obj.id,
                      type = attack,
                      data = {AttackType, TargetObj#obj.id}},
    db:write(Action).

set_guard(SourceId) ->
    Action = #action {source_id = SourceId,
                      type = guard,
                      data = none},
    db:write(Action).

set_dodge(SourceId) ->
    [SourceObj] = db:read(obj, SourceId),

    Action = #action {source_id = SourceId,
                      type = dodge,
                      data = none},
    db:write(Action),

    %Add Dodging effect
    obj:add_effect(SourceObj#obj.id, <<"dodging">>, none).

process_action(Action) ->
    case Action#action.type of
        attack ->
            process_attack(Action);
        guard ->
            process_guard(Action);
        dodge ->
            process_dodge(Action);
        _ ->
            lager:info("Unknown action type: ~p", [Action#action.type]) 
    end.

process_attack(Action) ->
    lager:info("Process attack"),
    SourceId = Action#action.source_id,
    {AttackType, TargetId} = Action#action.data,

    SourceObj = get_obj(db:read(obj, SourceId)), 
    TargetObj = get_obj(db:read(obj, TargetId)),
    
    Result = is_valid_obj(SourceObj) andalso
             is_valid_obj(TargetObj) andalso
             is_visible(SourceObj, TargetObj) andalso
             is_adjacent(SourceObj, TargetObj) andalso
             is_target_alive(TargetObj) andalso
             is_targetable(TargetObj) andalso
             (not is_attack_dodged(TargetObj)),
    
    process_dmg(Result, AttackType, SourceObj, TargetObj).

process_guard(Action) ->
    lager:info("Process guard"),
    SourceId = Action#action.source_id,
    add_stamina(SourceId, 25).    

process_dodge(Action) ->
    lager:info("Process dodge"),
    SourceId = Action#action.source_id,

    obj:remove_effect(SourceId, <<"dodging">>).

get_obj([]) ->
    false;
get_obj([Obj]) ->
    Obj.

is_valid_obj(false) ->
    lager:info("Invalid obj"),
    false;
is_valid_obj(_Obj) ->
    true.

is_visible(SourceObj, TargetObj) ->
    perception:is_visible(SourceObj#obj.id, TargetObj#obj.id).

is_adjacent(SourceObj, TargetObj) ->
    {SX, SY} = SourceObj#obj.pos,
    TargetPos = TargetObj#obj.pos,
    Neighbours = map:neighbours(SX, SY), 
    case lists:member(TargetPos, Neighbours) of
        true ->
            true;
        false ->
            lager:info("Not adjacent"),
            false
    end.

is_targetable(#obj{id = Id}) ->
    HasWall = obj:has_effect(Id, <<"wall">>),
    IsTargetable = not HasWall,
    lager:info("is_targetable: ~p", [IsTargetable]),
    IsTargetable.

is_target_alive(#obj {state = State}) when State =:= dead -> 
    false;
is_target_alive(_) -> 
    true.

is_attack_dodged(#obj {id = Id}) ->
    IsDodging = obj:has_effect(Id, <<"dodging">>),
    Result = case IsDodging of
                 true -> 
                     rand:uniform() =< 0.5;
                 false ->
                     false
             end,
    lager:info("IsDodge: ~p", [Result]),
    Result.
                               
broadcast_dmg(SourceId, TargetId, Dmg, State) ->
    %Convert id here as message is being built
    Message = #{<<"packet">> => <<"dmg">>,
                <<"sourceid">> => util:bin_to_hex(SourceId),
                <<"targetid">> => util:bin_to_hex(TargetId),
                <<"dmg">> => Dmg,
                <<"state">> => State},

    [SourceObj] = db:read(obj, SourceId),
    [TargetObj] = db:read(obj, TargetId),

    SourcePos = SourceObj#obj.pos,
    TargetPos = TargetObj#obj.pos,

    perception:broadcast(SourcePos, TargetPos, Message).

broadcast_combo(SourceId, TargetId, Combo) ->
    Message = #{<<"packet">> => <<"combo">>,
                <<"sourceid">> => util:bin_to_hex(SourceId),
                <<"targetid">> => util:bin_to_hex(TargetId),
                <<"combo">> => Combo},

    [SourceObj] = db:read(obj, SourceId),
    [TargetObj] = db:read(obj, TargetId),

    SourcePos = SourceObj#obj.pos,
    TargetPos = TargetObj#obj.pos,

    perception:broadcast(SourcePos, TargetPos, Message).

process_dmg(false, _, AtkObj, _) ->
    db:delete(action, AtkObj#obj.id),
    lager:info("Invalid attack");      
process_dmg(true, AttackType, AtkObj, DefObj) ->
    AtkId = AtkObj#obj.id,
    DefId = DefObj#obj.id,

    AtkUnit = obj:get(AtkId),
    DefUnit = obj:get(DefId),

    AtkItems = item:get_equiped(AtkId),
    DefItems = item:get_equiped(DefId),

    AtkWeapons = item:get_equiped_weapon(AtkId),

    AtkStamina = maps:get(<<"stamina">>, AtkUnit),
    BaseDmg = maps:get(<<"base_dmg">>, AtkUnit),
    DmgRange = maps:get(<<"dmg_range">>, AtkUnit),
    BaseDef = maps:get(<<"base_def">>, DefUnit),
    DefHp = maps:get(<<"hp">>, DefUnit),

    %Check for combos
    {ComboName, ComboDmg} = check_combos(AttackType, AtkId),

    %Add item stats
    TotalDmg = (BaseDmg + get_items_value(<<"damage">>, AtkItems)) * ComboDmg,
    TotalArmor = BaseDef + get_items_value(<<"armor">>, DefItems),

    %Random roll and armor reduction
    RandomDmg = rand:uniform(DmgRange) + TotalDmg,
    DmgRoll = RandomDmg + TotalDmg,
    ArmorReduction = TotalArmor / (TotalArmor + 50),

    %Apply attack type modifier
    Dmg = round(DmgRoll * (1 - ArmorReduction)) * attack_type_mod(AttackType),
    NewHp = DefHp - Dmg,

    %Check for skill increases
    skill_gain_combo(AtkId, ComboName),
    skill_gain_atk(AtkId, AtkWeapons, RandomDmg, DmgRange, Dmg),

    %Update stamina
    NewStamina = AtkStamina - attack_type_cost(AttackType),
    obj:update(AtkObj#obj.id, <<"stamina">>, NewStamina),

    %Check if unit is alive
    UnitState = is_unit_dead(NewHp),

    %Broadcast damage
    lager:debug("Broadcasting dmg: ~p newHp: ~p", [Dmg, NewHp]),
    broadcast_dmg(AtkId, DefId, Dmg, UnitState),

    case ComboName of
        none -> nothing;
        _ -> broadcast_combo(AtkId, DefId, ComboName)
    end,

    %Check if unit is dead 
    case UnitState of
        <<"alive">> ->
            obj:update(DefId, <<"hp">>, NewHp);
        <<"dead">> ->
            AtkXp = maps:get(<<"xp">>, AtkUnit),
            DefKillXp = maps:get(<<"kill_xp">>, DefUnit),
            obj:update(AtkId, <<"xp">>, AtkXp + DefKillXp),
            process_unit_dead(DefId)
    end.


is_unit_dead(Hp) when Hp =< 0 ->
    <<"dead">>;
is_unit_dead(_Hp) ->
    <<"alive">>.

process_unit_dead(DefId) ->
    lager:info("Unit ~p died.", [DefId]),

    %Remove action associated with dead unit
    db:delete(action, DefId),

    lager:info("Updating unit state"),
    NewObj = obj:update_dead(DefId),

    %Remove potential wall effect
    obj:set_wall_effect(NewObj).

set_combat_state(#obj{state = State}) when State =:= combat ->
    nothing;
set_combat_state(#obj{id = Id}) ->
    obj:update_state(Id, combat).

%is_state(ExpectedState, State) when ExpectedState =:= State -> true;
%is_state(_ExpectdState, _State) -> false.

is_state_not(NotExpectedState, State) when NotExpectedState =:= State -> false;
is_state_not(_NotExpectedState, _State) -> true.

attack_type_mod(weak) -> 0.5;
attack_type_mod(basic) -> 1;
attack_type_mod(fierce) -> 1.5.

attack_type_cost(weak) -> 5;
attack_type_cost(basic) -> 10;
attack_type_cost(fierce) -> 20.

stamina_cost({attack, AttackType}) -> attack_type_cost(AttackType);
stamina_cost(dodge) -> 25;
stamina_cost(_) -> 0.

has_stamina(AtkId, ActionData) ->
    AtkUnit = obj:get(AtkId),
    Stamina = maps:get(<<"stamina">>, AtkUnit),
    StaminaCost = stamina_cost(ActionData),

    Result = Stamina >= StaminaCost,
    Result.

sub_stamina(SourceId, Value) ->
    add_stamina(SourceId, -1 * Value).
add_stamina(SourceId, Value) ->
    SourceObj = obj:get(SourceId),
    Stamina = maps:get(<<"stamina">>, SourceObj),
    obj:update(SourceId, 'stamina', Stamina + Value).

num_ticks({attack, _AttackType}) -> ?TICKS_SEC * 10;
num_ticks(guard) -> ?TICKS_SEC * 30;
num_ticks(dodge) -> ?TICKS_SEC * 20. 

get_items_value(_, []) ->
    0;
get_items_value(Attr, Items) ->
    F = fun(Item, Total) ->
            maps:get(Attr, Item, 0) + Total
        end,

    lists:foldl(F, 0, Items).

to_str(weak) -> "w";
to_str(basic) -> "b";
to_str(fierce) -> "f".

check_combos(AttackType, ObjId) ->
    case db:dirty_read(combat, ObjId) of
        [Combat] ->
            Attacks = Combat#combat.attacks ++ to_str(AttackType),

            case check_attacks(Attacks) of
                [{_, ComboName, ComboDmg}] ->
                    NewCombat = Combat#combat {attacks = ""},
                    db:dirty_write(NewCombat),
                    {ComboName, ComboDmg};
                _ -> 
                    NewCombat = Combat#combat {attacks = Attacks},
                    db:dirty_write(NewCombat),
                    {none, 0}
            end;
        _ ->
            Combat = #combat {id = ObjId,
                              attacks = to_str(AttackType)},
            db:dirty_write(Combat),
            {none, 0}
    end.

check_attacks(Attacks) ->
    F = fun({ComboAttacks, _ComboName, _ComboDmg}) ->
            case string:str(Attacks, ComboAttacks) of
                0 -> false;
                _ -> true
            end
        end,

    lists:filter(F, combos()).

combos() ->
    [{"wwf", <<"Shrouded Strike">>, 1.25},
     {"fff", <<"Shatter Strike">>, 1.5},
     {"wbf", <<"Rupture Strike">>, 2},
     {"wbwf", <<"Nightmare Strike">>, 3}].

skill_gain_combo(_AtkId, none) -> nothing;
skill_gain_combo(AtkId, ComboName) ->
    skill:update(AtkId, ComboName, 1).

skill_gain_atk(_AtkId, [], _RandomDmg, _DmgRange, _Dmg) -> nothing;
skill_gain_atk(_AtkId, _Weapons, _RandomDmg, DmgRange, Dmg) when Dmg < (DmgRange * 0.1) -> nothing;
skill_gain_atk(AtkId, Weapons, RandomDmg, DmgRange, _Dmg) when RandomDmg >= (DmgRange - 1) -> 
    lager:info("Skill gain weapons: ~p", [Weapons]), 
    F = fun(Weapon) ->
            Subclass = maps:get(<<"subclass">>, Weapon),
            skill:update(AtkId, Subclass, 1)
        end,

    lists:foreach(F, Weapons);
skill_gain_atk(_AtkId, _Weapons, _RandomDmg, _DmgRange, _Dmg) -> nothing.
