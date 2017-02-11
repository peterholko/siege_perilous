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
-export([combo/2, attack/3, defend/2]).
-export([has_stamina/2, stamina_cost/1, num_ticks/1]).
-export([is_valid_target/1, is_adjacent/2, is_target_alive/1, is_targetable/1, is_combo_type/1]).
-export([init_combos/1]).

%-compile(export_all).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, combat}, combat, [], []).

combo(SourceId, ComboType) ->
    reset_attacks(SourceId, ComboType).

attack(AttackType, SourceId, TargetId) ->
    gen_server:cast({global, combat}, {attack, AttackType, SourceId, TargetId}).

defend(DefendType, SourceId) ->
    gen_server:cast({global, combat}, {defend, DefendType, SourceId}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast({attack, AttackType, SourceId, TargetId}, Data) ->
    lager:info("Attack ~p ~p ~p", [AttackType, SourceId, TargetId]), 
    process_attack(AttackType, SourceId, TargetId),

    {noreply, Data};

handle_cast({defend, DefendType, SourceId}, Data) ->
    lager:info("Defend ~p ~p", [DefendType, SourceId]),
    process_defend(SourceId, DefendType),

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

process_defend(SourceId, DefendType) ->
    lager:info("Process Defend"),

    %Update stamina
    StaminaCost = stamina_cost(DefendType),
    obj:update_stamina(SourceId, -1 * StaminaCost),

    %Add defense effect
    effect:add(SourceId, DefendType, none).

process_attack(AttackType, AtkId, DefId) ->
    [AtkObj] = db:read(obj, AtkId),
    %[DefObj] = db:read(obj, DefId),

    %Check for any post events that should be cancelled
    remove_post_events(DefId),

    AtkItems = item:get_equiped(AtkId),
    DefItems = item:get_equiped(DefId),

    AtkWeapons = item:get_equiped_weapon(AtkId),

    BaseDmg = obj_attr:value(AtkId, <<"base_dmg">>),
    DmgRange = obj_attr:value(AtkId, <<"dmg_range">>),

    ItemDamage = get_items_value(<<"damage">>, AtkItems),
  
    BaseDef = obj_attr:value(DefId, <<"base_def">>),
    DefHp = obj_attr:value(DefId, <<"hp">>),
    ItemArmor = get_items_value(<<"armor">>, DefItems),

    HasDodge = effect:has_effect(DefId, ?DODGE),
    HasParry = effect:has_effect(DefId, ?PARRY),

    Dodged = check_dodge(HasDodge),
    Parried = check_parry(HasParry),

    Damage = process_damage(Dodged, Parried, CombatData),

%(Dodged, Parried, CombatData)
process_damage(true, _Parried, _CombatData) ->
    0; %Return 0 damage
process_damage(_Dodged, parry, _CombatData) ->
    %Add attack speed penalty
    effect:add(DefId, ?ATTACK_SPEED, {-0.4, 5}),

    %Return 0 damage
    0;
process_damage(_Dodged, parry_attack, _CombatData) ->
    %Add attack speed penalty
    effect:add(DefId, ?ATTACK_SPEED, {-0.4, 5}),

    %TODO extra attack

    %Return 0 damage
    0;
process_damage(_Dodge, parry_disarm, _CombatData) ->
    %Add disarm 
    effect:add(DefId, ?DISARM, none),

    %TODO Calculate full damage
    100;

process_damage(_Dodge, _Parried, CombatData) ->
    %TODO Calculate full damage

    %Add attack type to attacks list
    add_attack(AtkId, AttackType),

    %Has defense stance
    HasDefend = has_defend(DefId),

    %Check if combo is finished
    Combo = check_combo(AttackType, AtkId),

    %Check if combo is countered
    Countered = check_countered(AttackType, HasDefend, Combo),
    
    %Remove defend effect if countered
    remove_defend(Countered, DefId, HasDefend),

    %Determine Combo Damage 
    ComboDamage = combo_damage(Combo, Countered),

    %Check weapon effect
    %{Effect, EffectDamage} = check_weapon_effect(AtkWeapons, DefId, ComboDmg),

    %Check if Attacker is Fortified
    %FortifyPenalty = check_fortified(AtkId, AtkWeapons),

    %Calculate Total Base Damage
    TotalDmg = (BaseDmg + get_items_value(<<"damage">>, AtkItems)), 

    %Calculate Total Quick Damage
    TotalQuickDmg = (

    TotalArmor = BaseDef + get_items_value(<<"armor">>, DefItems),

    %Random roll
    RandomDmg = util:rand(DmgRange) + TotalDmg,
    DmgRoll = RandomDmg + TotalDmg,

    %Apply armor and defend ection reductions
    ArmorReduction = TotalArmor / (TotalArmor + 50),
    DmgReducedArmor = round(DmgRoll * (1 - ArmorReduction)),

    DefendTypeMod = defend_type_mod(HasDefend),
    %TerrainMod = map:defense_bonus(DefObj#obj.pos),
    TerrainMod = 1,

    DmgReduced = TerrainMod * DefendTypeMod * DmgReducedArmor,

    %Apply attack type modifier
    Dmg = DmgReduced * attack_type_mod(AttackType),
    NewHp = DefHp - Dmg,

    %Check for skill increases
    skill_gain_combo(AtkId, ComboName),
    skill_gain_atk(AtkId, AtkWeapons, RandomDmg, DmgRange, Dmg),

    %Update stamina
    obj:update_stamina(AtkObj#obj.id, -1 * stamina_cost(AttackType)),

    %Check if unit is alive
    UnitState = is_unit_dead(NewHp),

    %Broadcast damage
    lager:info("Broadcasting countered: ~p ~p ~p", [Countered, HasDefend, countered(Countered, HasDefend)]),
    broadcast_dmg(AtkId, DefId, AttackType, Dmg, UnitState, ComboName, countered(Countered, HasDefend)),

    %Check if unit is dead 
    case UnitState of
        <<"alive">> ->
            obj_attr:set(DefId, <<"hp">>, NewHp);
        <<"dead">> ->
            %AtkXp = maps:get(<<"xp">>, AtkUnit, 0),
            %DefKillXp = maps:get(<<"kill_xp">>, DefUnit, 0),
            %obj:update(AtkId, <<"xp">>, AtkXp + DefKillXp),
            process_unit_dead(DefId)
    end.

is_valid_target(TargetId) ->
    case db:read(obj, TargetId) of
        [Target] -> Target;
        [] -> false
    end.

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
    HasWall = effect:has_effect(Id, ?FORTIFIED),
    IsTargetable = not HasWall,
    lager:info("is_targetable: ~p", [IsTargetable]),
    IsTargetable.

is_combo_type(?QUICK) -> true;
is_combo_type(?PRECISE) -> true;
is_combo_type(?FIERCE) -> true;
is_combo_type(_) -> false.

is_target_alive(#obj {state = State}) when State =:= dead -> 
    false;
is_target_alive(_) -> 
    true.

is_unit_dead(Hp) when Hp =< 0 ->
    <<"dead">>;
is_unit_dead(_Hp) ->
    <<"alive">>.

process_unit_dead(DefId) ->
    lager:info("Unit ~p died.", [DefId]),
    NewObj = obj:update_dead(DefId),

    %Remove potential npc entry
    npc:remove(DefId),

    %Trigger removal of any effects caused by this obj
    obj:trigger_effects(NewObj).

attack_type_mod(?QUICK) -> 0.5;
attack_type_mod(?PRECISE) -> 1;
attack_type_mod(?FIERCE) -> 1.5.

defend_type_mod(?DODGE) -> 0.60;
defend_type_mod(?PARRY) -> 0.70;
defend_type_mod(?BRACE) -> 0.80;
defend_type_mod(_) -> 1.

stamina_cost(?QUICK) -> 5;
stamina_cost(?PRECISE) -> 10;
stamina_cost(?FIERCE) -> 20;
stamina_cost(?DODGE) -> 10;
stamina_cost(?PARRY) -> 10;
stamina_cost(?BRACE) -> 10;
stamina_cost(_) -> 0.

has_stamina(AtkId, ActionData) ->
    Stamina = obj_attr:value(AtkId, <<"stamina">>),
    StaminaCost = stamina_cost(ActionData),

    Result = Stamina >= StaminaCost,
    Result.

num_ticks({attack, _AttackType}) -> ?TICKS_SEC * 5;
num_ticks({defend, _DefendType}) -> ?TICKS_SEC * 15.

get_items_value(_, []) ->
    0;
get_items_value(Attr, Items) ->
    F = fun(Item, Total) ->
            maps:get(Attr, Item, 0) + Total
        end,

    lists:foldl(F, 0, Items).

to_str(?QUICK) -> "q";
to_str(?PRECISE) -> "p";
to_str(?FIERCE) -> "f".

check_combo(AtkId) ->
    case db:read(attack, AtkId) of
        [Attack] ->        
            check_attacks(AtkId, Attack);
        _ -> 
            false
    end.

check_attacks(AtkId, Attack) ->
    Pattern = {AtkId, '_', Attack#attack.combo_type, Attack#attack.types},

    case db:match_obj(combo, Pattern) of
        [Combo] -> Combo;
        _ -> false
    end.

check_countered(_, _, false) -> false;
check_countered(?QUICK, ?DODGE, _Combo) -> true;
check_countered(?PRECISE, ?PARRY, _Combo) -> true;
check_countered(?FIERCE, ?BRACE, _Comobo) -> true.

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

has_defend(DefId) ->
    DefendList = [{effect:has_effect(DefId, ?DODGE), ?DODGE},
                  {effect:has_effect(DefId, ?PARRY), ?PARRY},
                  {effect:has_effect(DefId, ?BRACE), ?BRACE}],
    
    F = fun({true, DefendType}, _Acc) -> DefendType;
           ({false, _}, Acc) -> Acc
        end,
    
    lists:foldl(F, false, DefendList).

countered(true, DefendType) -> DefendType;
countered(false, _) -> none.

remove_defend(true, DefId, DefendType) -> effect:remove(DefId, DefendType);
remove_defend(false, _, _) -> nothing.

remove_post_events(ObjId) ->
    case game:has_post_events(ObjId) of
        true -> game:cancel_event(ObjId);
        false -> none
    end.

is_range(<<"Bow">>) -> true;
is_range(_) -> false.

check_fortified(AtkId, AtkWeapons) ->
    F = fun(Weapon) ->
            WeaponSubClass = maps:get(<<"subclass">>, Weapon),
            is_range(WeaponSubClass)
        end,

    HasRange = lists:any(F, AtkWeapons),
    HasFortified = effect:has_effect(AtkId, ?FORTIFIED),

    %Penalize melee attackers with fortified by 75%
    case {HasFortified, HasRange} of 
        {true, true} -> 1;
        {true, false} -> 0.25;
        _ -> 1
    end.

broadcast_dmg(SourceId, TargetId, AttackType, Dmg, State, ComboName, Countered) ->
    Message = #{<<"packet">> => <<"dmg">>,
                <<"sourceid">> => SourceId,
                <<"targetid">> => TargetId,
                <<"attacktype">> => AttackType,
                <<"dmg">> => Dmg,
                <<"state">> => State,
                <<"combo">> => ComboName,
                <<"countered">> => Countered},

    [SourceObj] = db:read(obj, SourceId),
    [TargetObj] = db:read(obj, TargetId),

    SourcePos = SourceObj#obj.pos,
    TargetPos = TargetObj#obj.pos,
    Range = 2,

    perception:broadcast(SourcePos, TargetPos, Range, Message).

check_weapon_effect([AtkWeapon], DefId, ComboDmg) when ComboDmg > 0 ->
    DefHp = obj_attr:value(DefId, <<"hp">>),

    %Check deep wound
    DeepWoundChance = item_attr:value(item:id(AtkWeapon), ?DEEP_WOUND_CHANCE, 0), 
    case util:rand(100) =< DeepWoundChance of 
        true -> {?DEEP_WOUND, 100};
        false -> {none, DefHp * 0.2}
    end.

check_dodge(false) -> false;
check_dodge(true) -> util:rand(100) =< ?DODGE_CHANCE.

check_parry(false) -> false;
check_parry(true) -> 
    case util:rand(100) of
        Rand when Rand =< ?PARRY_CHANCE -> parry;
        Rand when Rand =< ?PARRY_ATTACK_CHANCE -> parry_attack;
        Rand when Rand =< ?PARRY_DISARM_CHANCE -> parry_disarm;
        _ -> false
    end.



init_combos(ObjId) ->
    ComboDefs = db:dump(combo_def),

    F = fun(ComboDef) ->
            Combo = #combo {id = ObjId, 
                            name = ComboDef#combo_def.name,
                            combo_type = ComboDef#combo_def.combo_type,
                            attacks = ComboDef#combo_def.attacks},

            db:write(Combo)
        end,

    lists:foreach(F, ComboDefs).

combo_damage(_, true) -> 0;
combo_damage(?GOUGE, false) -> 1;
combo_damage(?HAMSTRING, false) -> 1;
combo_damage(?IMTIMIDATING_SHOUT, false) -> 1;
combo_damage(?SHROUDED_SLASH, false) -> {?QUICK, 2};
combo_damage(?SHATTER_CLEAVE, false) -> {?FIERCE, 2.5};
combo_damage(?MASSIVE_PUMMEL, false) -> {?PRECISE, 4};
combo_damage(?NIGHTMARE_STRIKE, false) -> {?FIERCE, 8}.

reset_attacks(SourceId, ComboType) ->
    NewAttacks = #attack {id = SourceId,
                          combo_type = ComboType,
                          types = []},
    db:write(NewAttacks).

add_attack(AtkId, AttackType) ->
    case db:read(attack, AtkId) of
        [Attack] ->        
            Types = Attack#attack.types ++ to_str(AttackType),
            NewAttack = Attack#attack {types = Types},
            db:write(NewAttack);
        _ -> 
            %Has not started a combo, hence, do not save attack
            nothing
    end.


