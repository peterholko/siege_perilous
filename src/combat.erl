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
-export([combo/2, attack/3, defend/2, spell/3]).
-export([has_stamina/2, stamina_cost/1, num_ticks/1]).
-export([is_valid_target/1, is_adjacent/2, is_target_alive/1, is_targetable/1, is_combo_type/1, in_range/2]).
-export([init_combos/1]).

-ifdef(EUNIT_TEST).
-compile(export_all).
-endif.

%% Types





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

spell(SpellType, SourceId, TargetId) ->
    gen_server:cast({global, combat}, {spell, SpellType, SourceId, TargetId}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast({attack, AttackType, SourceId, TargetId}, Data) ->
    lager:debug("Attack ~p ~p ~p", [AttackType, SourceId, TargetId]), 
    process_attack(AttackType, SourceId, TargetId),

    {noreply, Data};

handle_cast({defend, DefendType, SourceId}, Data) ->
    lager:debug("Defend ~p ~p", [DefendType, SourceId]),
    process_defend(SourceId, DefendType),

    {noreply, Data};

handle_cast({spell, SpellType, SourceId, TargetId}, Data) ->
    lager:debug("Spell ~p ~p ~p", [SpellType, SourceId, TargetId]), 
    process_spell(SpellType, SourceId, TargetId),

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
    %Check for any post events that should be cancelled
    remove_post_events(DefId),

    HasDodge = effect:has_effect(DefId, ?DODGE),
    HasParry = effect:has_effect(DefId, ?PARRY),

    Dodged = check_dodge(HasDodge),
    Parried = check_parry(HasParry),

    process_damage(Dodged, Parried, AttackType, AtkId, DefId).

process_spell(_SpellType, CasterId, DefId) ->
    %TODO spell damage
    FinalDamage = 0,

    DefHp = obj_attr:value(DefId, <<"hp">>),
    NewHp = DefHp - FinalDamage,

    UnitState = is_dead(NewHp),
    Combo = false,
    Countered = false,

    broadcast_dmg(CasterId, DefId, ?SHADOW_BOLT, FinalDamage, UnitState, Combo, 
                  Countered),

    %Check if unit is dead 
    case UnitState of
        <<"alive">> ->
            obj_attr:set(DefId, <<"hp">>, NewHp);
        <<"dead">> ->
            process_unit_dead(DefId)
    end.

%(Dodged, Parried, CombatData)
process_damage(true, _Parried, _AttackType, _AtkId, _DefId) ->
    0; %Return 0 damage
process_damage(_Dodged, parry, _AttackType, _AtkId, DefId) ->
    %Add attack speed penalty
    effect:add(DefId, ?ATTACK_SPEED, {-0.4, 5}),

    %Return 0 damage
    0;
process_damage(_Dodged, parry_attack, _AttackType, _AtkId, DefId) ->
    %Add attack speed penalty
    effect:add(DefId, ?ATTACK_SPEED, {-0.4, 5}),

    %TODO extra attack

    %Return 0 damage
    0;
process_damage(_Dodge, parry_disarm, AttackType, AtkId, DefId) ->
    %Add disarm 
    effect:add(AtkId, ?DISARM),

    %TODO Calculate full damage
    calculate_damage(AttackType, AtkId, DefId);

process_damage(_Dodge, _Parried, AttackType, AtkId, DefId) ->
    calculate_damage(AttackType, AtkId, DefId).

calculate_damage(AttackType, AtkId, DefId) ->
    %Get Base values
    BaseDamage = obj_attr:value(AtkId, <<"base_dmg">>),
    DamageRange = obj_attr:value(AtkId, <<"dmg_range">>),
    BaseDef = obj_attr:value(DefId, <<"base_def">>),
    DefHp = obj_attr:value(DefId, <<"hp">>),

    %Get Items and Weapons 
    AtkItems = item:get_equiped(AtkId),
    DefItems = item:get_equiped(DefId),

    AtkWeapons = item:get_equiped_weapon(AtkId),

    %Get effect modifications
    AtkEffectDamage = effect_damage(AtkId),
    DefEffectDef = effect_def(DefId),

    %Get Damage from items
    ItemDamage = get_items_value(<<"damage">>, AtkItems),

    %Get Damage Effects from items
    AtkItemDamageEffect = get_items_effect_damage(AtkItems),

    %Get Attack Type modification
    AttackTypeDamage = attack_type_mod(AttackType),

    %Get Damage Reduction from Defensive action if any
    %DefensiveDamageReduction = defensive_type_damage_mod(

    %Determine Armor from items
    ItemArmor = get_items_value(<<"armor">>, DefItems),

    %Add attack type to attacks list
    add_attack(AtkId, AttackType),

    %Has defense stance
    HasDefend = has_defend(DefId),

    %Check if combo is finished
    Combo = check_combo(AtkId),
    lager:debug("Combo: ~p", [Combo]),

    %Check if combo is countered
    Countered = check_countered(AttackType, HasDefend, Combo),
    lager:debug("Countered: ~p", [Countered]),
    
    %Remove defend effect if countered
    remove_defend(Countered, DefId, HasDefend),

    %Determine Combo Damage 
    ComboDamage = combo_damage(Combo, Countered),

    %Check weapon effect
    _Effect = check_weapon_effect(AtkWeapons, DefId),

    %Check if Attacker is Fortified
    %FortifyPenalty = check_fortified(AtkId, AtkWeapons),

    %Random roll
    RollDamage = util:rand(DamageRange) + BaseDamage,

    %Calculate Total Base Damage
    lager:debug("Roll: ~p Base: ~p Combo: ~p Type: ~p Effect: ~p ItemEffect: ~p", [RollDamage, ItemDamage, ComboDamage, AttackTypeDamage, AtkEffectDamage, AtkItemDamageEffect]),
    TotalDamage = (RollDamage + ItemDamage) * ComboDamage * AttackTypeDamage * AtkEffectDamage * AtkItemDamageEffect,

    %Determin Total armor
    TotalArmor = (BaseDef * DefEffectDef) + ItemArmor,

    %Apply armor and defend ection reductions
    ArmorReduction = TotalArmor / (TotalArmor + 50),

    %Calculate Damage Reduction
    DamageArmorReduced = round(TotalDamage * (1 - ArmorReduction)),

    %Determine defensive modification if any
    DefendTypeMod = defend_type_mod(HasDefend),

    %Determine terrain mod if any
    %TerrainMod = map:defense_bonus(DefObj#obj.pos),
    TerrainMod = 1,

    %Calculate final damage reduction
    FinalDamage = TerrainMod * DefendTypeMod * DamageArmorReduced,

    %Calculate new HP
    NewHp = DefHp - FinalDamage,

    %Check for skill increases
    %skill_gain_combo(AtkId, ComboName),
    %skill_gain_atk(AtkId, AtkWeapons, RandomDmg, DmgRange, Dmg),

    %Update stamina
    obj:update_stamina(AtkId, -1 * stamina_cost(AttackType)),

    %Check if unit is alive
    UnitState = is_dead(NewHp),

    %Broadcast damage
    lager:debug("Broadcasting countered: ~p ~p ~p", [Countered, HasDefend, countered(Countered, HasDefend)]),
    broadcast_dmg(AtkId, DefId, AttackType, FinalDamage, UnitState, Combo, countered(Countered, HasDefend)),

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
    IsTargetable.

is_combo_type(?QUICK) -> true;
is_combo_type(?PRECISE) -> true;
is_combo_type(?FIERCE) -> true;
is_combo_type(_) -> false.

in_range(SourceObj, TargetObj) ->
    WeaponRange = item:get_weapon_range(SourceObj),

    case WeaponRange =:= 1 of
        true ->
            is_adjacent(SourceObj, TargetObj);
        false ->
            Distance = map:distance(obj:pos(SourceObj), obj:pos(TargetObj)),
            Distance =< WeaponRange
    end.

is_target_alive(#obj {state = State}) when State =:= dead -> 
    false;
is_target_alive(_) -> 
    true.

is_dead(Hp) when Hp =< 0 ->
    <<"dead">>;
is_dead(_Hp) ->
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

defend_type_mod(?DODGE) -> 1;
defend_type_mod(?PARRY) -> 1;
defend_type_mod(?BRACE) -> 0.66;
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

get_items_effect_damage(Items) ->
    F = fun(Item, Total) ->
            ItemSubclass = maps:get(<<"subclass">>, Item),
            Effects = maps:get(<<"effects">>, Item),

            G = fun(Effect, EffectTotal) ->
                    EffectType = maps:get(<<"type">>, Effect),
                    EffectValue = maps:get(<<"value">>, Effect),

                    case check_effect_item_damage(EffectType, ItemSubclass) of
                        true -> EffectTotal + EffectValue;
                        false -> EffectTotal
                    end
                end,

            Total + lists:foldl(G, 0, Effects)
        end,

    lists:foldl(F, 1, Items).

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
    Pattern = {combo, AtkId, '_', Attack#attack.combo_type, Attack#attack.types},

    case db:match_object(Pattern) of
        [Combo] -> Combo#combo.name;
        _ -> false
    end.

check_countered(_, _, false) -> false;
check_countered(?QUICK, ?DODGE, _Combo) -> true;
check_countered(?PRECISE, ?PARRY, _Combo) -> true;
check_countered(?FIERCE, ?BRACE, _Combo) -> true;
check_countered(_, _, _) -> false.

%skill_gain_combo(_AtkId, none) -> nothing;
%skill_gain_combo(AtkId, ComboName) ->
%    skill:update(AtkId, ComboName, 1).
%
%skill_gain_atk(_AtkId, [], _RandomDmg, _DmgRange, _Dmg) -> nothing;
%skill_gain_atk(_AtkId, _Weapons, _RandomDmg, DmgRange, Dmg) when Dmg < (DmgRange * 0.1) -> nothing;
%skill_gain_atk(AtkId, Weapons, RandomDmg, DmgRange, _Dmg) when RandomDmg >= (DmgRange - 1) -> 
%    lager:info("Skill gain weapons: ~p", [Weapons]), 
%    F = fun(Weapon) ->
%            Subclass = maps:get(<<"subclass">>, Weapon),
%            skill:update(AtkId, Subclass, 1)
%        end,
%
%    lists:foreach(F, Weapons);
%skill_gain_atk(_AtkId, _Weapons, _RandomDmg, _DmgRange, _Dmg) -> nothing.

has_defend(DefId) ->
    DefendList = [{effect:has_effect(DefId, ?DODGE), ?DODGE},
                  {effect:has_effect(DefId, ?PARRY), ?PARRY},
                  {effect:has_effect(DefId, ?BRACE), ?BRACE}],
    
    F = fun({true, DefendType}, _Acc) -> DefendType;
           ({false, _}, Acc) -> Acc
        end,
    
    lists:foldl(F, false, DefendList).

countered(true, DefendType) -> DefendType;
countered(false, _) -> false.

remove_defend(true, DefId, DefendType) -> effect:remove(DefId, DefendType);
remove_defend(false, _, _) -> nothing.

remove_post_events(ObjId) ->
    case game:has_post_events(ObjId) of
        true -> game:cancel_event(ObjId);
        false -> none
    end.

%is_range(<<"Bow">>) -> true;
%is_range(_) -> false.

%check_fortified(AtkId, AtkWeapons) ->
%    F = fun(Weapon) ->
%            WeaponSubClass = maps:get(<<"subclass">>, Weapon),
%            is_range(WeaponSubClass)
%        end,
%
%    HasRange = lists:any(F, AtkWeapons),
%    HasFortified = effect:has_effect(AtkId, ?FORTIFIED),
%
%    %Penalize melee attackers with fortified by 75%
%    case {HasFortified, HasRange} of 
%        {true, true} -> 1;
%        {true, false} -> 0.25;
%        _ -> 1
%    end.

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

check_weapon_effect([AtkWeapon], DefId) ->
    lager:info("Check Weapon Effect ~p", [AtkWeapon]),
    Subclass = item_attr:value(item:id(AtkWeapon), <<"subclass">>, none),
    RandNum = util:rand(100),    
    lager:info("Subclass ~p RanNum: ~p", [Subclass, RandNum]),

    Effect = check_weapon_subclass(Subclass, RandNum, DefId),    
    Effect;
check_weapon_effect(_, _) ->
    false.

check_weapon_subclass(?AXE, RandNum, DefId) when RandNum =< ?DEEP_WOUND_CHANCE -> 
    lager:info("Deep Wound!"),
    effect:add(DefId, ?DEEP_WOUND, none),
    ?DEEP_WOUND;
check_weapon_subclass(?SWORD, RandNum, DefId) when RandNum =< ?BLEED_CHANCE -> 
    effect:add(DefId, ?BLEED, none),
    ?BLEED;
check_weapon_subclass(?HAMMER, RandNum, DefId) when RandNum =< ?CONCUSS_CHANCE -> 
    effect:add(DefId, ?CONCUSS, none),
    ?CONCUSS;
check_weapon_subclass(?DAGGER, RandNum, DefId) when RandNum =< ?DISARM_CHANCE -> 
    effect:add(DefId, ?DISARM, none),
    ?DISARM;
check_weapon_subclass(?IMPALE_CHANCE, RandNum, DefId) when RandNum =< ?IMPALE_CHANCE -> 
    effect:add(DefId, ?IMPALE, none),
    ?IMPALE;
check_weapon_subclass(Subclass, RandNum, DefId) -> 
    lager:info("~p ~p ~p", [Subclass, RandNum, DefId]),
    false.

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
combo_damage(?SHROUDED_SLASH, false) -> 2;
combo_damage(?SHATTER_CLEAVE, false) -> 2.5;
combo_damage(?MASSIVE_PUMMEL, false) -> 4;
combo_damage(?NIGHTMARE_STRIKE, false) -> 8;
combo_damage(_, _) -> 1.

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

effect_damage(AtkId) ->
    Effects = effect:all(AtkId),

    F = fun(Effect, Acc) ->
            case Effect#effect.type of
                ?CONCUSS -> (1 + ?CONCUSS_DAMAGE) * Acc;
                ?DISARM ->  (1 + ?DISARM_DAMAGE) * Acc;
                ?DEMORALIZING_SHOUT -> (1 + ?DEMORALIZING_SHOUT_DAMAGE) * Acc;
                _ -> Acc
            end

        end,

    lists:foldl(F, 1, Effects).

effect_def(DefId) ->
    Effects = effect:all(DefId),

    F = fun(Effect, Acc) ->
            case Effect#effect.type of
                ?CONCUSS -> (1 + ?CONCUSS_DEF) * Acc;
                ?EXPOSE_ARMOR -> (1 + ?EXPOSE_ARMOR_DEF) * Acc;
                _ -> Acc
            end

        end,

    lists:foldl(F, 1, Effects).

check_effect_item_damage(<<"Axe">>, ?AXE_DMG_P) -> true;
check_effect_item_damage(<<"Sword">>, ?SWORD_DMG_P) -> true;
check_effect_item_damage(<<"Hammer">>, ?HAMMER_DMG_P) -> true;
check_effect_item_damage(<<"Dagger">>, ?DAGGER_DMG_P) -> true;
check_effect_item_damage(<<"Spear">>, ?SPEAR_DMG_P) -> true;
check_effect_item_damage(_, _) -> false.
