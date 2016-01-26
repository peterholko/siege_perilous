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
-export([attack/3, defend/2]).
-export([has_stamina/2, stamina_cost/1, add_stamina/2, sub_stamina/2, num_ticks/1]).
-export([is_adjacent/2, is_target_alive/1, is_targetable/1]).

%-compile(export_all).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, combat}, combat, [], []).

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
    sub_stamina(SourceId, StaminaCost),

    %Add defense effect
    obj:add_effect(SourceId, DefendType, none).

process_attack(AttackType, AtkId, DefId) ->
    [AtkObj] = db:read(obj, AtkId),

    AtkUnit = obj:get(AtkId),
    DefUnit = obj:get(DefId),

    AtkItems = item:get_equiped(AtkId),
    DefItems = item:get_equiped(DefId),

    AtkWeapons = item:get_equiped_weapon(AtkId),

    BaseDmg = maps:get(<<"base_dmg">>, AtkUnit),
    DmgRange = maps:get(<<"dmg_range">>, AtkUnit),
    BaseDef = maps:get(<<"base_def">>, DefUnit),
    DefHp = maps:get(<<"hp">>, DefUnit),

    HasDefend = has_defend(DefId),

    %Check for combos
    {ComboName, ComboDmg} = check_combos(AttackType, AtkId, AtkObj#obj.subclass),

    %Check if combo is countered
    {Countered, FinalComboDmg} = check_countered(AttackType, HasDefend, ComboDmg),

    %Remove Defend effect
    remove_defend(Countered, DefId, HasDefend),

    %Add item stats
    TotalDmg = (BaseDmg + get_items_value(<<"damage">>, AtkItems)) * FinalComboDmg,
    TotalArmor = BaseDef + get_items_value(<<"armor">>, DefItems),

    %Random roll
    RandomDmg = rand:uniform(DmgRange) + TotalDmg,
    DmgRoll = RandomDmg + TotalDmg,

    %Apply armor and defend ection reductions
    ArmorReduction = TotalArmor / (TotalArmor + 50),
    DmgReducedArmor = round(DmgRoll * (1 - ArmorReduction)),
    DmgReduced = defend_type_mod(HasDefend) * DmgReducedArmor,

    %Apply attack type modifier
    Dmg = DmgReduced * attack_type_mod(AttackType),
    NewHp = DefHp - Dmg,

    %Check for skill increases
    skill_gain_combo(AtkId, ComboName),
    skill_gain_atk(AtkId, AtkWeapons, RandomDmg, DmgRange, Dmg),

    %Update stamina
    sub_stamina(AtkObj#obj.id, stamina_cost(AttackType)),

    %Check if unit is alive
    UnitState = is_unit_dead(NewHp),

    %Broadcast damage
    lager:info("Broadcasting countered: ~p ~p ~p", [Countered, HasDefend, countered(Countered, HasDefend)]),
    broadcast_dmg(AtkId, DefId, AttackType, Dmg, UnitState, ComboName, countered(Countered, HasDefend)),

    %Check if unit is dead 
    case UnitState of
        <<"alive">> ->
            obj:update(DefId, <<"hp">>, NewHp);
        <<"dead">> ->
            AtkXp = maps:get(<<"xp">>, AtkUnit, 0),
            DefKillXp = maps:get(<<"kill_xp">>, DefUnit, 0),
            obj:update(AtkId, <<"xp">>, AtkXp + DefKillXp),
            process_unit_dead(DefId)
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
    HasWall = obj:has_effect(Id, ?WALL),
    IsTargetable = not HasWall,
    lager:info("is_targetable: ~p", [IsTargetable]),
    IsTargetable.

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

    lager:info("Updating unit state"),
    NewObj = obj:update_dead(DefId),

    %Remove potential npc entry
    npc:remove(DefId),

    %Remove potential wall effect
    obj:set_wall_effect(NewObj).

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

check_combos(AttackType, ObjId, ObjSubclass) ->
    case db:dirty_read(combat, ObjId) of
        [Combat] ->
            Attacks = Combat#combat.attacks ++ to_str(AttackType),

            case check_attacks(Attacks, ObjSubclass) of
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

check_attacks(Attacks, ObjSubclass) ->
    F = fun({ComboAttacks, _ComboName, _ComboDmg}) ->
            lager:info("Attacks: ~p ComboAttacks: ~p", [Attacks, ComboAttacks]),            
            case string:str(Attacks, ComboAttacks) of
                0 -> false;
                _ -> true
            end
        end,

    lists:filter(F, combos(ObjSubclass)).

combos(<<"npc">>) ->
    [{"qqqf", <<"Undead Devour">>, 3}];
combos(_) ->
    [{"qqf", <<"Shrouded Strike">>, 1.25},
     {"fff", <<"Shatter Strike">>, 1.5},
     {"qpf", <<"Rupture Strike">>, 2},
     {"qpqf", <<"Nightmare Strike">>, 3}].

check_countered(?QUICK, ?DODGE, ComboDmg) when ComboDmg > 0 -> {true, 0};
check_countered(?PRECISE, ?PARRY, ComboDmg) when ComboDmg > 0 -> {true, 0}; 
check_countered(?FIERCE, ?BRACE, ComboDmg) when ComboDmg > 0 -> {true, 0};
check_countered(_, _, ComboDmg) -> {false, ComboDmg}.

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
    DefendList = [{obj:has_effect(DefId, ?DODGE), ?DODGE},
                  {obj:has_effect(DefId, ?PARRY), ?PARRY},
                  {obj:has_effect(DefId, ?BRACE), ?BRACE}],
    
    F = fun({true, DefendType}, _Acc) -> DefendType;
           ({false, _}, Acc) -> Acc
        end,
    
    lists:foldl(F, false, DefendList).

countered(true, DefendType) -> DefendType;
countered(false, _) -> none.

remove_defend(true, DefId, DefendType) -> obj:remove_effect(DefId, DefendType);
remove_defend(false, _, _) -> nothing.


broadcast_dmg(SourceId, TargetId, AttackType, Dmg, State, ComboName, Countered) ->
    %Convert id here as message is being built
    Message = #{<<"packet">> => <<"dmg">>,
                <<"sourceid">> => util:bin_to_hex(SourceId),
                <<"targetid">> => util:bin_to_hex(TargetId),
                <<"attacktype">> => AttackType,
                <<"dmg">> => Dmg,
                <<"state">> => State,
                <<"combo">> => ComboName,
                <<"countered">> => Countered},

    [SourceObj] = db:read(obj, SourceId),
    [TargetObj] = db:read(obj, TargetId),

    SourcePos = SourceObj#obj.pos,
    TargetPos = TargetObj#obj.pos,

    perception:broadcast(SourcePos, TargetPos, Message).

%set_combat_state(#obj{state = State}) when State =:= combat ->
%    nothing;
%set_combat_state(#obj{id = Id}) ->
%    obj:update_state(Id, combat).

%is_state_not(NotExpectedState, State) when NotExpectedState =:= State -> false;
%is_state_not(_NotExpectedState, _State) -> true.
%
%is_visible(SourceObj, TargetObj) ->
%    perception:is_visible(SourceObj#obj.id, TargetObj#obj.id).
