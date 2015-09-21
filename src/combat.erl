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
-export([do_action/1, attack/2]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, combat}, combat, [], []).

do_action(Action) ->
    gen_server:cast({global, combat}, {do_action, Action}).

attack(SourceId, TargetId) ->
    gen_server:cast({global, combat}, {attack, SourceId, TargetId}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    random:seed(erlang:now()),
    {ok, []}.

handle_cast({do_action, Action}, Data) ->
    process_action(Action),
    {noreply, Data};

handle_cast({attack, SourceId, TargetId}, Data) ->
    lager:info("Attack"), 
    [SourceObj] = db:read(local_obj, SourceId),
    [TargetObj] = db:read(local_obj, TargetId),

    Result = is_state_not(dead, SourceObj#local_obj.state) andalso
             is_state_not(dead, TargetObj#local_obj.state),

    set_attack_unit(Result, SourceObj, TargetObj),

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

set_attack_unit(false, _, _) ->
    lager:info("set_attack_unit failed");
set_attack_unit(true, SourceObj, TargetObj) ->
    set_combat_state(SourceObj#local_obj.id),
    set_combat_state(TargetObj#local_obj.id),

    Action = #action {source_id = SourceObj#local_obj.id,
                      type = attack,
                      data = TargetObj#local_obj.id},
    db:write(Action).

process_action(Action) ->
    case Action#action.type of
        attack ->
            process_attack(Action);
        _ ->
            lager:info("Unknown action type: ~p", [Action#action.type]) 
    end.

process_attack(Action) ->
    lager:info("Process attack"),
    SourceId = Action#action.source_id,
    TargetId = Action#action.data,

    SourceObj = get_obj(db:read(local_obj, SourceId)), 
    TargetObj = get_obj(db:read(local_obj, TargetId)),
    
    Result = is_valid_obj(SourceObj) andalso
             is_valid_obj(TargetObj) andalso
             is_adjacent(SourceObj, TargetObj) andalso
             is_target_alive(TargetObj#local_obj.state) andalso
             is_targetable(TargetObj),
    
    process_dmg(Result, SourceId, TargetId).

get_obj([]) ->
    false;
get_obj([Obj]) ->
    Obj.

is_valid_obj(false) ->
    lager:info("Invalid local obj"),
    false;
is_valid_obj(_Obj) ->
    true.

is_adjacent(SourceObj, TargetObj) ->
    {SX, SY} = SourceObj#local_obj.pos,
    TargetPos = TargetObj#local_obj.pos,
    Neighbours = map:neighbours(SX, SY, 32, 38), 
    case lists:member(TargetPos, Neighbours) of
        true ->
            true;
        false ->
            lager:info("Not adjacent"),
            false
    end.

is_targetable(#local_obj{effect = Effect} = LocalObj) ->
    lager:info("LocalObj: ~p", [LocalObj]),
    HasWall = lists:member(<<"wall">>, Effect),
    
    Targetable = not HasWall,
    lager:info("Targetable: ~p", [Targetable]),
    Targetable.

is_target_alive(dead) -> 
    lager:info("Target not alive"),
    false;
is_target_alive(_) -> 
    true.

broadcast_dmg(SourceId, TargetId, Dmg, State) ->
    %Convert id here as message is being built
    Message = #{<<"packet">> => <<"dmg">>,
                <<"sourceid">> => util:bin_to_hex(SourceId),
                <<"targetid">> => util:bin_to_hex(TargetId),
                <<"dmg">> => Dmg,
                <<"state">> => State},

    [SourceObj] = db:read(local_obj, SourceId),
    [TargetObj] = db:read(local_obj, TargetId),

    GlobalPos = SourceObj#local_obj.global_pos,
    SourcePos = SourceObj#local_obj.pos,
    TargetPos = TargetObj#local_obj.pos,

    l_perception:broadcast(GlobalPos, SourcePos, TargetPos, Message).

process_dmg(false, AtkId, _) ->
    db:delete(action, AtkId),
    local:update_state(AtkId, none),
    lager:info("Invalid attack");      
process_dmg(true, AtkId, DefId) ->
    AtkUnit = local_obj:get_stats(AtkId),
    DefUnit = local_obj:get_stats(DefId),

    {AtkObjId} = bson:lookup(obj_id, AtkUnit),
    {DmgBase} = bson:lookup(base_dmg, AtkUnit),
    {DmgRange} = bson:lookup(dmg_range, AtkUnit),

    {DefObjId} = bson:lookup(obj_id, DefUnit),
    {DefArmor} = bson:lookup(base_def, DefUnit),
    {DefHp} = bson:lookup(hp, DefUnit),

    DmgRoll = random:uniform(DmgRange) + DmgBase,
    
    DmgReduction = DefArmor / (DefArmor + 50),

    Dmg = round(DmgRoll * (1 - DmgReduction)),
    NewHp = DefHp - Dmg,

    %Check if unit is alive
    UnitState = is_unit_dead(NewHp),

    %Broadcast damage
    lager:debug("Broadcasting dmg: ~p newHp: ~p", [Dmg, NewHp]),
    broadcast_dmg(AtkId, DefId, Dmg, UnitState),

    %Check if unit is dead 
    case UnitState of
        <<"alive">> ->
            local_obj:update(DefId, 'hp', NewHp);
        <<"dead">> ->
            process_unit_dead(AtkObjId, DefObjId, DefId)
    end.

is_unit_dead(Hp) when Hp =< 0 ->
    <<"dead">>;
is_unit_dead(_Hp) ->
    <<"alive">>.

process_unit_dead(_AtkObjId, _DefObjId, DefId) ->
    lager:info("Unit ~p died.", [DefId]),

    %Remove action associated with dead unit
    db:delete(action, DefId),

    lager:info("Updating unit state"),
    NewLocalObj = local:update_dead(DefId),

    %Remove potential wall effect
    local:set_wall_effect(NewLocalObj).

set_combat_state(Id) ->
    local:update_state(Id, combat).

%is_state(ExpectedState, State) when ExpectedState =:= State -> true;
%is_state(_ExpectdState, _State) -> false.

is_state_not(NotExpectedState, State) when NotExpectedState =:= State -> false;
is_state_not(_NotExpectedState, _State) -> true.
