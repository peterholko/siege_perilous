%% -------------------------------------------------------------------
%% Author  : Peter Holko
%%% Description : Battle manager
%%%
%%% Created : Dec 15, 2014
%%% -------------------------------------------------------------------
-module(battle).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/2, add_target/2]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, battle}, battle, [], []).

create(AtkId, DefId) ->
    gen_server:cast({global, battle}, {create, AtkId, DefId}).

add_target(SrcUnitId, TgtUnitId) ->
    gen_server:cast({global, battle}, {add_target, SrcUnitId, TgtUnitId}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast({create, AtkId, DefId}, Data) ->   

    AtkObj = map:get_obj(AtkId), 
    DefObj = map:get_obj(DefId), 

    create_battle(AtkObj, DefObj),
    set_combat_state([AtkObj, DefObj]),

    AtkPerception = perception(AtkObj, DefObj),
    DefPerception = perception(DefObj, AtkObj),

    {noreply, Data};

handle_cast({add_target, SrcUnitId, TgtUnitId}, Data) ->

    db:dirty_delete(target, SrcUnitId), 

    Target = #target {src_unit_id = SrcUnitId,
                      tgt_unit_id = TgtUnitId},

    db:dirty_write(Target),

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

create_battle(AtkObj, DefObj) ->
    Id = counter:increment(battle),
    Battle = #battle {id = Id,
                      attacker = {AtkObj#map_obj.player, AtkObj#map_obj.id},
                      defender = {DefObj#map_obj.player, DefObj#map_obj.id}},

    db:write(Battle),
    Id.

set_combat_state([]) ->
    lager:info("Done updating combat state"),
    done;
set_combat_state([Entity | Rest]) ->
    map:update_obj_state(Entity, combat),
    set_combat_state(Rest).

perception(AtkObj, DefObj) ->

    AtkUnits = units_perception(AtkObj#map_obj.units, []),
    DefUnits = units_perception(DefObj#map_obj.units, []),

    {AtkUnits, DefUnits}.

units_perception([], Units) ->
    Units;
units_perception([UnitId | Rest], Units) ->
    Unit = unit:get_unit_and_type(UnitId),
    units_perception(Rest, [Unit | Units]).


