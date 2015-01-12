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
-export([create/1]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, battle}, battle, [], []).

create(Entities) ->
    gen_server:cast({global, battle}, {create, Entities}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast({create, Entities}, Data) ->   

    create_battle(Entities),
    set_combat_state(Entities),
    set_events(Entities),

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

create_battle(Entities) ->
    Id = counter:increment(battle),
    Battle = #battle {id = Id,
                      entities = Entities},

    db:write(Battle).

set_combat_state([]) ->
    lager:info("Done updating combat state"),
    done;
set_combat_state([EntityId | Rest]) ->
    lager:info("Updating entity ~p to combat state", [EntityId]),
    Entity = map:get_obj(EntityId),
    map:update_obj_state(Entity, combat),

    set_combat_state(Rest).

set_events([]) ->
    lager:info("Done adding battle events");
set_events([EntityId | Rest]) ->
    Entity = get_entity(EntityId),
    {Units} = bson:lookup(units, Entity),

    set_unit_events(Units),

    set_events(Rest).

set_unit_events([]) ->
    lager:info("Done adding unit events");
set_unit_events([UnitId | Rest]) ->

    Unit = get_unit(UnitId),
    {UnitTypeId} = bson:lookup(type, Unit),
    
    UnitType = get_unit_type(UnitTypeId),
    lager:info("UnitType: ~p", [UnitType]),

    set_unit_events(Rest).

get_entity(Id) ->
    BinId = util:hex_to_bin(binary_to_list(Id)),
    Cursor = mongo:find(mdb:get_conn(), <<"obj">>, {'_id', {BinId}}),
    [Entity] = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Entity.

get_unit(Id) -> 
    Cursor = mongo:find(mdb:get_conn(), <<"unit">>, {'_id', Id}),
    [Unit] = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Unit.

get_unit_type(Id) ->
    Cursor = mongo:find(mdb:get_conn(), <<"unit_type">>, {'_id', Id}),
    [UnitType] = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    UnitType.

