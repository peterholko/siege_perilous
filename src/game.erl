%% Author: Peter
%% Created: Dec 04, 2014
%% Description: Game process to handle game state
-module(game).
-behaviour(gen_server).

%%
%% Include files
%%
-include("schema.hrl").
-include("common.hrl").
%%
%% Exported Functions
%%
-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add_event/5, cancel_event/1]).
-export([trigger_global/0, trigger_local/1, trigger_explored/2]).
-export([get_perception/0, get_explored/0, reset/0]).

%%
%% API Functions
%%

start() ->
    gen_server:start({global, game_pid}, game, [], []).

add_event(PlayerProcess, EventType, EventData, EventSource, EventTick) ->
    [{counter, tick, CurrentTick}] = db:dirty_read(counter, tick),

    Event = #event { id = counter:increment(event),
                     player_process = PlayerProcess,
                     type = EventType,
                     data = EventData,
                     source = EventSource,
                     tick = CurrentTick + EventTick},

    db:write(Event).

cancel_event(EventSource) ->
    case db:index_read(event, EventSource, #event.source) of
        [Event] ->
            lager:info("cancel_event - Deleting event: ~p", [Event]),
            db:delete(event, Event#event.id);
        _ ->
            lager:info("Cancel_event - none found from ~p", [EventSource]),
            nothing
    end.

trigger_global() ->
    gen_server:cast({global, game_pid}, trigger_global).

trigger_local(Global) ->
    gen_server:cast({global, game_pid}, {trigger_local, Global}).

trigger_explored(Player, Global) ->
    gen_server:cast({global, game_pid}, {trigger_explored, Player, Global}).

get_perception() ->
    gen_server:call({global, game_pid}, get_perception).

get_explored() ->
    gen_server:call({global, game_pid}, get_explored).

reset() ->
    gen_server:cast({global, game_pid}, reset_perception).

%% ====================================================================
%% %% Server functions
%% ====================================================================


init([]) -> 

    GlobalPerception = false,
    LocalPerception = [],
    Perception = {GlobalPerception, LocalPerception},
    Explored = [],

    Data = #game {perception = Perception,
                  explored = Explored},
    {ok, Data}.

terminate(_Reason, _) ->
    ok.

handle_cast(trigger_global, #game{perception = Perception,
                                  explored = Explored}) ->
    {_GPerception, LPerception} = Perception,
    NewPerception = {true, LPerception},
    NewData = #game {perception = NewPerception,
                     explored = Explored},
    {noreply, NewData};

handle_cast({trigger_local, GlobalPos}, #game{perception = Perception,
                                              explored = Explored}) ->
    {GPerception, LPerception} = Perception,
    NewLPerception = [GlobalPos | LPerception],
    NewPerception = {GPerception, NewLPerception},
    NewData = #game {perception = NewPerception,
                     explored = Explored},
    {noreply, NewData};

handle_cast({trigger_explored, Player, GlobalPos}, #game{perception = Perception,
                                                         explored = Explored} ) ->
    NewExplored = [{Player, GlobalPos} | Explored],
    NewData = #game {perception = Perception,
                     explored = NewExplored},
    {noreply, NewData};

handle_cast(reset_perception, _Data) ->
    GlobalPerception = false,
    LocalPerception = [],
    NewPerception = {GlobalPerception, LocalPerception},
    NewData = #game{perception = NewPerception,
                    explored = []},
    {noreply, NewData};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call(get_perception, _From, Data = #game{perception = Perception,
                                                explored = _Explored}) ->
    {reply, Perception, Data};

handle_call(get_explored, _From, Data = #game{perception = _Perception,
                                              explored = Explored}) ->
    {reply, Explored, Data};

handle_call(Event, From, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Event}, 
                              {from, From}
                             ]),
    {noreply, Data}.

handle_info({'EXIT', _Pid, _Reason}, Data) ->
    %% child exit?
    {noreply, Data};

handle_info(Info, Data) ->
    error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Info}]),
    {noreply, Data}.

code_change(_OldVsn, Data, _Extra) ->
    {ok, Data}.

%%
%% Local Functions
%%
