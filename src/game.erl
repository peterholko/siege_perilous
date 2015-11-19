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
-export([trigger_perception/0, trigger_explored/1]).
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

trigger_perception() ->
    gen_server:cast({global, game_pid}, trigger_perception).

trigger_explored(Player) ->
    gen_server:cast({global, game_pid}, {trigger_explored, Player}).

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
    Perception = false,
    Explored = [],

    Data = #game {perception = Perception,
                  explored = Explored},


    {ok, Data}.

terminate(_Reason, _) ->
    ok.

handle_cast(trigger_perception, #game{perception = _Perception,
                                  explored = Explored}) ->
    NewData = #game {perception = true,
                     explored = Explored},
    {noreply, NewData};

handle_cast({trigger_explored, Player}, #game{perception = Perception,
                                              explored = Explored} ) ->
    NewExplored = [Player | Explored],
    NewData = #game {perception = Perception,
                     explored = NewExplored},
    {noreply, NewData};

handle_cast(reset_perception, _Data) ->
    NewData = #game{perception = false,
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
