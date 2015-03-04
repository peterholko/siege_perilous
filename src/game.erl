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
-export([add_event/4, set_perception/1, get_perception/0]).

%%
%% API Functions
%%

start() ->
    gen_server:start({global, game_pid}, game, [], []).

add_event(PlayerProcess, EventType, EventData, EventTick) ->
    [{counter, tick, CurrentTick}] = db:dirty_read(counter, tick),

    Event = #event { id = counter:increment(event),
                     player_process = PlayerProcess,
                     type = EventType,
                     data = EventData,
                     tick = CurrentTick + EventTick},

    db:write(Event).

toggle_global(State) ->
    gen_server:cast({global, game_pid}, {toggle_global, State}).

toggle_local(State, Global) ->
    gen_server:cast({global, game_pid}, {toggle_local, State, Global}).

get_global() ->
    gen_server:call({global, game_pid}, get_global_state).

get_local() ->
    gen_server:call({global, game_pid}, get_local_state).
%% ====================================================================
%% %% Server functions
%% ====================================================================


init([]) ->    
    Perception = false,
    {ok, Perception}.

terminate(_Reason, _) ->
    ok.

handle_cast({toggle_global, State}, _Data) ->
    NewData = State,
    {noreply, NewData};

handle_cast({toggle_local, State, Global}, _Data) ->
    NewData = State,
    {noreply, NewData};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call(get_global, _From, Data) ->
    {reply, Data, Data};

handle_call(get_local, _From, Data) ->
    {reply, Data, Data};

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
