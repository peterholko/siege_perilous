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
-export([add_event/4]).

%%
%% API Functions
%%

add_event(PlayerProcess, EventType, EventData, EventTick) ->
    
    [{counter, tick, CurrentTick}] = db:dirty_read(counter, tick),

    Event = #event { id = counter:increment(event),
                     player_process = PlayerProcess,
                     type = EventType,
                     data = EventData,
                     tick = CurrentTick + EventTick},

    db:write(Event).

start() ->
    gen_server:start({global, game_pid}, game, [], []).

init([]) ->    
    {ok, []}.

terminate(_Reason, _) ->
    ok.

handle_cast(stop, Data) ->
    {stop, normal, Data};

handle_cast(cast, Data) ->
    {noreply, Data}.

handle_call(call, _From, Data) ->
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
