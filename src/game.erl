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
-export([add_event/5, cancel_event/1, trigger_global/0, trigger_local/1, get_perception/0, reset_perception/0]).

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
    [Event] = db:index_read(event, EventSource, #event.source),
    db:delete(event, Event#event.id).

trigger_global() ->
    gen_server:cast({global, game_pid}, trigger_global).

trigger_local(Global) ->
    gen_server:cast({global, game_pid}, {trigger_local, Global}).

get_perception() ->
    gen_server:call({global, game_pid}, get_perception).

reset_perception() ->
    gen_server:cast({global, game_pid}, reset_perception).

%% ====================================================================
%% %% Server functions
%% ====================================================================


init([]) ->    
    GlobalPerception = false,
    LocalPerception = [],
    {ok, {GlobalPerception, LocalPerception}}.

terminate(_Reason, _) ->
    ok.

handle_cast(trigger_global, Data) ->
    {_GPerception, LPerception} = Data,
    NewData = {true, LPerception},
    {noreply, NewData};

handle_cast({trigger_local, GlobalPos}, Data) ->
    {GPerception, LPerception} = Data,
    NewLPerception = [GlobalPos | LPerception],
    NewData = {GPerception, NewLPerception},
    {noreply, NewData};

handle_cast(reset_perception, _Data) ->
    GlobalPerception = false,
    LocalPerception = [],
    NewData = {GlobalPerception, LocalPerception},
    {noreply, NewData};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call(get_perception, _From, Data) ->
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
