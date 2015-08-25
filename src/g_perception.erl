%%% -------------------------------------------------------------------
%%% Author  : Peter Holko
%%% Description : Calculates perception data
%%%
%%% Created : Dec 15, 2014
%%% -------------------------------------------------------------------
-module(g_perception).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([recalculate/0]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, g_perception_pid}, g_perception, [], []).

recalculate() ->
    gen_server:cast({global, g_perception_pid}, recalculate).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast(recalculate, Data) ->   

    do_recalculate(),

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

do_recalculate() ->
    lager:info("Global recalculate").

