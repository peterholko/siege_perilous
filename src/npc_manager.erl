%% -------------------------------------------------------------------
%% Author  : Peter Holko
%%% Description : NPC Manager server
%%%
%%% Created : Jan 5, 2015
%%% -------------------------------------------------------------------
-module(npc_manager).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_all_npc/0]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, npc_manager}, npc_manager, [], []).

start_all_npc() ->
    gen_server:cast({global, npc_manager}, start_all_npc).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, []}.

handle_cast(start_all_npc, Data) ->   
    AllNPC = db:index_read(player, true, #player.npc),

    start_npc(AllNPC),   

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

start_npc([]) ->
    lager:info("Finished starting NPC processes.");

start_npc([NPC | Rest]) ->

    npc:start(NPC#player.id),

    start_npc(Rest).
