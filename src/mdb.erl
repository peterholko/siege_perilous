%% -------------------------------------------------------------------
%% Author  : Peter Holko
%%% Description : Mongodb interface
%%%
%%% Created : Dec 5, 2014
%%% -------------------------------------------------------------------
-module(mdb).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("schema.hrl").
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_conn/0]).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, mdb_pid}, mdb, [], []).

get_conn() ->
    gen_server:call({global, mdb_pid}, {get_conn}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Host = "127.0.0.1",
    Port = 27017,
    Database = <<"sp">>,
   
    {ok, Connection} = mongo:connect(Host, Port, Database),
    {ok, Connection}.

handle_cast('TEST', Data) ->   
    {noreply, Data};

handle_cast(stop, Data) ->
    {stop, normal, Data}.

handle_call({get_conn}, _From, Data) ->   
    
    Conn = Data,

    {reply, Conn, Data};

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

