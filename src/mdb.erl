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
-export([update/3, delete/2]).
-export([find/2, find/3, find_one/2, find_one/3, dump/1]).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, mdb_pid}, mdb, [], []).

get_conn() ->
    gen_server:call({global, mdb_pid}, {get_conn}).

update(Collection, Id, Value) when is_map(Value) ->
    ValueTmp = bson:flatten_map(Value),
    NewValue = maps:remove(<<"_id">>, ValueTmp),
    gen_server:cast({global, mdb_pid}, {update, Collection, Id, NewValue}); 
update(Collection, Id, Value) when is_tuple(Value) ->
    gen_server:cast({global, mdb_pid}, {update, Collection, Id, Value}). 

delete(Collection, Id) ->
    gen_server:cast({global, mdb_pid}, {delete, Collection, Id}).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Database = <<"sp">>,
   
    {ok, Connection} = mongo:connect([{database, Database}]),
    io:fwrite("mongo connected."),
    {ok, Connection}.

handle_cast({update, Collection, Id, Value}, Data) ->
    Connection = Data,
    %Exclude id from any updates
    Cmd = {<<"$set">>, Value},
    ok = mongo:update(Connection, Collection, {<<"_id">>, Id}, Cmd),

    {noreply, Data};

handle_cast({delete, Collection, Id}, Data) ->
    Connection = Data,
    mongo:delete(Connection, Collection, {<<"_id">>, Id}),

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

find_one(Collection, Key, Value) ->
    mongo:find_one(mdb:get_conn(), Collection, {Key, Value}).

find(Collection, Key, Value) ->
    Cursor = mongo:find(mdb:get_conn(), Collection, {Key, Value}),
    Results = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Results.

find(Collection, Tuple) ->
    Cursor = mongo:find(mdb:get_conn(), Collection, Tuple),
    Results = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Results.

find_one(Collection, Tuple) ->
    mongo:find_one(mdb:get_conn(), Collection, Tuple).

dump(Collection) ->
    Cursor = mongo:find(mdb:get_conn(), Collection, {}),
    Results = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Results.
