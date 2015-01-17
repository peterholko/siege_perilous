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
-export([get_conn/0, create_obj/2]).
-export([update/3, delete/2, lookup/2, to_bin_id/1]).
%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_server:start({global, mdb_pid}, mdb, [], []).

get_conn() ->
    gen_server:call({global, mdb_pid}, {get_conn}).

update(Collection, Id, Value) ->
    gen_server:cast({global, mdb_pid}, {update, Collection, Id, Value}). 

delete(Collection, Id) ->
    gen_server:cast({global, mdb_pid}, {delete, Collection, Id}).

create_obj(Player, Units) ->
    gen_server:cast({global, mdb_pid}, {create_obj, Player, Units}).

lookup(Attr, Doc) when is_map(Doc) ->
    maps:get(atom_to_binary(Attr, latin1), Doc);
lookup(Attr, Doc) ->
    {Val} = bson:lookup(Attr, Doc),
    Val.

to_bin_id(Id) when is_tuple(Id) ->
    Id;
to_bin_id(BinId = <<_Id:92>>) ->
    {BinId};
to_bin_id(Id) when is_binary(Id) ->
    BinId = util:hex_to_bin(binary_to_list(Id)),
    {BinId}.
%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    Host = "127.0.0.1",
    Port = 27017,
    Database = <<"sp">>,
   
    {ok, Connection} = mongo:connect(Host, Port, Database),
    {ok, Connection}.

handle_cast({create_obj, Player, Units}, Data) ->   
    lager:info("Data: ~p", [Data]),
    Connection = Data,

    Result = mongo:insert(Connection, <<"obj">>, [{player, Player, units, Units}]),
    lager:info("Result: ~p", [Result]),

    {noreply, Data};

handle_cast({update, Collection, Id, Value}, Data) ->
    Connection = Data,
    BinId = to_bin_id(Id),
    Cmd = {'$set', Value},
    
    mongo:update(Connection, Collection, {'_id', BinId}, Cmd),

    {noreply, Data};

handle_cast({delete, Collection, Id}, Data) ->
    Connection = Data,
    BinId = to_bin_id(Id),
    
    mongo:delete(Connection, Collection, {'_id', BinId}),

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

