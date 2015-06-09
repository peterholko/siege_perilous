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
-export([to_map/1]).
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
    %Exclude id from any updates
    Cmd = {'$set', bson:exclude(['_id'], Value)},
    
    Result = mongo:update(Connection, Collection, {'_id', Id}, Cmd),
    lager:info("Result: ~p", [Result]),

    {noreply, Data};

handle_cast({delete, Collection, Id}, Data) ->
    Connection = Data,
    mongo:delete(Connection, Collection, {'_id', Id}),

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

doc_foldr (Fun, Acc, Doc) -> doc_foldrN (Fun, Acc, Doc, 0, tuple_size (Doc) div 2).

doc_foldrN (_, Acc, _, Low, Low) -> Acc;
doc_foldrN (Fun, Acc, Doc, Low, High) ->
    Acc1 = Fun (element (High * 2 - 1, Doc), element (High * 2, Doc), Acc),
    doc_foldrN (Fun, Acc1, Doc, Low, High - 1).

%  Convert bson document to a map and converts BSON id to hex id
to_map([]) ->
    [];
to_map(Doc) when is_list(Doc) ->
    [D] = Doc, 
    to_map(D);
to_map(Doc) -> doc_foldr (fun (Label, Value, List) -> 
                               maps:put(atom_to_binary(Label, latin1), convert_id(Value), List)
                          end, 
                          maps:new(), 
                          Doc).

convert_id(Value) when is_tuple(Value) ->
    convert_bin_id(Value);
convert_id(ValueList) when is_list(ValueList) ->

    F = fun(V, Acc) ->
        convert_list(V, Acc)
   end,

    lists:foldl(F, [], ValueList);

convert_id(Value) ->
    Value.

convert_list(V, Acc) when is_tuple(V) ->
    FF = fun(Label, Val, List) -> 
            maps:put(atom_to_binary(Label, latin1), convert_id(Val), List)
    end,

    [doc_foldr(FF, maps:new(), V) | Acc];
convert_list(V, Acc) ->
    [V | Acc].

convert_bin_id({Value}) when is_binary(Value) ->
    to_hex(Value, Value);
convert_bin_id(Value) ->
    Value.
    
to_hex(<<_:96>>, Value) ->
    util:bin_to_hex(Value);
to_hex(Value, Value) ->
    Value.
 
