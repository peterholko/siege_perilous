%% Author: Peter
%% Created: Dec 15, 2008
%% Description: TODO: Add description to db
-module(db).

%%
%% Include files
%%
-include("common.hrl").
-include("schema.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%
%% Exported Functions
%%
-export([create_schema/0, start/0, 
         write/1, read/2, delete/2, index_read/3,
         dirty_write/1, dirty_read/2, dirty_index_read/3, dirty_delete/2, dirty_match_object/1,
         dirty_delete_object/1, dump/1,
         reset_tables/0,
         do/1
        ]).

%%
%% API Functions
%%

create_schema() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),

    ok = application:set_env( mnesia, dump_log_write_threshold, 10000 ), 

    {atomic, ok} = mnesia:create_table(player, [{disc_copies, [node()]}, {attributes, record_info(fields, player)}]),
    {atomic, ok} = mnesia:create_table(connection, [{disc_copies, [node()]}, {attributes, record_info(fields, connection)}]),

    mnesia:add_table_index(player, name),

    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([player, connection], 5000).

write(R) ->
    F = fun() -> mnesia:write(R) end,
    {atomic, Value} = mnesia:transaction(F),
    Value.  

read(T, K) ->
    F = fun() -> mnesia:read(T, K) end,
    {atomic, Value} = mnesia:transaction(F),
    Value.

delete(T, K) ->
    F = fun() -> mnesia:delete(T, K, write) end,
    {atomic, Value} = mnesia:transaction(F),
    Value.  

index_read(T, V, K) ->
    F = fun() ->  mnesia:index_read(T, V, K) end,
    {atomic, Value} = mnesia:transaction(F),
    Value.

dirty_read(T, K) ->
    mnesia:dirty_read(T, K).

dirty_index_read(T, V, K) ->
    mnesia:dirty_index_read(T, V, K).

dirty_write(R) ->
    mnesia:dirty_write(R).

dirty_delete(T, K) ->
    mnesia:dirty_delete(T, K). 

dirty_delete_object(P) ->
    mnesia:dirty_delete_object(P).

dirty_match_object(P) ->
    mnesia:dirty_match_object(P).

dump(Table) ->
    do(qlc:q([X || X <- mnesia:table(Table)])).

%%
%% Local Functions
%%

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

%% Testing data

test_tables() ->
    [{connection, 1, none, none},
     {connection, 2, none, none},
     {connection, 3, none, none},
     {connection, 4, none, none},     
     {connection, 99, none, none},     
     {player, 1, <<"test1">>, <<"123123">>, 0, false},
     {player, 2, <<"test2">>, <<"123123">>, 0, false},
     {player, 3, <<"test3">>, <<"123123">>, 0, false},
     {player, 4, <<"peter">>, <<"123123">>, 0, false},
     {player, 99, <<"test99">>, <<"123123">>, 0, false}].

reset_tables() ->

    F = fun() ->
            lists:foreach(fun mnesia:write/1, test_tables())
        end,    
    mnesia:transaction(F).    
