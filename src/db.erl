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

    {atomic, ok} = mnesia:create_table(counter, [{disc_copies, [node()]}, {attributes, record_info(fields, counter)}]),    
    {atomic, ok} = mnesia:create_table(player, [{disc_copies, [node()]}, {attributes, record_info(fields, player)}]),
    {atomic, ok} = mnesia:create_table(connection, [{disc_copies, [node()]}, {attributes, record_info(fields, connection)}]),
    {atomic, ok} = mnesia:create_table(tile, [{ram_copies, [node()]}, {attributes, record_info(fields, tile)}]),  
    {atomic, ok} = mnesia:create_table(map_obj, [{disc_copies, [node()]}, {attributes, record_info(fields, map_obj)}]),    
    {atomic, ok} = mnesia:create_table(explored_map, [{ram_copies, [node()]}, {attributes, record_info(fields, explored_map)}]),  
    {atomic, ok} = mnesia:create_table(perception, [{ram_copies, [node()]}, {attributes, record_info(fields, perception)}]),  
    {atomic, ok} = mnesia:create_table(event, [{disc_copies, [node()]}, {attributes, record_info(fields, event)}]),    
    {atomic, ok} = mnesia:create_table(battle, [{disc_copies, [node()]}, {attributes, record_info(fields, battle)}]),    
    {atomic, ok} = mnesia:create_table(battle_obj, [{type, bag}, {disc_copies, [node()]}, {attributes, record_info(fields, battle_obj)}]),    
    {atomic, ok} = mnesia:create_table(battle_unit, [{disc_copies, [node()]}, {attributes, record_info(fields, battle_unit)}]),    
    {atomic, ok} = mnesia:create_table(local_map, [{type, bag}, {disc_copies, [node()]}, {attributes, record_info(fields, local_map)}]),    
    {atomic, ok} = mnesia:create_table(charge_time, [{disc_copies, [node()]}, {attributes, record_info(fields, charge_time)}]),    
    {atomic, ok} = mnesia:create_table(action, [{disc_copies, [node()]}, {attributes, record_info(fields, action)}]),    
    {atomic, ok} = mnesia:create_table(resource, [{type, bag}, {disc_copies, [node()]}, {attributes, record_info(fields, resource)}]),    

    mnesia:add_table_index(player, name),
    mnesia:add_table_index(player, npc),
    mnesia:add_table_index(connection, socket),
    mnesia:add_table_index(map_obj, pos),
    mnesia:add_table_index(map_obj, class),
    mnesia:add_table_index(map_obj, player),
    mnesia:add_table_index(event, tick),
    mnesia:add_table_index(battle_unit, battle),
    mnesia:add_table_index(battle_unit, pos),
    mnesia:add_table_index(action, battle),

    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([counter, player, connection, tile, map_obj, explored_map, perception,
                            event, battle, battle_unit, charge_time, action, resource, local_map], 5000).

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
    [
     {connection, 1, none},
     {connection, 2, none},
     {connection, 3, none},
     {connection, 4, none},     
     {connection, 99, none},     
     {connection, 100, none},     
     {player, 1, <<"test1">>, <<"123123">>, 0, false, false},
     {player, 2, <<"test2">>, <<"123123">>, 0, false, false},
     {player, 3, <<"test3">>, <<"123123">>, 0, false, false},
     {player, 4, <<"peter">>, <<"123123">>, 0, false, false},
     {player, 99, <<"zombie99">>, <<"123123">>, 0, false, true},
     {player, 100, <<"zombie100">>, <<"123123">>, 0, false, true},
     {explored_map, 1, [{2,2},{2,1},{1,0},{0,1},{0,2},{1,2},{1,1}]},
     {map_obj, {<<84,130,44,203,28,147,177,96,56,16,143,37>>}, {1,1}, 1, entity, <<"heromage">>, none},
     {map_obj, {<<84,130,44,203,28,147,177,96,56,16,143,20>>}, {1,0}, 2, entity, <<"zombie">>, none},
     {resource, <<"Copper Ore">>, ?MOUNTAINS},
     {resource, <<"Copper Ore">>, ?HILLS}
    ].

reset_tables() ->

    F = fun() ->
            lists:foreach(fun mnesia:write/1, test_tables())
        end,    
    mnesia:transaction(F).    
