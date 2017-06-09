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
-export([create_schema/0, start/0, first/1, next/2,
         write/1, read/2, delete/2, index_read/3, select/2, match_object/1,
         dirty_write/1, dirty_read/2, dirty_index_read/3, dirty_delete/2, dirty_match_object/1,
         dirty_delete_object/1, dump/1,
         import/1,
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

    {atomic, ok} = mnesia:create_table(counter, [{ram_copies, [node()]}, {attributes, record_info(fields, counter)}]),    
    {atomic, ok} = mnesia:create_table(player, [{ram_copies, [node()]}, {attributes, record_info(fields, player)}]),
    {atomic, ok} = mnesia:create_table(connection, [{ram_copies, [node()]}, {attributes, record_info(fields, connection)}]),
    {atomic, ok} = mnesia:create_table(explored_map, [{ram_copies, [node()]}, {attributes, record_info(fields, explored_map)}]),  
    {atomic, ok} = mnesia:create_table(perception, [{ram_copies, [node()]}, {attributes, record_info(fields, perception)}]),  
    {atomic, ok} = mnesia:create_table(event, [{ram_copies, [node()]}, {attributes, record_info(fields, event)}]),    
    {atomic, ok} = mnesia:create_table(map, [{ram_copies, [node()]}, {attributes, record_info(fields, map)}]),    
    {atomic, ok} = mnesia:create_table(obj, [{ram_copies, [node()]}, {attributes, record_info(fields, obj)}]),    
    {atomic, ok} = mnesia:create_table(obj_attr, [{ram_copies, [node()]}, {attributes, record_info(fields, obj_attr)}]),    
    {atomic, ok} = mnesia:create_table(obj_def, [{ram_copies, [node()]}, {attributes, record_info(fields, obj_def)}]),    
    {atomic, ok} = mnesia:create_table(item, [{ram_copies, [node()]}, {attributes, record_info(fields, item)}]),    
    {atomic, ok} = mnesia:create_table(item_attr, [{ram_copies, [node()]}, {attributes, record_info(fields, item_attr)}]),    
    {atomic, ok} = mnesia:create_table(item_def, [{ram_copies, [node()]}, {attributes, record_info(fields, item_def)}]), 
    {atomic, ok} = mnesia:create_table(recipe, [{ram_copies, [node()]}, {attributes, record_info(fields, recipe)}]),    
    {atomic, ok} = mnesia:create_table(recipe_attr, [{ram_copies, [node()]}, {attributes, record_info(fields, recipe_attr)}]),    
    {atomic, ok} = mnesia:create_table(recipe_def, [{ram_copies, [node()]}, {attributes, record_info(fields, recipe_def)}]),    
    {atomic, ok} = mnesia:create_table(skill, [{ram_copies, [node()]}, {attributes, record_info(fields, skill)}]),    
    {atomic, ok} = mnesia:create_table(skill_attr, [{ram_copies, [node()]}, {attributes, record_info(fields, skill_attr)}]),    
    {atomic, ok} = mnesia:create_table(skill_def, [{ram_copies, [node()]}, {attributes, record_info(fields, skill_def)}]),    
    {atomic, ok} = mnesia:create_table(action, [{ram_copies, [node()]}, {attributes, record_info(fields, action)}]),    
    {atomic, ok} = mnesia:create_table(resource_def, [{ram_copies, [node()]}, {attributes, record_info(fields, resource_def)}]),
    {atomic, ok} = mnesia:create_table(poi_def, [{ram_copies, [node()]}, {attributes, record_info(fields, poi_def)}]),
    {atomic, ok} = mnesia:create_table(resource, [{type, bag}, {ram_copies, [node()]}, {attributes, record_info(fields, resource)}]),    
    {atomic, ok} = mnesia:create_table(test, [{ram_copies, [node()]}, {attributes, record_info(fields, test)}]),    
    {atomic, ok} = mnesia:create_table(hero, [{ram_copies, [node()]}, {attributes, record_info(fields, hero)}]),    
    {atomic, ok} = mnesia:create_table(villager, [{ram_copies, [node()]}, {attributes, record_info(fields, villager)}]),    
    {atomic, ok} = mnesia:create_table(htn, [{ram_copies, [node()]}, {attributes, record_info(fields, htn)}]),    
    {atomic, ok} = mnesia:create_table(npc, [{ram_copies, [node()]}, {attributes, record_info(fields, npc)}]),    
    {atomic, ok} = mnesia:create_table(state, [{ram_copies, [node()]}, {attributes, record_info(fields, state)}]),    
    {atomic, ok} = mnesia:create_table(effect, [{ram_copies, [node()]}, {attributes, record_info(fields, effect)}]),    
    {atomic, ok} = mnesia:create_table(world, [{ram_copies, [node()]}, {attributes, record_info(fields, world)}]),  
    {atomic, ok} = mnesia:create_table(encounter, [{ram_copies, [node()]}, {attributes, record_info(fields, encounter)}]),  
    {atomic, ok} = mnesia:create_table(revent, [{ram_copies, [node()]}, {attributes, record_info(fields, revent)}]),  
    {atomic, ok} = mnesia:create_table(combo, [{type, bag}, {ram_copies, [node()]}, {attributes, record_info(fields, combo)}]),  
    {atomic, ok} = mnesia:create_table(combo_def, [{ram_copies, [node()]}, {attributes, record_info(fields, combo_def)}]),  
    {atomic, ok} = mnesia:create_table(attack, [{ram_copies, [node()]}, {attributes, record_info(fields, attack)}]),  

    mnesia:add_table_index(player, name),
    mnesia:add_table_index(player, npc),
    mnesia:add_table_index(connection, socket),
    mnesia:add_table_index(event, tick),
    mnesia:add_table_index(event, source),
    mnesia:add_table_index(event, type),
    mnesia:add_table_index(obj, player),
    mnesia:add_table_index(obj, pos),
    mnesia:add_table_index(obj, state),
    mnesia:add_table_index(obj, class),
    mnesia:add_table_index(obj, subclass),
    mnesia:add_table_index(item, owner),
    mnesia:add_table_index(htn, parent),
    mnesia:add_table_index(effect, id),
    mnesia:add_table_index(villager, player),
    mnesia:add_table_index(villager, shelter),
    mnesia:add_table_index(villager, structure),
    mnesia:add_table_index(villager, storage),

    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([counter, player, connection, map, obj, explored_map, perception,
                            event, action, resource, world], 1000).

import(DefFileName) ->
    PrivDir = code:lib_dir(sp) ++ "/priv/",
    {ok, Bin} = file:read_file(PrivDir ++ DefFileName ++ ".json"),

    DefListOfMap = jsx:decode(Bin, [return_maps]),

    F = fun(DefMap) ->
            DefName = maps:get(<<"name">>, DefMap),
            DefList = maps:to_list(DefMap),
            import_entry(list_to_atom(DefFileName), DefName, DefList)
        end,

    lists:foreach(F, DefListOfMap).

import_entry(Table, ObjName, ObjList) ->
    F = fun({<<"_id">>, _}) -> nothing;
           ({Attr, Value}) ->       
            Key = {ObjName, Attr},
            R = {Table, Key, Value},
            db:dirty_write(R)
        end,

    lists:foreach(F, ObjList).

first(T) ->
    F = fun() -> mnesia:first(T) end,
    {atomic, Value} = mnesia:transaction(F),
    Value.

next(T, K) ->
    F = fun() -> mnesia:next(T, K) end,
    {atomic, Value} = mnesia:transaction(F),
    Value.

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

select(T, F_MS) ->
    F = fun() -> mnesia:select(T, F_MS) end,
    {atomic, Value} = mnesia:transaction(F),
    Value.

match_object(P) ->
    F = fun() -> mnesia:dirty_match_object(P) end,
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
     {connection, 98, none},     
     {connection, ?UNDEAD, none},     
     {connection, ?ANIMAL, none},     
     {player, 98, <<"natives">>, <<"123123">>, 0, false, true},
     {player, ?UNDEAD, <<"Undead">>, <<"123123">>, 0, false, true},
     {player, ?ANIMAL, <<"Animals">>, <<"123123">>, 0, false, true},
     {counter, player, ?NPC_ID},
     {world, time, day},
     %{revent, 1, <<"Silent Night">>, <<"The night passes without incident.">>, [<<"Ok.">>], [<<"Nothing happens.">>], [none]},
     %{revent, 1, <<"The Baying Hounds">>, <<"Haunting howls, from what you hope are dogs, surround your camp.">>, [<<"Post a watch.">>, <<"They'll leave us alone. Do nothing.">>], [<<"Lose 10% Morale.">>, <<"Potential Danger...">>], [{attrmod, {morale, -10}}, {random, [{45, spawn, <<"Wolf">>}]} ]}
     {revent, 1, <<"The Overgrown Tombstone">>, <<"You find an an ancient tombstone just outside your camp.  It appears to have been left undisturbed by both man and beast for a long time.">>, [<<"Leave it alone.">>, <<"Dig up the grave.">>, <<"Bless the site.">>], [<<"Nothing happens.">>, <<"Possible loot...">>, <<"Gain Morale 10%">>], [none, {random, [{75, loot}, {25, spawn, <<"Skeleton">>}]}, {attrmod, {morale, 10}}]},
     {combo_def, <<"Hamstring">>, ?QUICK, "qq"},
     {combo_def, <<"Gouge">>, ?QUICK, "pq"},
     {combo_def, <<"Imtimidating Shout">>, ?QUICK, "ff"},
     {combo_def, <<"Shrounded Slash">>, ?PRECISE, "pfq"},
     {combo_def, <<"Shatter Cleave">>, ?PRECISE, "qff"},
     {combo_def, <<"Massive Pummel">>, ?FIERCE, "qpfp"},
     {combo_def, <<"Nightmare Strike">>, ?FIERCE, "fpqf"}
    ].

reset_tables() ->

    F = fun() ->
            lists:foreach(fun mnesia:write/1, test_tables())
        end,    
    mnesia:transaction(F).    
