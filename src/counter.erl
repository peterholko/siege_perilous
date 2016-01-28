%% Author: Peter
%% Created: Feb 15, 2009
%% Description: Implements simple persistent counter using mnesia
-module(counter).

-include("schema.hrl").

-export([increment/1, increment/2, reset/1, value/1]).

increment(Type) ->
    increment(Type, 1).

increment(Type, Value) ->
    mnesia:dirty_update_counter(counter, Type, Value).    

reset(Type) ->
    Counter = #counter{
                       type = Type,
                       value = 0
                      },
    ok = db:write(Counter).

value(Type) ->
    [{counter, _, V}] = mnesia:dirty_read(counter, Type),
    V.
