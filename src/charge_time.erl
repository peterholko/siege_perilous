%% Author: Peter
%% Created: Feb 15, 2009
%% Description: Implements simple persistent counter using mnesia
-module(charge_time).

-include("schema.hrl").

-export([increment/2, reset/1]).

increment(UnitId, Value) ->
    mnesia:dirty_update_counter(charge_time, UnitId, Value).    

reset(UnitId) ->
    ChargeTime = #charge_time { unit_id = UnitId,
                                charge_time = 0},
    ok = db:write(ChargeTime).
