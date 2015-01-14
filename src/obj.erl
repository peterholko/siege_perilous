%% Author: Peter
%% Created: Jan 15, 2015
%% Description: Handles access to unit data
-module(obj).

-include("schema.hrl").

-export([get_obj/1]).

get_obj(Id) ->
    BinId = util:hex_to_bin(binary_to_list(Id)),
    Cursor = mongo:find(mdb:get_conn(), <<"obj">>, {'_id', {BinId}}),
    [Obj] = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    Obj.

