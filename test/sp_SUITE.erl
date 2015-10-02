-module(sp_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0]).
-export([item_value_test/1,
         combo_test/1]).

all() ->
    [item_value_test,
     combo_test].

item_value_test(_Config) ->
    Items = [{'_id',{<<86,10,243,34,15,192,228,107,85,0,0,6>>},damage,5,equip,<<"true">>,
             name,<<"Stronghold Dagger">>, owner, {<<84,219,228,71,15,76,42,231,31,114,99,13>>},
             quantity,1}],
    ?assertEqual(5, combat:get_item_value(damage, Items)),
    ?assertEqual(0, combat:get_item_value(armor, Items)).

combo_test(_Config) ->
    Id = {<<84,219,228,71,15,76,42,231,31,114,99,13>>},

    ?assertEqual({none, 0}, combat:check_combos(weak, Id)).
