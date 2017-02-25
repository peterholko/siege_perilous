-module(sp_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1]).
-export([item_value_test/1,
                  combo_test/1]).

all() ->
    [item_value_test,
     combo_test].

init_per_suite(Config) ->
    setup:start(),

    Config.

item_value_test(_Config) ->
    ?assertEqual(5, 5).

combo_test(_Config) ->
    Weight = item_def:value(<<"Salarian Wheat">>, <<"weight">>),
    ?assertEqual(1, Weight).
