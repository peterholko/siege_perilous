%% Author: Peter
%% Created: Jan 17, 2017
%% Description: Recipe
-module(relation).

-include("common.hrl").
-include("schema.hrl").

-export([is_ally/2]).

is_ally(Source, Target) when Source =:= Target ->
  true;
is_ally(Source, Target) ->
  case db:read(relation, {Source, Target}) of
    [] -> false;
    [Relation] -> Relation#relation.score > ?NEUTRAL
  end.

