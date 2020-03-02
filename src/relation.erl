%% Author: Peter
%% Created: Jan 17, 2017
%% Description: Recipe
-module(relation).

-include("common.hrl").
-include("schema.hrl").

-export([is_ally/2]).


is_ally(#obj{player = SourcePlayer}, #obj{player = TargetPlayer}) 
  when SourcePlayer =:= TargetPlayer ->
    true;
is_ally(#obj{player = SourcePlayer}, #obj{player = TargetPlayer}) ->
  case db:read(relation, {SourcePlayer, TargetPlayer}) of
    [] -> false;
    [Relation] -> Relation#relation.score > ?NEUTRAL
  end.

