%% Author: Peter
%% Created: Dec, 2016
%% Description: Effect module
-module(effect).

-include("schema.hrl").
-include("common.hrl").

-export([add/3, remove/2, has_effect/2, get_effect_data/2, get_effects/1, all/1]).

add(Id, EffectType, EffectData) ->
    Effect = #effect {key = {Id, EffectType},
                      id = Id,
                      type = EffectType,
                      data = EffectData,
                      modtick = counter:value(tick)},
    db:write(Effect).

remove(Id, EffectType) ->
    db:delete(effect, {Id, EffectType}).

has_effect(Id, EffectTypeList) when is_list(EffectTypeList) ->
    F = fun(EffectType, Acc) ->
            has_effect(Id, EffectType) or Acc
        end,

    lists:foldl(F, false, EffectTypeList);

has_effect(Id, EffectType) ->
    Result = db:read(effect, {Id, EffectType}),
    length(Result) > 0.

get_effect_data(Id, EffectType) ->
    case db:read(effect, {Id, EffectType}) of
        [Effect] -> Effect#effect.data;
        [] -> invalid
    end.

get_effects(Id) ->
    Effects = db:index_read(effect, Id, #effect.id),
    
    F = fun(Effect, Acc) ->
            EffectMap = #{<<"name">> => Effect#effect.type},
            [EffectMap | Acc]
        end,

    lists:foldl(F, [], Effects).

all(Id) -> db:index_read(effect, Id, #effect.id).
