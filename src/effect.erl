%% Author: Peter
%% Created: Dec, 2016
%% Description: Effect module
-module(effect).

-include("schema.hrl").
-include("common.hrl").

-export([add/2, add/3, add/4, add/5, remove/2, has_effect/2, get_effect_data/2, get_effects/1, all/1]).

add(Id, EffectType) ->
    add(Id, EffectType, none, -1, -1).

add(Id, EffectType, EffectData) ->    
    add(Id, EffectType, EffectData, -1, -1).

add(Id, EffectType, EffectData, Expiry) ->    
    add(Id, EffectType, EffectData, Expiry, -1).

add(Id, EffectType, EffectData, TicksUntilExpiry, Interval) ->
    CurrentTick = counter:value(tick),

    ExpiryTick = case TicksUntilExpiry =:= -1 of
                     true -> -1;
                     false -> TicksUntilExpiry + CurrentTick
                 end,

    NextTick = case Interval =:= -1 of
                   true -> -1;
                   false -> Interval + CurrentTick
               end,

    lager:info("Added Expiry ~p ~p ~p", [EffectType, ExpiryTick, CurrentTick]),

    Effect = #effect {key = {Id, EffectType},
                      id = Id,
                      type = EffectType,
                      data = EffectData,
                      expiry = ExpiryTick,
                      next_tick = NextTick},
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
