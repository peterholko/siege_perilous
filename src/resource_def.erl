%% Author: Peter
%% Created: Jan 15, 2017
%% Description: Wrapper for resource_def table
-module(resource_def).

-include("schema.hrl").

-export([list/0, terrain_list/0, all/1, all_to_map/1, select/2, value/2]).

list() -> 
    AllResourceDef = ets:tab2list(resource_def),

    F = fun(ResourceDef, Acc) ->
            {Name, _Attr} = ResourceDef#resource_def.key,
            [Name | Acc]
        end,

    util:unique_list(lists:foldl(F, [], AllResourceDef)).

terrain_list() ->
    ResourceList = list(),

    F = fun(Resource, TerrainMap) ->
            ResourceMap = all_to_map(Resource),
            TerrainList = maps:get(<<"terrain">>, ResourceMap),            
            
            update_terrain_map(Resource, TerrainList, TerrainMap)

        end,
    
    lists:foldl(F, #{}, ResourceList).

all(Name) ->
    db:dirty_match_object({resource_def, {Name, '_'}, '_'}).

all_to_map(Name) ->
    to_map(all(Name)).

select(Key, Val) ->
    ListOfMatches = db:dirty_match_object({resource_def, {'_', Key}, Val}),
    
    F = fun(ResourceDef, Acc) ->
            {Name, _} = ResourceDef#resource_def.key,
            All = all(Name),
            [to_map(All) | Acc]
        end,

    lists:foldl(F, [], ListOfMatches).

to_map(All) ->
    F = fun(ResourceDef, AllMap) ->
            {_Name, Attr} = ResourceDef#resource_def.key,
            Value = ResourceDef#resource_def.value,
            maps:put(Attr, Value, AllMap) 
        end,
    lists:foldl(F, #{}, All).

value(All, Attr) when is_list(All) ->
    [H | _Rest] = All,
    Key = H#resource_def.key,
    {Name, _} = Key,
    ResourceDef = lists:keyfind({Name, Attr}, 2, All),
    ResourceDef#resource_def.value;

value(Name, Attr) ->
    [ResourceDef] = db:dirty_read(resource_def, {Name, Attr}),
    ResourceDef#resource_def.value.

%%% Internal %%%

update_terrain_map(Resource, TerrainList, TerrainMap) ->

    F = fun(Terrain, Map) ->
            ResourceList = maps:get(Terrain, Map, []),
            NewResourceList = [Resource | ResourceList],
            maps:put(Terrain, NewResourceList, Map)
        end,

    lists:foldl(F, TerrainMap, TerrainList).
