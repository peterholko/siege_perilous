%% Author: Peter
%% Created: Feb 15, 2009
%% Description: Implements simple persistent counter using mnesia
-module(obj_def).

-include("schema.hrl").

-export([all/1, value/2]).

all(Name) ->
    db:dirty_match_object({obj_def, {Name, '_'}, '_'}).

value(All, Attr) when is_list(All) ->
    [H | _Rest] = All,
    Key = H#obj_def.key,
    {Name, _} = Key,
    ObjDef = lists:keyfind({Name, Attr}, 2, All),
    ObjDef#obj_def.value;

value(Name, Attr) ->
    [ObjDef] = db:dirty_read(obj_def, {Name, Attr}),
    ObjDef#obj_def.value.

