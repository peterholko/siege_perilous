%% Author: Peter
%% Created: Dec 24, 2008
%% Description: TODO: Add description to util
-module(util).

%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").

%%
%% Exported Functions
%%
-export([round3/1,
         ceiling/1,
         floor/1,
         diff_game_days/2,
         get_time/0,
         get_time_seconds/0,
         unique_list/1,
         replace/3,
         is_process_alive/1,
         get_app/1
        ]).

%%
%% API Functions
%%

round3(Num) ->
    RoundedNum = round(Num * 1000),
    RoundedNum / 1000.    

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

unique_list(L) ->
    T = ets:new(temp,[set]),
    L1 = lists:filter(fun(X) -> ets:insert_new(T, {X,1}) end, L),
    ets:delete(T),
    L1.

replace(1, [_|Rest], New) -> [New|Rest];
replace(I, [E|Rest], New) -> [E|replace(I-1, Rest, New)].

is_process_alive(Pid) 
  when is_pid(Pid) ->
    rpc:call(node(Pid), erlang, is_process_alive, [Pid]).

get_time() ->
    {Megasec, Sec, Microsec} = erlang:now(),
    Milliseconds = (Megasec * 1000000000) + (Sec * 1000) + (Microsec div 1000),
    Milliseconds.

get_time_seconds() ->
    {Megasec, Sec, _Microsec} = erlang:now(),
    Seconds = (Megasec * 1000000) + Sec,
    Seconds.

diff_game_days(StartTime, EndTime) ->
    Diff = EndTime - StartTime,
    NumGameDays = Diff / (3600 * ?GAME_NUM_HOURS_PER_DAY),
    NumGameDays.

get_app(Module) ->
    {ok, App} = application:get_application(Module),
    App.
