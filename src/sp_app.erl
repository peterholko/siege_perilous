-module(sp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
      {'_', [
        {"/", cowboy_static, {priv_file, sp, "index.html"}},
        {"/websocket", sp_handler, []}
      ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 10100}],
        [{env, [{dispatch, Dispatch}]}]),
      sp_sup:start_link().

stop(_State) ->
    ok.
