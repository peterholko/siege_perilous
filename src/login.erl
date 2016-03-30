%% Author: Peter
%% Created: Dec 20, 2008
%% Description: TODO: Add description to login
-module(login).

%%
%% Include files
%%

-include("common.hrl").
-include("schema.hrl").

%%
%% Exported Functions
%%
-export([login/3]).

%% 
%% API Functions
%%

login(Name, Pass, Socket) 
  when is_binary(Name),
       is_binary(Pass),
       is_pid(Socket) -> % socket handler process
    
    PlayerInfo = db:index_read(player, Name, #player.name),
    login(PlayerInfo, [Name, Pass, Socket]).

%%
%% Local Functions
%%

login([], [Name, Pass, Socket]) ->
    PlayerId = counter:increment(player),
    
    Player = #player {id = PlayerId,
                      name = Name,
                      password = Pass},

    Connection = #connection {player = PlayerId,
                              process = Socket},

    ExploredMap = #explored_map {player = PlayerId,
                                 tiles = [],
                                 new_tiles = []}, 

    db:write(Player),
    db:write(Connection),
    db:write(ExploredMap),
   
    Pos = map:random_location(),
    AdjPos = map:get_random_neighbour(Pos),
 
    HeroId = obj:create(Pos, PlayerId, unit, <<"hero">>, <<"Hero Mage">>, none),
    VillagerId = obj:create(AdjPos, PlayerId, unit, <<"villager">>, <<"Human Villager">>, none),

    item:create(HeroId, <<"Crimson Root">>, 100),
    item:create(HeroId, <<"Cragroot Popular">>, 10),
    item:create(VillagerId, <<"Crimson Root">>, 100),

    map:add_explored(PlayerId, Pos, 2),

    {success, PlayerId};

login([PlayerInfo], [_Name, Pass,_] = Args)
  when is_record(PlayerInfo, player) ->
    PlayerId = PlayerInfo#player.id,
    PlayerConn = case db:read(connection, PlayerId) of
                     [P] ->
                         P;
                     _ ->
                         ok = db:delete(connection, PlayerId),
                         #connection{ player = PlayerId }
                 end,    

    %% replace dead ids with none
    PlayerConn1 = PlayerConn#connection {process = fix_pid(PlayerConn#connection.process)},

    %% check player state and login
    Condition = check_player(PlayerInfo, PlayerConn1, [Pass], 
                             [
                              fun is_account_disabled/3,
                              fun is_bad_password/3,
                              fun is_player_online/3,
                              fun is_offline/3
                             ]),    
    
    {Player2, PlayerInfo1, Result} = login(PlayerInfo, PlayerConn1, Condition, Args),
    case {db:write(Player2), db:write(PlayerInfo1)} of
        {ok, ok} ->
            Result;
        _ ->
            {error, ?ERR_UNKNOWN}
    end.  

login(PlayerInfo, PlayerConn, account_disabled, _) ->
    {PlayerInfo, PlayerConn, {error, ?ERR_ACCOUNT_DISABLED}};

login(PlayerInfo, PlayerConn, player_online, Args) ->
    %% player is already online
    lager:info("login: player is already online~n"),
    login(PlayerInfo, PlayerConn, player_offline, Args);

login(PlayerInfo, PlayerConn, bad_password, _) ->
    N = PlayerInfo#player.login_errors + 1,
    MaxLoginErrors = 10,
    if
        N > MaxLoginErrors ->
            %% disable account
            PlayerInfo1 = PlayerInfo#player { disabled = true },
            {PlayerInfo1, PlayerConn, {error, ?ERR_ACCOUNT_DISABLED}};
        true ->
            PlayerInfo1 = PlayerInfo#player { login_errors = N },
            {PlayerInfo1, PlayerConn, {error, ?ERR_BAD_LOGIN}}
    end;

login(PlayerInfo, PlayerConn, player_offline, [Name, _, Socket]) ->
    lager:info("Successful login of user ~p~n", [Name]),

    %% update player connection
    PlayerConn1 = PlayerConn#connection {player = PlayerInfo#player.id,
                                         process = Socket
                                        },
    {PlayerInfo, PlayerConn1, {success, PlayerInfo#player.id}}.

check_player(PlayerInfo, PlayerConn, Pass, [Guard|Rest]) ->
    case Guard(PlayerInfo, PlayerConn, Pass) of
        {true, Condition} ->
            Condition;
        _ ->
            check_player(PlayerInfo, PlayerConn, Pass, Rest)
    end;

check_player(_Info, _PlayerConn, _Args, []) ->
    %% fall through
    unknown_error.

is_account_disabled(PlayerInfo, _, _) ->
    {PlayerInfo#player.disabled, account_disabled}.

is_player_online(_, PlayerConn, _) ->
    SocketAlive = PlayerConn#connection.process /= none,
    lager:info("SocketAlive: ~p~n", [SocketAlive]),
    {SocketAlive, player_online}.

is_bad_password(PlayerInfo, _, [Pass]) ->
    Match = PlayerInfo#player.password =:= Pass,
    lager:info("login: is_bad_password - Match: ~w~n", [Match]),
    {not Match, bad_password}.

is_offline(_, PlayerConn, _) ->
    SocketDown = PlayerConn#connection.process =:= none,
    {SocketDown, player_offline}.    

fix_pid(none) ->
    none;

fix_pid(Pid)
  when is_pid(Pid) ->
    case util:is_process_alive(Pid) of
        true ->
            Pid;
        _ ->
            none
    end.
