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
-export([login/3, remove/1]).

%% 
%% API Functions
%%

login(Name, Pass, Socket) 
  when is_binary(Name),
       is_binary(Pass),
       is_pid(Socket) -> % socket handler process
    
    Player = db:index_read(player, Name, #player.name),
    process_login(Player, [Name, Pass, Socket]).

remove(PlayerId) ->
    db:delete(player, PlayerId),
    db:delete(connection, PlayerId),
    db:delete(explored_map, PlayerId).

%%
%% Local Functions
%%

process_login([], [Name, Pass, Socket]) ->
    lager:info("Creating init new player..."),
    PlayerId = counter:increment(player),
    
    Player = #player {id = PlayerId,
                      name = Name,
                      password = Pass},

    Connection = #connection {player = PlayerId,
                              status = init,
                              process = Socket},

    ExploredMap = #explored_map {player = PlayerId,
                                 tiles = [],
                                 new_tiles = []}, 

    db:write(Player),
    db:write(Connection),
    db:write(ExploredMap),
   
    {first_login, PlayerId};

process_login([Player], [_Name, Pass,_] = Args) ->
    lager:info("Found player - checking player state and connection"),
    PlayerId = Player#player.id,

    PlayerConn = case db:read(connection, PlayerId) of
                    [Conn] -> Conn;
                    _ -> #connection{ player = PlayerId }
                 end,    

    %% replace dead ids with none
    NewPlayerConn = PlayerConn#connection {process = fix_pid(PlayerConn#connection.process)},

    %% check player state and login
    Condition = check_player(Player, NewPlayerConn, [Pass], 
                             [
                              fun is_class_not_selected/3,
                              fun is_account_disabled/3,
                              fun is_bad_password/3,
                              fun is_player_online/3,
                              fun is_offline/3
                             ]),    
    
    {NewPlayer, NewPlayerConn2, Result} = login(Player, NewPlayerConn, Condition, Args),

    db:write(NewPlayer),
    db:write(NewPlayerConn2),

    Result.

login(PlayerInfo, PlayerConn, class_not_selected, _) ->
    {PlayerInfo, PlayerConn, {select_class, PlayerInfo#player.id}};

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
    {PlayerInfo, PlayerConn1, {relogin, PlayerInfo#player.id}}.

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

is_class_not_selected(PlayerInfo, _, _) ->
    lager:info("Class Selected Player: ~p", [PlayerInfo]),
    {PlayerInfo#player.class =:= none, class_not_selected}.

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
