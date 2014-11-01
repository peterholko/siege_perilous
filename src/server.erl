%% Author: Peter
%% Created: Oct 30, 2014
%% Description: Server module
-module(server).

%%
%% Include files
%%
-include("packet.hrl").

%%
%% Records
%%

-record(client, {
                 server_pid = none,
                 player_pid = none,
                 clock_sync = none,
                 ready = none
                }).

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%

start() ->
    spawn(fun() -> init() end).    

init() ->
    
    % Start up log4erl
    application:start(log4erl),
    log4erl:conf("conf/log4erl.conf"),
    log4erl:change_log_level(info),

    %Startup jiffy
    application:start(jiffy),

    % Create schema and load db data
    ?INFO("Creating schema and loading db data..."), 
    %db:create_schema(),
    %ok = db:start(),
    %db:reset_tables(),
    %db:reset_game_tables(),

    % Load game data
    %data:load(),
    
    % Load start chat
    %chat:start(),

    % Load map data
    %log4erl:info("Loading map data..."), 
    %map:start(),
    %map:load(),
 
    % Start managers
    %kingdom:start(),
    %improvement:start(),    
    %map_object:start(),
    %population:start(),
    %unit:start(),   
    %item:start(),
    %market:start(),    
    %city_manager:start(),
    %army_manager:start(),
    %trigger:start(),   
 
    % Create game loop
    %log4erl:info("Starting game process...") ,
    %{ok, _GamePid} = game:start(),
    %ok = game:load_entities(),	
    %ok = game:setup_perception(),	 
    %ok = game:setup_events(),  
    %spawn(fun() -> game_loop:loop(util:get_time(), global:whereis_name(game_pid)) end),
    
   % Start socket listener
    {ok, ListenSocket} = gen_tcp:listen(2345, [binary,
                                               {active, once},
                                               {keepalive, true},
                                               {nodelay, true},
                                               {packet, 0}]),

    
    Client = #client{ server_pid = self() },
    log4erl:info("Server listening..."),
    do_accept(ListenSocket, Client).

%%
%% Local Functions
%%

do_accept(ListenSocket, Client) ->	
    case gen_tcp:accept(ListenSocket) of 
        {ok, Socket} ->
            log4erl:debug("Socket accepted."),
            spawn(fun() -> do_accept(ListenSocket, Client) end),
            handle_client(Socket, Client);
        {error, closed} ->
            log4erl:error("Socket not accepted.")
    end.

handle_client(Socket, Client) ->
    receive
        {tcp, Socket, Bin} ->
            
            log4erl:info("Status: Data accepted: ~w", [Bin]),
            NewClient = case catch packet:read(Bin) of
                            {'EXIT', Error} ->
                                log4erl:error("Could not parse packet"),
                                error_logger:error_report(
                                  [{module, ?MODULE},
                                   {line, ?LINE},
                                   {message, "Could not parse command"},
                                   {Bin, Bin},
                                   {error, Error},
                                   {now, now()}]),
                                Client;                    
                            #login{ name = Name, pass = Pass} ->
                                process_login(Client, Socket, Name, Pass);   
                            #logout{} ->
                                process_logout(Client, Socket);                        
                            #chat_message{player_id = PlayerId, player_name = PlayerName, message = Message} ->
                                process_chat(Client, PlayerId, PlayerName, Message);
                            clocksync ->
                                process_clocksync(Client, Socket);    
                            clientready ->
                                process_clientready(Client, Socket);
                            Event ->
                                process_event(Client, Socket, Event)                            
                        end,
            inet:setopts(Socket,[{active, once}]),
            handle_client(Socket, NewClient);
        
        {tcp_closed, Socket} ->
            log4erl:info("tcp_closed - player_pid: ~w", [Client#client.player_pid]),
            gen_server:call(Client#client.player_pid, 'LOGOUT'),
            handle_client(Socket, Client);

        {tcp_error, Socket, Reason} ->
            log4erl:info("tcp_error: ~w", [Reason]),
            handle_client(Socket, Client);
                
        {packet, Packet} ->
            ok = packet:send(Socket, Packet),
            handle_client(Socket, Client)   
    
    end.

process_login(Client, Socket, Name, Pass) ->
    case login:login(Name, Pass, self()) of
        {error, Error} ->
            ok = packet:send(Socket, #bad{ cmd = ?CMD_LOGIN, error = Error}),
            Client;
        {ok, PlayerPID} ->
            log4erl:info("process_login - ok"),
            PlayerId = gen_server:call(PlayerPID, 'ID'),
            log4erl:info("process_login - PlayerPID -> ~w", [PlayerPID]),
            ok = packet:send(Socket, #player_id{ id = PlayerId }),
            
            Client#client{ player_pid = PlayerPID }
    end.	

process_logout(Client, _Socket) ->
    ?INFO("process_logout"),
    ok = gen_server:call(Client#client.player_pid, 'LOGOUT'),
    Client.

process_chat(Client, PlayerId, PlayerName, Message) ->
    ?INFO("process_chat"),
    chat:broadcast(PlayerId, PlayerName, Message),
    Client. 

process_clocksync(Client, Socket) ->
    log4erl:debug("server: process_clocksync~n"),
    ok = packet:send_clocksync(Socket),
    Client.

process_clientready(Client, _Socket) ->
    log4erl:debug("server - process_clientready~n"),
    if
        Client#client.ready =/= true ->
            log4erl:debug("server - client.ready =/= true~n"),
            %PlayerPID = Client#client.player_pid,    
            %log4erl:debug("PlayerPID ~w~n", [PlayerPID]),			
            %PlayerId = gen_server:call(PlayerPID, 'ID'),   
 
            %gen_server:cast(PlayerPID, {'SEND_SETUP_INFO'}),        
            %gen_server:cast(global:whereis_name(game_pid), {'ADD_PLAYER', PlayerId, PlayerPID}),    
            
            NewClient = Client#client{ ready = true };
        true ->			
            log4erl:debug("server - clientready failed as ready state was true"),
            NewClient = Client
    end,
    
    NewClient.

process_event(Client, _Socket, Event) ->
    if
        Client#client.ready == true ->
            gen_server:cast(Client#client.player_pid, Event);
        true ->
            ?INFO("process_event client not ready"),
            ?INFO("client: ~w", Client)
            %ok = gen_server:call(Client#client.player_pid, 'LOGOUT')
    end,
    Client.

