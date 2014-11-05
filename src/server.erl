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

    %Startup rec2json
    application:start(rec2json),

    % Create schema and load db data
    ?INFO("Creating schema and loading db data..."), 
    db:create_schema(),
    ok = db:start(),
    db:reset_tables(),

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
                                               {packet, 0},
                                               {reuseaddr, true}]),

    
    Client = #client{ server_pid = self() },
    log4erl:info("Server listening..."),
    do_accept(ListenSocket, Client).

%%
%% Local Functions
%%

do_accept(ListenSocket, Client) ->	
    case gen_tcp:accept(ListenSocket) of 
        {ok, Socket} ->
            log4erl:info("Socket accepted."),
            spawn(fun() -> do_accept(ListenSocket, Client) end),
            handle_client(Socket, Client);
        {error, closed} ->
            log4erl:info("Socket not accepted.")
    end.

handle_client(Socket, Client) ->
    receive
        {tcp, Socket, Bin} ->
            NewClient = handle_packet(Socket, Client, Bin),
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

handle_packet(Socket, Client, Bin) ->
    
    log4erl:info("Data received: ~p~n", [Bin]),
    PacketReturn = packet:read(Bin),

    NewClient = case catch PacketReturn of
                    {'EXIT', Error} ->
                        log4erl:info("Could not parse packet. Bin: ~p Error: ~p~n", [Bin, Error]),
                        Client;                    
                    #{<<"cmd">> := <<"login">>} ->                                
                        process_login(Client, Socket, PacketReturn);   
                    clientready ->
                        process_clientready(Client, Socket);
                    Event ->
                        process_event(Client, Socket, Event)                            
                end,

    NewClient.

process_login(Client, Socket, PacketReturn) ->

    User = maps:get(<<"user">>, PacketReturn),
    Pass = maps:get(<<"pass">>, PacketReturn),

    case login:login(User, Pass, self()) of
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

process_clientready(Client, _Socket) ->
    log4erl:info("server - process_clientready~n"),
    if
        Client#client.ready =/= true ->
            log4erl:info("server - client.ready =/= true~n"),
            %PlayerPID = Client#client.player_pid,    
            %log4erl:info("PlayerPID ~w~n", [PlayerPID]),			
            %PlayerId = gen_server:call(PlayerPID, 'ID'),   
 
            %gen_server:cast(PlayerPID, {'SEND_SETUP_INFO'}),        
            %gen_server:cast(global:whereis_name(game_pid), {'ADD_PLAYER', PlayerId, PlayerPID}),    
            
            NewClient = Client#client{ ready = true };
        true ->			
            log4erl:info("server - clientready failed as ready state was true"),
            NewClient = Client
    end,
    
    NewClient.

process_event(Client, Socket, Event) ->
    if
        Client#client.ready == true ->
            gen_server:cast(Client#client.player_pid, Event);
        true ->
            ?INFO("process_event client not ready"),
            ?INFO("client: ~w~n", Client),
            ?INFO("event: ~p~n", Event),
            ok = packet:send(Socket, #bad{ cmd = ?CMD_LOGIN, error = 99})
            %ok = gen_server:call(Client#client.player_pid, 'LOGOUT')
    end,
    Client.

