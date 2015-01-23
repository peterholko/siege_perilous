-record(counter, {type, 
                  value}).

-record(player, {id,
                 name,
                 password,
                 login_errors = 0,
                 disabled = false,
                 npc = false}).

-record(connection, {player,
                     process = none}).

-record(tile, {pos,
               type}).

-record(map_obj, {id, 
                  pos,
                  player,
                  type,
                  state}).

-record(explored_map, {player,
                       tiles}).

-record(event, {id, 
                player_process,
                type,
                data,
                tick}).

-record(perception, {player,
                     data}).

-record(battle, {id,
                 player,
                 obj}).

-record(battle_unit, {unit_id,
                      speed,
                      battle}).

-record(charge_time, {unit_id,
                      charge_time}).

-record(action, {source_id,
                 type,
                 data}).


