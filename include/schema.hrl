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

-record(resource, {resource_type,
                   tile_type}).

-record(map_obj, {id, 
                  pos,
                  player,
                  class,
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
                 tiles}).

-record(battle_obj, {battle,
                     player,
                     obj}).

-record(battle_unit, {unit,
                      pos,
                      player,
                      state,
                      speed,
                      battle}).

-record(battle_map, {type, 
                     tiles}).

-record(charge_time, {unit_id,
                      charge_time}).

-record(action, {source_id,
                 type,
                 data}).

