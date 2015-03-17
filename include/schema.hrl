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

-record(global_map, {pos,
                     tile}).

-record(resource, {resource_type,
                   tile_type}).

-record(obj, {id, 
              pos,
              last_pos,
              player,
              class,
              type,
              state}).

-record(explored_map, {player,
                       tiles,
                       new_tiles}).

-record(event, {id, 
                player_process,
                type,
                data,
                tick}).

-record(perception, {player,
                     data}).

-record(battle_unit, {unit,
                      speed}).

-record(local_map, {index,
                    tile = 0,
                    misc}).

-record(local_obj, {id,
                    global_obj_id,
                    global_pos,
                    pos,
                    player,
                    class,
                    type,
                    state}).
                    
-record(charge_time, {unit_id,
                      charge_time}).

-record(action, {source_id,
                 type,
                 data}).
