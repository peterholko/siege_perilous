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
                      obj,
                      pos,
                      type,
                      state,
                      speed,
                      battle}).

-record(local_map, {index,
                    tile = 0,
                    misc}).

-record(local_obj, {global_pos,
                    global_obj_id,
                    id,
                    pos,
                    player,
                    class,
                    type,
                    state}).
                    
-record(charge_time, {unit_id,
                      charge_time}).

-record(action, {source_id,
                 type,
                 data,
                 battle}).

