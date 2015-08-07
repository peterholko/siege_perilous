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

-record(resource, {index, 
                   name,
                   max,
                   quantity}).

-record(resource_def, {tile,
                       name,
                       quantity}).

-record(obj, {id, 
              pos,
              last_pos = none,
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
                source = none,
                tick}).

-record(perception, {player,
                     data}).

-record(battle_unit, {unit,
                      speed}).

-record(local_map, {index,
                    tile,
                    layers}).

-record(local_obj, {id,
                    global_obj_id,
                    global_pos,
                    pos,
                    player,
                    class,
                    subclass = none,
                    name,
                    state,
                    effect = [],
                    vision = false}).
                    
-record(charge_time, {unit_id,
                      charge_time}).

-record(action, {source_id,
                 type,
                 data}).

-record(villager, {id,
                   task,
                   dwelling}).

-record(npc, {id, 
              objective = wander}).

-record(test, {attr,
               value}).
