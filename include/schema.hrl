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

-record(game, {perception,
               explored}).

-record(world, {attr,
                value}).

-record(resource, {index, 
                   name,
                   max,
                   quantity,
                   obj = none}).

-record(resource_def, {tile,
                       name,
                       quantity}).

-record(poi_def, {tile,
                  name}).

-record(explored_map, {player,
                       tiles,
                       new_tiles}).

-record(event, {id, 
                pid,
                type,
                data,
                source = none,
                tick,
                class}).

-record(perception, {entity,
                     data}).

-record(map, {index,
              tile,
              layers}).

-record(obj, {id,
              pos,
              player,
              class,
              subclass = none,
              name,
              state,
              effect = [],
              vision = -1,
              modtick}).
            
-record(charge_time, {unit_id,
                      charge_time}).

-record(action, {source_id,
                 type,
                 data}).

-record(villager, {id,
                   task,
                   dwelling}).

-record(test, {attr,
               value}).

-record(htn, {label,
              index,
              parent,
              conditions = [],
              effects = [],
              type,
              task = none}).

-record(npc, {id,
              target = none,
              orders = wander,
              orders_data = none,
              plan = [],
              new_plan = false,
              task_state = none,
              task_index = 0,
              path = none,
              attacks = []}).

-record(effect, {key,
                 id,
                 type,
                 data}).

-record(combat, {id, 
                 attacks}).
