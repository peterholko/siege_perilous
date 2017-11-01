-record(counter, {type, 
                  value}).

-record(player, {id,
                 name,
                 password,
                 login_errors = 0,
                 disabled = false,
                 hero = false,
                 npc = false}).

-record(connection, {player,
                     process = none}).


-record(chat, {obj_id,
               text}).

-record(game, {perception,
               explored}).

-record(world, {attr,
                value}).

-record(resource, {index, 
                   name,
                   max,
                   quantity,
                   obj = none,
                   revealed = false}).

-record(resource_def, {key,  %{name, attr}
                       value}).

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
                     player,
                     data}).


-record(obj_event, {id, 
                    pid,
                    event, 
                    data,
                    tick}).

-record(obj_create, {obj,
                     source_pos}).

-record(obj_update, {obj,
                     source_pos,
                     attr,
                     value}).

-record(obj_move, {obj,
                   source_pos,
                   dest_pos}).

-record(map, {index,
              tile,
              layers}).

-record(obj, {id,
              pos,
              player,
              name,
              template,
              class,
              subclass,
              state,
              effect = [],
              vision = -1,
              modtick}).

-record(obj_attr, {key, % {id, attr}
                   value}).

-record(obj_template, {key,  % {name, attr}
                       value}).

-record(item, {id,
               name,
               quantity,
               owner,
               class,
               subclass,
               weight,
               durability, 
               equip = <<"false">>}).

-record(item_attr, {key, % {id, attr}
                    value}).

-record(item_def, {key, % {name, attr}
                   value}).

-record(recipe, {id,
                 name}).

-record(recipe_attr, {key, % {id, attr}
                      value}).

-record(recipe_def, {key, % {name, attr}
                     value}).

-record(skill, {key, % {id, name}
                value}).

-record(skill_attr, {key, % {id, attr}
                     value}).

-record(skill_def, {key, % {name, attr}
                    value}).

-record(charge_time, {unit_id,
                      charge_time}).

-record(action, {source_id,
                 type,
                 data}).

-record(hero, {player,
               obj}).

-record(villager, {id,
                   player,
                   target = none,
                   order = none,
                   activity = none,
                   structure = none,
                   shelter = none,
                   storage = none,
                   morale = 60,
                   enemies = none,
                   dest = none,
                   plan = [],
                   plan_data = none,
                   new_plan = false,
                   task_state = none,
                   task_index = 0,
                   path = none
                   }).

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
              order = wander,
              order_data = none,
              plan = [],
              new_plan = false,
              task_state = none,
              task_index = 0,
              path = none,
              combo = [],
              attacks = []}).

-record(state, {id, 
                state,
                data,
                modtick}).

-record(effect, {key,
                 id,
                 type,
                 data,
                 modtick}).


-record(encounter, {pos, 
                    num,
                    modtick}).

-record(revent, {id,
                 title, 
                 text,
                 responses = [],
                 resolutions = [],
                 effects = []}).

-record(combo_def, {name, 
                    combo_type, 
                    attacks}).

-record(combo, {id, 
                name,
                combo_type,
                attacks}).

-record(attack, {id,
                 combo_type, 
                 types}).



-record(p_event, {observer,
                  event,
                  added,
                  removed,
                  updated}).
