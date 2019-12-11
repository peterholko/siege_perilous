-record(image_def, {name,
                    data}).

-record(counter, {type, 
                  value}).

-record(player, {id,
                 name,
                 password,
                 login_errors = 0,
                 disabled = false,
                 hero = false,
                 class = none,
                 npc = false,
                 data = #{}}).

-record(connection, {player,
                     status = init,
                     process = none}).

-record(chat, {obj_id,
               text}).

-record(game, {perception,
               explored}).

-record(game_attr, {key, 
                    value}).

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
                     data,
                     diff = []}).


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

-record(obj_delete, {obj,
                     source_pos}).

-record(obj_hide, {obj}).

-record(obj_reveal, {obj}).

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
              hp = 0,
              stamina = 0,
              mana = 0,
              thirst = 0,
              hunger = 0,
              focus = 0,
              stress = 0,
              groups = [],
              vision = -1,
              image,
              hsl = [],
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
               image,
               weight,
               durability, 
               equip = <<"false">>}).

-record(item_attr, {key, % {id, attr}
                    value}).

-record(item_template, {key, % {name, attr}
                        value}).

-record(recipe, {id,
                 owner,
                 owner_structure, % {owner, structure}
                 name}).

-record(recipe_attr, {key, % {id, attr}
                      value}).

-record(recipe_template, {key, % {name, attr}
                     value}).

-record(experiment, {structure,
                     recipe = none,
                     state,
                     exp_item = none,
                     req = []}).

-record(skill, {key, % {id, name}
                name,
                level = 0,
                xp}).

-record(skill_attr, {key, % {id, attr}
                     value}).

-record(skill_template, {key, % {name, attr}
                         value}).

-record(skill_mod, {key, % {skill, level}
                    
                    }).

-record(charge_time, {unit_id,
                      charge_time}).

-record(action, {source_id,
                 type,
                 data}).

-record(hero, {player,
               obj}).

-record(villager, {id,                   
                   player,
                   behavior = none,
                   order = none,
                   enemies = [],
                   target = none,                   
                   dest = none,
                   path = [],
                   plan = [],
                   last_plan = 0,
                   last_run = 0,
                   task_state = none,
                   task_index = 0,
                   structure = none,
                   shelter = none,
                   storage = none,
                   morale = none,
                   activity = <<"none">>,
                   data = #{}
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

-record(state, {id, 
                state,
                data,
                modtick}).

-record(effect, {key,
                 id,
                 type,
                 data = #{},
                 expiry = -1,
                 next_tick = -1}).


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

-record(npc, {id,              
              player, 
              order,
              target,
              dest = none,
              plan = [],
              task_state = none,
              task_index = 0,
              path = none,
              last_plan = 0,
              last_run = 0, 
              phase = 1,
              attacks = [],
              combo = [],
              data = #{}}).

-record(active_info, {index, %{player, type, id}
                      player,
                      id}).

-record(relation, {key, % {source, target}
                   score}).
