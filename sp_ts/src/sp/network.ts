//import {startGame} from './game'
import {Global} from './global'
import { NetworkEvent } from './networkEvent';
import { ObjectState } from './objectState';
import { TileState } from './tileState';
import { GameEvent } from './gameEvent';

export class Network {

  private websocket;

  public static sendPing() {
    Global.socket.sendMessage("0");
  }

  public static sendLogin(username: string, password: string) {
    var m = '{"cmd": "login", "username": "' + username + '", "password": "' + password + '"}';
    Global.socket.sendMessage(m);
  }

  public static sendSelectedClass(className: string) {
    var m = {
      cmd: "select_class",
      classname: className 
    };

    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendImageDef(imageName : string) {
    var m = {
      cmd: 'image_def',
      name: imageName
    }
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendMove(newX : integer, newY : integer) {
    var m = {
      cmd: "move_unit",
      id: Global.heroId,
      x: newX,
      y: newY
    }
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendItemTransfer(targetId, item) {
    var m = '{"cmd": "item_transfer", "targetid": ' + targetId + ', "item": ' + item + '}';
    Global.socket.sendMessage(m);
  }

  public static sendItemSplit(item, quantity) {
    var m = {
      cmd: "item_split",
      item: item,
      quantity: quantity
    };
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendInfoObj(id) {
    var m = '{"cmd": "info_unit", "id": ' + id + '}';
    Global.socket.sendMessage(m);
  }

  public static sendInfoItem(id) {
    var m = '{"cmd": "info_item", "id": ' + id + '}';
    Global.socket.sendMessage(m);
  }

  public static sendInfoTile(x, y) {
    var m = '{"cmd": "info_tile", "x": ' + x + ', "y": ' + y + '}';
    Global.socket.sendMessage(m);
  }

  public static sendInfoInventory(id) {
    var m = '{"cmd": "info_inventory", "id": ' + id + '}';
    Global.socket.sendMessage(m);
  }

  public static sendInfoItemTransfer(sourceId, targetId) {
    var m = {
      cmd: 'info_item_transfer',
      sourceid: sourceId,
      targetid: targetId
    };
    Global.socket.sendMessage(JSON.stringify(m));
  }
  
  public static sendInfoExperiment(structureId) {
     var m = {
      cmd: 'info_experiment',
      structureid: structureId
    };
    
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendInfoAttrs(id) {
    var m = '{"cmd": "info_attrs", "id": ' + id + '}';
    Global.socket.sendMessage(m);
  }

  public static sendInfoSkills(id) {
    var m = '{"cmd": "info_skills", "id": ' + id + '}';
    Global.socket.sendMessage(m);
  }

  public static sendExpore(id) {
    var m = '{"cmd": "explore", "sourceid": ' + id + '}';  
    Global.socket.sendMessage(m);
  }

  public static sendFollow(id) {
    var m = {
      cmd: "order_follow",
      sourceid: id
    };
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendCreateFoundation(id, structureName) {
    var m = {
      cmd: "create_foundation",
      sourceid: id,
      structure: structureName
    }
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendBuild(id, structureid) {
    var m = {
      cmd: "build", 
      sourceid: id,
      structureid: structureid
    }
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendGetStructureList() {
    var m = {
      cmd: "structure_list"
    }
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendOrderGather(id, resourceType) {
    var m = {
      cmd: "order_gather",
      sourceid: id,
      restype: resourceType
    };
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendOrderCraft(structureId, recipe) {
    var m = {
      cmd: "order_craft",
      sourceid: structureId,
      recipe: recipe
    }
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendOrderRefine(structureId) {
    var m = {
      cmd: "order_refine",
      structureid: structureId
    }
    Global.socket.sendMessage(JSON.stringify(m));
  }
  
  public static sendGetStats(id) {
    var m = {
      cmd: "get_stats",
      id: id
    };
    Global.socket.sendMessage(JSON.stringify(m)); 
  }

  public static sendAttack(attackType, sourceId, targetId) {
    var m = {
      cmd: "attack",
      attacktype: attackType,
      sourceid: sourceId,
      targetid: targetId
    };
    Global.socket.sendMessage(JSON.stringify(m)); 
  }

  public static sendTick() {
    var m = {
      cmd: "tick"
    }
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendGetAssignList() {
    var m = {
      cmd: "assign_list"
    }
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendAssign(sourceId, targetId) {
    var m = {
      cmd: "assign",
      sourceid: sourceId,
      targetid: targetId,
    }
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendGetRecipeList(structureId) {
    var m = {
      cmd: "recipe_list",
      structureid: structureId
    }
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendBuyItem(itemId, quantity) {
    var m = {
      cmd: "buy_item",
      itemid: itemId,
      quantity: quantity
    }
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendSellItem(itemId, targetId, quantity) {
    var m = {
      cmd: "sell_item",
      itemid: itemId,
      targetid: targetId,
      quantity: quantity
    }
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendInfoHauling(sourceId) {
    var m = {
      cmd: "info_hauling",
      sourceid: sourceId
    }    
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendHire(sourceId, targetId) {
    var m = {
      cmd: "hire",
      sourceid: sourceId,
      targetid: targetId
    }    
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendSetExpItem(itemId) {
    var m = {
      cmd: "set_exp_item",
      itemid: itemId
    }    
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendSetExpResource(itemId) {
    var m = {
      cmd: "set_exp_resource",
      itemid: itemId
    }    
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendOrderExperiment(structureId) {
    var m = {
      cmd: "order_experiment",
      structureid: structureId
    }    
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendResetExperiment(structureId) {
     var m = {
      cmd: "reset_experiment",
      structureid: structureId
    }    
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendInfoExit(Key, Type) {
      var m = {
      cmd: "info_exit",
      key: Key,
      type: Type
    }    
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendEquip(itemId) {
      var m = {
      cmd: "equip",
      item: itemId
    }    
    Global.socket.sendMessage(JSON.stringify(m));
  }

  public static sendUnEquip(itemId) {
      var m = {
      cmd: "unequip",
      item: itemId
    }    
    Global.socket.sendMessage(JSON.stringify(m));
  }
 
  constructor() {
    var url : string = "ws://" + window.location.host + "/websocket";
    this.websocket = new WebSocket(url);

    this.websocket.onopen = (evt) => {
      console.log('Opened websocket');
      setInterval(function() {
        console.log('Sending Ping');
        Network.sendPing()
      }, 50000);
    };

    this.websocket.onclose = (evt) => {
      console.log('Websocket Closing...');
      Global.gameEmitter.emit(NetworkEvent.SERVER_OFFLINE);
    }

    this.websocket.onerror = (evt) => {
      console.log('Websocket Error...');
      Global.gameEmitter.emit(NetworkEvent.NETWORK_ERROR);
    }

    this.websocket.onmessage = (evt) => {
      var jsonData = JSON.parse(evt.data);

      //Check if error message is in the packet
      if(jsonData.hasOwnProperty('errmsg')) {
        console.log('Error received: ' + jsonData.errmsg);
        Global.gameEmitter.emit(NetworkEvent.ERROR, jsonData);
      } else if(jsonData.packet == "select_class") {
        Global.playerId = jsonData.player;
        Global.gameEmitter.emit(NetworkEvent.SELECT_CLASS, {});
      } else if(jsonData.packet == "info_select_class") {
        if(jsonData.result == "success") {
          console.log("Class selected, logging in")
          Global.gameEmitter.emit(NetworkEvent.LOGGED_IN, {});
        }
      } else if(jsonData.packet == "login") {
        console.log("Login successful")
        Global.playerId = jsonData.player;
        Global.gameEmitter.emit(NetworkEvent.LOGGED_IN, {});
      } else if(jsonData.packet == 'perception') {
        console.log('Received Perception');

        this.processTileStates(jsonData.data.map);
        this.processInitObjStates(jsonData.data.objs);

        //Add small delay to prevent perception event before Scenes are created.
        setTimeout(function() {console.log('Emitting perception event'); Global.gameEmitter.emit(NetworkEvent.PERCEPTION, jsonData);}, 3000);
      } else if(jsonData.packet == 'changes') {
        console.log("--- Changes Packet Received ---");
        console.log(jsonData);
        this.processUpdateObjStates(jsonData.events);
        Global.gameEmitter.emit(NetworkEvent.CHANGES, jsonData);
      } else if(jsonData.packet == 'hero_dead') {
        Global.gameEmitter.emit(NetworkEvent.HERO_DEAD, jsonData);
      } else if(jsonData.packet == 'map') {
        console.log(jsonData);
        this.processTileStates(jsonData.data);
        Global.gameEmitter.emit(NetworkEvent.MAP, jsonData);
      } else if(jsonData.packet == 'image_def') {
        Global.gameEmitter.emit(NetworkEvent.IMAGE_DEF, jsonData);
      } else if(jsonData.packet == 'stats') {
        this.processGetStats(jsonData.data);
        Global.gameEmitter.emit(NetworkEvent.STATS, {});
      } else if(jsonData.packet == "info_unit") {
        Global.gameEmitter.emit(NetworkEvent.INFO_OBJ, jsonData);
      } else if(jsonData.packet == "info_tile") {
        Global.gameEmitter.emit(NetworkEvent.INFO_TILE, jsonData);
      } else if(jsonData.packet == "info_item") {
        Global.gameEmitter.emit(NetworkEvent.INFO_ITEM, jsonData)
      } else if(jsonData.packet == "info_inventory") {
        Global.gameEmitter.emit(NetworkEvent.INFO_INVENTORY, jsonData);
      } else if(jsonData.packet == "info_item_transfer") {
        Global.gameEmitter.emit(NetworkEvent.INFO_ITEM_TRANSFER, jsonData);
      } else if(jsonData.packet == "item_transfer") {
        Global.gameEmitter.emit(NetworkEvent.ITEM_TRANSFER, jsonData);
      } else if(jsonData.packet == "item_split") {
        if(jsonData.result == 'success') {
          Network.sendInfoInventory(jsonData.owner);
        }
      } else if(jsonData.packet == "info_attrs") {
        Global.gameEmitter.emit(NetworkEvent.INFO_ATTRS, jsonData);
      } else if(jsonData.packet == "info_skills") {
        Global.gameEmitter.emit(NetworkEvent.INFO_SKILLS, jsonData);
      } else if(jsonData.packet == "info_experiment") {
        Global.gameEmitter.emit(NetworkEvent.INFO_EXPERIMENT, jsonData);
      } else if(jsonData.packet == "structure_list") {
        Global.gameEmitter.emit(NetworkEvent.STRUCTURE_LIST, jsonData);
      } else if(jsonData.packet == 'build') {
        Global.gameEmitter.emit(NetworkEvent.BUILD, jsonData);
      } else if(jsonData.packet == 'dmg') {
        this.processDmg(jsonData);
        Global.gameEmitter.emit(NetworkEvent.DMG, jsonData);
      } else if(jsonData.packet == 'speech') {
        Global.gameEmitter.emit(NetworkEvent.SPEECH, jsonData);
      } else if(jsonData.packet == 'assign_list') {
        Global.gameEmitter.emit(NetworkEvent.ASSIGN_LIST, jsonData);
      } else if(jsonData.packet == 'recipe_list') {
        Global.gameEmitter.emit(NetworkEvent.RECIPE_LIST, jsonData);
      } else if(jsonData.packet == 'buy_item') {
        Global.gameEmitter.emit(NetworkEvent.BUYSELL_ITEM, jsonData);
      } else if(jsonData.packet == 'sell_item') {
        Global.gameEmitter.emit(NetworkEvent.BUYSELL_ITEM, jsonData);
      } else if(jsonData.packet == 'info_hauling') {
        Global.gameEmitter.emit(NetworkEvent.INFO_HAULING, jsonData);
      } else if(jsonData.packet == 'set_exp_item') {
        Global.gameEmitter.emit(NetworkEvent.INFO_EXPERIMENT, jsonData);
      } else if(jsonData.packet == 'set_exp_resource') {
        Global.gameEmitter.emit(NetworkEvent.INFO_EXPERIMENT, jsonData);
      } 
    }
  }

  processInitObjStates(objs) {
    for(var index in objs) {
      var obj = objs[index];
      var objectState : ObjectState = {
        id: obj.id,
        player: obj.player,
        name: obj.name,
        class: obj.class,
        subclass: obj.subclass,
        template: obj.template,
        groups: obj.groups,
        state: obj.state,
        prevstate: obj.state,
        x: obj.x,
        y: obj.y,
        vision: obj.vision,
        image: obj.image,
        op: 'added'
      };

      if(objectState.subclass == 'hero') {
        Global.heroId = objectState.id;
        Network.sendGetStats(Global.heroId);
      }

      Global.objectStates[objectState.id] = objectState; 
    }
  }

  processTileStates(tiles) {
    for(var index in tiles) {
      var tile = tiles[index];
      var tileState : TileState = {
        index: tile.x + '_' + tile.y,
        hexX: tile.x,
        hexY: tile.y,
        tiles: tile.t
      };

      Global.tileStates[tileState.index] = tileState;
    }
  }

  processUpdateObjStates(events) {
    //Reset the operation
    for(var objectId in Global.objectStates) {
        var objectState = Global.objectStates[objectId] as ObjectState;
        objectState.op = 'none';
    }

    for(var i = 0; i < events.length; i++) {
      var eventType = events[i].event;

      if(eventType == "obj_create") {
          var obj = events[i].obj;

          Global.objectStates[obj.id] = obj;
          Global.objectStates[obj.id].prevstate = obj.state;
          Global.objectStates[obj.id].op = 'added';
          
      } else if(eventType == "obj_update") {
          var obj_id = events[i].obj_id;
          var attr = events[i].attr;
          var value = events[i].value;

          if(attr == 'state') {
              Global.objectStates[obj_id].prevstate = Global.objectStates[obj_id].state;
              Global.objectStates[obj_id].state = value;
          }

          Global.objectStates[obj_id].op = 'updated';
          console.log("Emitting obj update: " + obj_id);
          Global.gameEmitter.emit(GameEvent.OBJ_UPDATE, obj_id);
      } else if(eventType == "obj_move") {
          var obj = events[i].obj;
          var src_x = events[i].src_x;
          var src_y = events[i].src_y;      

          if(obj.id in Global.objectStates) {
              Global.objectStates[obj.id].prevstate = Global.objectStates[obj.id].state;
              Global.objectStates[obj.id].state = obj.state;
              Global.objectStates[obj.id].x = obj.x;
              Global.objectStates[obj.id].y = obj.y;
              Global.objectStates[obj.id].op = 'updated';
          } else {
              Global.objectStates[obj.id] = obj;
              Global.objectStates[obj.id].prevstate = obj.state;
              Global.objectStates[obj.id].eventType = 'obj_move';
              Global.objectStates[obj.id].prevX = src_x;
              Global.objectStates[obj.id].prevY = src_y;
              Global.objectStates[obj.id].op = 'added';
          }            
      } else if(eventType =="obj_delete") {            
          var obj_id = events[i].obj_id;

          Global.objectStates[obj_id].op = 'deleted';
      } 
    }
  }

  processGetStats(data) {
    console.log(data);

    Global.heroHp = data.hp;
    Global.heroMaxHp = data.base_hp;
    Global.heroSta = data.stamina;
    Global.heroMaxSta = data.base_stamina;
  }

  public sendMessage(message : String) {
    this.websocket.send(message);
  }

  processDmg(data) {
    if(data.targetid == Global.heroId) {

      if(data.dmg > Global.heroHp) {
        Global.heroHp = 0;
      } else {
        Global.heroHp -= data.dmg;
      }

      Global.gameEmitter.emit(NetworkEvent.STATS, {});
   }
  }
}
