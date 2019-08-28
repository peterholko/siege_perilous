//import {startGame} from './game'
import {Global} from './global'
import { NetworkEvent } from './networkEvent';
import { ObjectState } from './objectState';
import { TileState } from './tileState';

export class Network {

  private websocket;

  public static sendPing() {
    Global.socket.sendMessage("0");
  }

  public static sendLogin(username: string, password: string) {
    var m = '{"cmd": "login", "username": "' + username + '", "password": "' + password + '"}';
    Global.socket.sendMessage(m);
  }

  public static sendMove(newX : integer, newY : integer) {
    console.log('')
    var m = '{"cmd": "move_unit", "id": ' + '7' + ', "x": ' + newX + ', "y": ' + newY + '}';
    Global.socket.sendMessage(m);
  }

  public static sendItemTransfer(targetId, item) {
    var m = '{"cmd": "item_transfer", "targetid": ' + targetId + ', "item": ' + item + '}';
    Global.socket.sendMessage(m);
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
    var m = '{"cmd": "info_item_transfer", "sourceid": ' + sourceId + ', "targetid": ' + targetId + '}';
    Global.socket.sendMessage(m);
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
    }

    this.websocket.onerror = (evt) => {
      console.log('Websocket Error...');
    }

    this.websocket.onmessage = (evt) => {
      var jsonData = JSON.parse(evt.data);

      //Check if error message is in the packet
      if(jsonData.hasOwnProperty('errmsg')) {
        console.log('Error received: ' + jsonData.errmsg);
        Global.gameEmitter.emit(NetworkEvent.ERROR, jsonData);
      } else if(jsonData.packet == "login") {
        console.log("Login successful")
        Global.playerId = jsonData.player;
        Global.gameEmitter.emit(NetworkEvent.LOGGED_IN, {});

      } else if(jsonData.packet == 'perception') {
        console.log('Received Perception');

        this.processTileStates(jsonData.data.map);
        this.processInitObjStates(jsonData.data.objs);

        Global.gameEmitter.emit(NetworkEvent.PERCEPTION, jsonData);
      } else if(jsonData.packet == 'changes') {
        console.log(jsonData);
        this.processUpdateObjStates(jsonData.events);
        Global.gameEmitter.emit(NetworkEvent.CHANGES, jsonData);
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
      } else if(jsonData.packet == "info_attrs") {
        Global.gameEmitter.emit(NetworkEvent.INFO_ATTRS, jsonData);
      } else if(jsonData.packet == "info_skills") {
        Global.gameEmitter.emit(NetworkEvent.INFO_SKILLS, jsonData);
      } else if(jsonData.packet == "structure_list") {
        Global.gameEmitter.emit(NetworkEvent.STRUCTURE_LIST, jsonData);
      } else if(jsonData.packet == 'build') {
        Global.gameEmitter.emit(NetworkEvent.BUILD, jsonData);
      } else if(jsonData.packet == 'dmg') {
        this.processDmg(jsonData);
        Global.gameEmitter.emit(NetworkEvent.DMG, jsonData);
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
        state: obj.state,
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
    console.log("processUpdateStates");
    console.log(events);

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
          Global.objectStates[obj.id].op = 'added';
          
      } else if(eventType == "obj_update") {
          var obj_id = events[i].obj_id;
          var attr = events[i].attr;
          var value = events[i].value;

          if(attr == 'state') {
              Global.objectStates[obj_id].state = value;
          }

          Global.objectStates[obj_id].op = 'updated';
      } else if(eventType == "obj_move") {
          var obj = events[i].obj;
          var src_x = events[i].src_x;
          var src_y = events[i].src_y;      

          if(obj.id in Global.objectStates) {
              Global.objectStates[obj.id].state = obj.state;
              Global.objectStates[obj.id].x = obj.x;
              Global.objectStates[obj.id].y = obj.y;
              Global.objectStates[obj.id].op = 'updated';
          } else {
              Global.objectStates[obj.id] = obj;
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
