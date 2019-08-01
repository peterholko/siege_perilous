import {startGame} from './game'
import {Global} from './global'
import { NetworkEvent } from './networkEvent';
import { ObjectState } from './objectState';
import { TileState } from './tileState';

export class Network {

  private websocket;

  public static sendMove(newX : integer, newY : integer) {
    console.log('')
    var m = '{"cmd": "move_unit", "id": ' + '7' + ', "x": ' + newX + ', "y": ' + newY + '}';
    Global.socket.sendMessage(m);
  }

  public static sendInfoUnit(id) {
    var m = '{"cmd": "info_unit", "id": ' + id + '}';
    Global.socket.sendMessage(m);
  }

  public static sendInfoTile(id) {
    var m = '{"cmd": "info_unit", "id": ' + id + '}';
    Global.socket.sendMessage(m);
  }

  public static sendExpore(id) {
    var m = '{"cmd": "explore", "sourceid": ' + id + '}';  
    Global.socket.sendMessage(m);
  }

  public static sendGetStructureList(id) {
    var m = '{"cmd": "structure_list"}';
    Global.socket.sendMessage(m);
  }

  constructor() {
    var url : string = "ws://" + window.location.host + "/websocket";
    this.websocket = new WebSocket(url);

    this.websocket.onopen = (evt) => {
      console.log('Opened websocket');
    };

    this.websocket.onmessage = (evt) => {
      var jsonData = JSON.parse(evt.data);

      if(jsonData.packet == "login") {
        var login = (<HTMLInputElement>document.getElementById("login"));
        var logo = (<HTMLInputElement>document.getElementById("logo"));

        login.style.display = "none";
        logo.style.display = "none";

        startGame();

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
      }
    };
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
        hexX: obj.x,
        hexY: obj.y,
        vision: obj.vision,
        image: obj.image,
        op: 'added'
      };

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
              Global.objectStates[obj.id].hexX = obj.x;
              Global.objectStates[obj.id].hexY = obj.y;
              Global.objectStates[obj.id].op = 'updated';
          } else {
              Global.objectStates[obj.id] = obj;
              Global.objectStates[obj.id].eventType = 'obj_move';
              Global.objectStates[obj.id].prevHexX = src_x;
              Global.objectStates[obj.id].prevHexY = src_y;
              Global.objectStates[obj.id].op = 'added';
          }            
      } else if(eventType =="obj_delete") {            
          var obj_id = events[i].obj_id;

          Global.objectStates[obj_id].op = 'deleted';
      } 
    }
  }

  public sendMessage(message : String) {
    this.websocket.send(message);
  }
}
