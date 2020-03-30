import { Global } from './global';

export class Obj {

  constructor() {
  }

  static getObjsAt(hexX, hexY) : Array<integer> {
    var objsAt = []

    for(var objId in Global.objectStates) {
        var objectState = Global.objectStates[objId];

        if(objectState.x == hexX && objectState.y == hexY &&
           objectState.state != 'deleting') {
          objsAt.push(objId);
        }
    }

  return objsAt;
  }
}