import { Global } from './global';

export class Obj {

  constructor() {
  }

  static getObjsAt(hexX, hexY) : Array<integer> {
    var objsAt = []

    console.log("getObjsAt: " + JSON.stringify(Global.objectStates));
    for(var objId in Global.objectStates) {
        var objectState = {...Global.objectStates[objId]};

        if(objectState.x == hexX && objectState.y == hexY &&
           objectState.op != 'deleted') {
          objsAt.push(objId);
        }
    }
  console.log("getObjsAt: " + objsAt);

  return objsAt;
  }
}