import {startGame} from './game'
import {GlobalVars} from './globalvars'

export class Network {

  private websocket;

  public static sendMove(newX : integer, newY : integer) {
    console.log('')
    var move = '{"cmd": "move_unit", "id": ' + '7' + ', "x": ' + newX + ', "y": ' + newY + '}';
    GlobalVars.socket.sendMessage(move);
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

      } else {
          GlobalVars.messages.push(jsonData);
      }
    };
  }

  public sendMessage(message : String) {
    this.websocket.send(message);
  }
}
