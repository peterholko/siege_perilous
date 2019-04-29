/**
 * @author       Peter Holko
 * @copyright    2018 - 2019 Peter Holko
 */

import "phaser";
import { GameScene } from "./scenes/gameScene";

var game : Game;
var network : Network;

// main game configuration
const config: GameConfig = {
  width: 1332,
  height: 750,
  type: Phaser.AUTO,
  parent: "game",
  scene: GameScene,
  physics: {
    default: "arcade",
    arcade: {
      gravity: { y: 200 }
    }
  }
};

// game class
export class Game extends Phaser.Game {
  constructor(config: GameConfig) {
    super(config);
  }
}

export function sendLogin() {
  var username = (<HTMLInputElement>document.getElementById("username")).value;
  var password = (<HTMLInputElement>document.getElementById("password")).value

  var login = '{"cmd": "login", "username": "' + username + 
              '", "password": "' + password + '"}';

  network.sendMessage(login);
}

// when the page is loaded, create our game instance
window.addEventListener("load", () => {
  console.log('Opening websocket to server')
  
  network = new Network();
});

export class Network {
  private websocket : WebSocket;

  constructor() {
    var url : string = "ws://" + window.location.host + "/websocket";

    this.websocket = new WebSocket(url);

    this.websocket.onopen = (evt: Event) => {
      console.log('Opened websocket');
    }

    this.websocket.onmessage = (evt: MessageEvent) => {
      var jsonData = JSON.parse(evt.data);
      console.log(jsonData);

      if(jsonData.packet == "login") {
        game = new Game(config);

        var login = (<HTMLInputElement>document.getElementById("login"));
        var logo = (<HTMLInputElement>document.getElementById("logo"));

        login.style.display = "none";
        logo.style.display = "none";
      } else if(jsonData.packet == "perception") {
        console.log(jsonData.data.map);
      }
    }
  }

  sendMessage(message) : void {
    this.websocket.send(message);
  }
}


