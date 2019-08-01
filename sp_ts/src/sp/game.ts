/**
 * @author       Peter Holko
 * @copyright    2018 - 2019 Peter Holko
 */

import "phaser";
import { ObjectScene } from './scenes/objectScene';
import { MapScene } from './scenes/mapScene';
import { UIScene } from './scenes/uiScene';
import { Global } from './global';

export function startGame() {
  Global.gameEmitter = new Phaser.Events.EventEmitter();
  Global.game = new Game(config);
}

const config = {
  title: "Siege Perilous",
  version: "1.0",
  width: 667,
  height: 375,
  type: Phaser.AUTO,
  parent: "game",
  scene: [MapScene, ObjectScene, UIScene],
  dom: {
    createContainer: true
  },
  input: {
    mouse: true
  },
  render: { pixelArt: true }
};

export class Game extends Phaser.Game {

  constructor(config) {
    super(config);
  }
}



export function getTileAt(hexX, hexY) {
  var key = hexX + '_' + hexY;

  if(key in Global.tileStates) {
    return Global.tileStates[key];
  } else {
    return false;
  }
}