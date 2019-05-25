/**
 * @author       Peter Holko
 * @copyright    2018 - 2019 Peter Holko
 */

import "phaser";
import { GameScene } from './scenes/gameScene';
import { UIScene } from './scenes/uiScene';
import { GlobalVars } from './globalvars';

export function startGame() {
  GlobalVars.gameEmitter = new Phaser.Events.EventEmitter();
  GlobalVars.game = new Game(config);
}

const config = {
  title: "Siege Perilous",
  version: "1.0",
  width: 667,
  height: 375,
  type: Phaser.AUTO,
  parent: "game",
  scene: [GameScene, UIScene],
  dom: {
    createContainer: true
  },
  input: {
    mouse: true
  },
  render: { pixelArt: true, antialias: false }
};

export class Game extends Phaser.Game {

  constructor(config) {
    super(config);
  }
}