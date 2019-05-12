/**
 * @author       Peter Holko
 * @copyright    2018 - 2019 Peter Holko
 */

import "phaser";
import { GameScene } from './scenes/gameScene';
import { GlobalVars } from './globalvars';

export function startGame() {
  GlobalVars.game = new Game(config);
}

const config: GameConfig = {
  title: "Siege Perilous",
  version: "1.0",
  width: 666,
  height: 375,
  type: Phaser.AUTO,
  parent: "game",
  scene: [GameScene],
  input: {
    mouse: true
  },
  render: { pixelArt: true, antialias: false }
};

export class Game extends Phaser.Game {
  constructor(config: GameConfig) {
    super(config);
  }
}