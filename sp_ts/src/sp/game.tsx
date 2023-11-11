/**
 * @author       Peter Holko
 * @copyright    2018 - 2019 Peter Holko
 */

import Phaser from "phaser";
import { ObjectScene } from './scenes/objectScene';
import { MapScene } from './scenes/mapScene';
import { Global } from './global';

import * as React from "react";
import styles from "./app.module.css";

import { GAME_HEIGHT, GAME_WIDTH } from "./config";

document.addEventListener("visibilitychange", function () {
  if (document.visibilityState === 'visible') {
    console.log("Tab visisble");
    Global.gameEmitter.emit("VISIBLE", {});
  } else {
    console.log("Tab no longer visisble");
  }
});

export default class Game extends React.Component {
  componentDidMount() {
    const config: any = {
      title: "Siege Perilous",
      version: "0.0.1",
      width: window.innerWidth,
      height: window.innerHeight,
      type: Phaser.AUTO,
      parent: "game",
      scene: [MapScene, ObjectScene],
      input: {
        mouse: true
      },
      render: { pixelArt: true },
      fx: {
        glow: {
          distance: 32,
          quality: 0.1
        }
      }
    };

    new Phaser.Game(config);
  }

  shouldComponentUpdate() {
    return false;
  }

  public render() {
    return <div id="game" className={styles.game} />;
  }
}

export function getTileAt(hexX, hexY) {
  var key = hexX + '_' + hexY;

  if (key in Global.tileStates) {
    return Global.tileStates[key];
  } else {
    return false;
  }
}