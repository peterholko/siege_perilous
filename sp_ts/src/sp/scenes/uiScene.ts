/**
 * @author       Peter Holko
 * @copyright    2018 - 2019 Peter Holko
 */

import { Util } from '../util';
import { GlobalVars } from '../globalvars';
import { Tile } from '../objects/tile';
import { GameSprite } from '../objects/gameSprite';
import { GameEvent } from '../gameEvent';
import { GameScene } from './gameScene';
import { Network } from '../network';
import { ObjectState } from '../objectState';

export class UIScene extends Phaser.Scene {

  private playerframe;
  private movecompass;
  private selectIcon1;
  private selectIcon2;
  private selectIcon3;
  private selectIcon4;

  constructor() {
    super({
      key: "UIScene",
      active: true
    });
  }

  preload(): void {
  }

  create(): void {
    console.log('Create');
    var selectIcon1Img = document.createElement('img');
    var selectIcon2Img = document.createElement('img');
    var selectIcon3Img = document.createElement('img');
    var selectIcon4Img = document.createElement('img');

    selectIcon1Img.src = './static/art/ui/selecticon.png';
    selectIcon2Img.src = './static/art/ui/selecticon.png';
    selectIcon3Img.src = './static/art/ui/selecticon.png';
    selectIcon4Img.src = './static/art/ui/selecticon.png';

    this.selectIcon1 = this.add.dom(587, 10, selectIcon1Img);
    this.selectIcon2 = this.add.dom(508, 10, selectIcon2Img);
    this.selectIcon3 = this.add.dom(429, 10, selectIcon3Img);
    this.selectIcon4 = this.add.dom(350, 10, selectIcon4Img);

    this.selectIcon1.visible = false;
    this.selectIcon2.visible = false;
    this.selectIcon3.visible = false;
    this.selectIcon4.visible = false;

    GlobalVars.gameEmitter.on(GameEvent.TILE_CLICK, this.tileClickHandler, this);

    this.playerframe = document.createElement('img');
    this.movecompass = document.createElement('img');

    this.playerframe.src = './static/art/ui/playerframe.png';
    this.movecompass.src = './static/art/ui/movecompass.png';

    this.add.dom(10, 10, this.playerframe);
    var compass = this.add.dom(10, 225, this.movecompass);
    
    compass.addListener('click')
    console.log(compass);

    var _this = this;

    compass.on('click', function (event) {
      console.log('compass click');
      console.log('offset: ' + event.offsetX + ', ' + event.offsetY);
      console.log('compass: ' + compass.width);

      var pocX = event.offsetX + this.width / 2;
      var pocY = event.offsetY + this.height / 2;

      console.log('poc: ' + pocX + ', ' + pocY);

      var centerX = compass.displayWidth / 2;
      var centerY = compass.displayHeight / 2;

      console.log('center: ' + centerX + ', ' + centerY);

      var angleRads = Math.atan2(pocX - centerX, pocY - centerY);
      var angleDegrees = ((angleRads * 180) / Math.PI) + 180;

      console.log('Angle: ' + angleDegrees);

      var gameScene = _this.scene.get('GameScene') as GameScene;
      var heroObj = gameScene.objectStates[gameScene.heroId] as ObjectState;

      if(angleDegrees < 30 || angleDegrees >= 330) {
        console.log('N');
        var nextPos = Util.nextPosByDirection(heroObj.hexX, heroObj.hexY, 'N');
        Network.sendMove(nextPos.q, nextPos.r);
      } else if(angleDegrees < 90 && angleDegrees >= 30) {
        console.log('NW');
        var nextPos = Util.nextPosByDirection(heroObj.hexX, heroObj.hexY, 'NW');
        Network.sendMove(nextPos.q, nextPos.r);
      } else if(angleDegrees < 150 && angleDegrees >= 90) {
        console.log('SW');
        var nextPos = Util.nextPosByDirection(heroObj.hexX, heroObj.hexY, 'SW');
        Network.sendMove(nextPos.q, nextPos.r);
      } else if(angleDegrees < 210 && angleDegrees >= 150) {
        console.log('S');
        var nextPos = Util.nextPosByDirection(heroObj.hexX, heroObj.hexY, 'S');
        Network.sendMove(nextPos.q, nextPos.r);
      } else if(angleDegrees < 270 && angleDegrees >= 210) {
        console.log('SE');
        var nextPos = Util.nextPosByDirection(heroObj.hexX, heroObj.hexY, 'SE');
        Network.sendMove(nextPos.q, nextPos.r);
      } else if(angleDegrees < 330 && angleDegrees >= 270) {
        console.log('NE');
        var nextPos = Util.nextPosByDirection(heroObj.hexX, heroObj.hexY, 'NE');
        Network.sendMove(nextPos.q, nextPos.r);
      }
    });

  }

   private tileClickHandler(gameObject) {
    console.log('tileClickHandler');
    var tile = gameObject as Tile;

    var gameScene = this.scene.get('GameScene') as GameScene;
    var objsOnTile = gameScene.getObjsAt(tile.hexX, tile.hexY);

    for(var i = 0; i < objsOnTile.length; i++) {
      var gameSprite = objsOnTile[i] as GameSprite;
      console.log('frameTotal: ' + gameSprite.texture.frameTotal);
      console.log(gameSprite);
      if(gameSprite.texture.frameTotal > 1) {
        
        var spriteImage = document.createElement('img');
        spriteImage.src = './static/art/' + gameSprite.imageName + '_single.png';
        
        this.add.dom(587, 10, spriteImage);

        this.selectIcon1.visible = true;
      }
    }
  }


} 