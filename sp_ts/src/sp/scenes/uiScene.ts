/**
 * @author       Peter Holko
 * @copyright    2018 - 2019 Peter Holko
 */

import { Util } from '../util';
import { Global } from '../global';
import { Tile } from '../objects/tile';
import { GameSprite } from '../objects/gameSprite';
import { GameEvent } from '../gameEvent';
import { MapScene } from './mapScene';
import { Network } from '../network';
import { ObjectState } from '../objectState';
import { InfoPanel } from '../ui/infoPanel';
import { Obj } from '../obj';
import { SelectBox } from '../ui/selectBox';

export class UIScene extends Phaser.Scene {

  private avatarframe; 
  private fillhp;

  private movecompass;

  private exploreButton;
  private gatherButton;
  private buildButton;
  private exitButton;
  
  private targetInfoButton;

  private singlePanel;
  private thinPanel1;

  private heroSpriteImg;

  private selectBoxes = [];
  private selectBoxInitX = 587;
  private selectBoxInitY = 10;
  private selectBoxBorder;

  constructor() {
    super({
      key: "UIScene",
      active: true
    });
  }

  preload(): void {
  }

  create(): void {
    console.log('UI Scene Create');

    var selectBoxBorderImg = document.createElement('img');

    var exploreButtonImg = Util.createImage('./static/art/ui/explorebutton.png');
    var gatherButtonImg = Util.createImage('./static/art/ui/gatherbutton.png');
    var buildButtonImg = Util.createImage('./static/art/ui/buildbutton.png');
    var exitButtonImg = Util.createImage('./static/art/ui/exitbutton.png');

    var targetInfoButtonImg = Util.createImage('./static/art/ui/targetinfobutton.png');

    var singlePanelImg = document.createElement('img');

    var avatarFrameImg = document.createElement('img');
    var hpFrameImg = document.createElement('img');
    var staFrameImg = document.createElement('img');
    var fillhpImg = document.createElement('img');

    this.heroSpriteImg = document.createElement('img');

    singlePanelImg.src = './static/art/ui/halfframe.png';

    hpFrameImg.src = './static/art/ui/hpframe.png';
    staFrameImg.src = './static/art/ui/hpframe.png';
    fillhpImg.src = './static/art/ui/fillhp.png';
    avatarFrameImg.src = './static/art/ui/heroring.png';
    selectBoxBorderImg.src = './static/art/ui/selectboxborder.png';

    this.heroSpriteImg.src = './static/art/heromage_single.png';

    this.selectBoxBorder = this.add.dom(0,0, selectBoxBorderImg).setOrigin(0, 0);
    this.selectBoxBorder.visible = false;

    this.exploreButton = this.add.dom((Global.gameWidth / 2) - 125, 
                                      Global.gameHeight - 60, 
                                      exploreButtonImg).setOrigin(0, 0);

    this.gatherButton = this.add.dom((Global.gameWidth / 2) - 75, 
                                     Global.gameHeight - 60, gatherButtonImg).setOrigin(0, 0);

    this.buildButton = this.add.dom((Global.gameWidth / 2) - 25, 
                                    Global.gameHeight - 60, buildButtonImg).setOrigin(0, 0);


    this.targetInfoButton = this.add.dom(this.selectBoxInitX + 5, 
                                         this.selectBoxInitY + 80, 
                                         targetInfoButtonImg).setOrigin(0, 0);

    Global.gameEmitter.on(GameEvent.TILE_CLICK, this.tileClickHandler, this);

    this.movecompass = document.createElement('img');
    this.movecompass.src = './static/art/ui/movecompass.png';

    //fillhpImg.height = 18;

    this.add.dom(40, 10, hpFrameImg).setOrigin(0, 0);
    //this.add.dom(64, 36, staFrameImg).setOrigin(0, 0);
    

    fillhpImg.width = 100;
    fillhpImg.height = 14;
  
    //this.fillhp = this.add.dom(64 + 50, 22 + 7, fillhpImg).setOrigin(0.5, 0.5);
    this.add.dom(8, 21, avatarFrameImg).setOrigin(0, 0);
    this.add.dom(12, 25, this.heroSpriteImg).setOrigin(0, 0);

    var compass = this.add.dom(10, 225, this.movecompass).setOrigin(0, 0);

    this.thinPanel1 = new InfoPanel(this);

    var _this = this;

    this.buildButton.addListener('click')
    this.buildButton.on('click', function(event) {
      Network.sendInfoUnit('7');
    });


    this.add.dom(5,5, singlePanelImg).setOrigin(0, 0);

    compass.addListener('click')
    console.log(compass);


    compass.on('click', function (event) {
      console.log('compass click');
      console.log('offset: ' + event.offsetX + ', ' + event.offsetY);
      console.log('compass: ' + compass.width);

      var pocX = event.offsetX + this.width / 2;
      var pocY = event.offsetY + this.height / 2;

      console.log('poc: ' + pocX + ', ' + pocY);

      var centerX = 151 / 2;
      var centerY = 152 / 2;

      console.log('center: ' + centerX + ', ' + centerY);

      var angleRads = Math.atan2(pocX - centerX, pocY - centerY);
      var angleDegrees = ((angleRads * 180) / Math.PI) + 180;

      console.log('Angle: ' + angleDegrees);

      var heroObj = Global.objectStates[Global.heroId] as ObjectState;

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
    for(var i = 0; i < this.selectBoxes.length; i++) {
      this.selectBoxes[i].destroy();
    }

    this.selectBoxBorder.visible = false;

    var tile = gameObject as Tile;
    var objIdsOnTile = Obj.getObjsAt(tile.hexX, tile.hexY);

    //Add terain tile first
    var tileIndex = tile.hexX + '_' + tile.hexY;
    var tileState = Global.tileStates[tileIndex];
    var tileId = tileState.tiles.reverse()[0];
    var imageName = Global.tileset[tileId].image; 
   
    var selectBox = new SelectBox({scene: this,
      x: this.selectBoxInitX, 
      y: this.selectBoxInitY});

    selectBox.setImage(tileIndex, 'tile', imageName, 0.66, 13, 12);

    selectBox.targetImg.addListener('click');
    selectBox.targetImg.on('click', (event) => {
      this.selectBoxBorder.depth = 100;
      this.selectBoxBorder.x = this.selectBoxInitX + 4;
      this.selectBoxBorder.y = this.selectBoxInitY + 4;
      this.selectBoxBorder.visible = true;
    });

    this.selectBoxes.push(selectBox)

    for(var i = 0; i < objIdsOnTile.length; i++) {
      if(i > 5)
        break;

      var objId : integer = Number(objIdsOnTile[i]);

      if(Util.isSprite(Global.objectStates[objId].image)) {
        var imageName = Global.objectStates[objId].image + '_single.png';
      } else {
        var imageName = Global.objectStates[objId].image + '.png';
      }

      var selectBox = new SelectBox({scene: this, 
        x: this.selectBoxInitX - (79 * i + 79), y: this.selectBoxInitY});

      var ui = this;

      selectBox.setImage(objId, 'obj', imageName);
      selectBox.targetImg.addListener('click');
      selectBox.targetImg.on('click', function(event) {
        ui.selectBoxBorder.depth = 100;
        ui.selectBoxBorder.x = event.x - event.offsetX + 4;
        ui.selectBoxBorder.y = event.y - event.offsetY + 4;
        ui.selectBoxBorder.visible = true;
      });

      this.selectBoxes.push(selectBox);
    }

  }

} 