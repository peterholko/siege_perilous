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
import { NetworkEvent } from '../networkEvent';
import { ObjectState } from '../objectState';

export class ObjectScene extends Phaser.Scene {

  private renderToggle = false;

  public objectList = {};

  //private imageDefList = [];
  private spriteTasks = [];

  constructor() {
    super({
      key: "ObjectScene",
      active: true
    });
  }

  preload(): void {
    this.load.image('selecthex', './static/art/hover-hex.png');
  }

  create(): void {
    console.log('Object Scene Create');
    this.time.addEvent({ delay: 200, callback: this.processRender, callbackScope: this, loop: true });
    
    Global.gameEmitter.on(NetworkEvent.PERCEPTION, this.setRender, this);
    Global.gameEmitter.on(NetworkEvent.CHANGES, this.setRender, this);
    Global.gameEmitter.on(NetworkEvent.IMAGE_DEF, this.processImageDefMessage, this);
    
    this.load.on('filecomplete', this.loadSpriteSheet, this);

    this.add.image(100, 100, 'selecthex');

    //this.cameras.main.centerOn(864, 2592);
  }

  processImageDefMessage(message) {
    console.log('image_def')
    
    if(message.result != '404') {
      console.log(message.name);
      console.log(message.data);
      Global.imageDefList[message.name] = message.data;

      if(Array.isArray(message.data.images)) {
        console.log(message.data.images);
        //TODO handle stockade type images
      } else {
        console.log(message.name);
        this.load.spritesheet(message.name, './static/art/' + message.name + '.png',
                              {frameWidth: message.data.frames.width, 
                                frameHeight: message.data.frames.height})
        this.load.start();
      }
    }
  }

  processRender() : void {
    if(this.renderToggle) {
      console.log('Draw Objects');
      this.drawObjects()
      this.renderToggle = false;
    }
  }

  setRender() : void {
    console.log('Object Scene Set Render')
    this.renderToggle = true;
  }

  drawObjects() : void {
    for(var objectId in Global.objectStates) {
        var objectState = Global.objectStates[objectId] as ObjectState;

        
        if(objectState.op == 'added') {
          console.log('Object Added');

          if(this.textures.exists(objectState.image)) {
            this.addSprite(objectState);
          } else {
            var getImage = '{"cmd": "image_def", "name": "' + objectState.image + '"}';
            Global.socket.sendMessage(getImage);
            console.log(getImage);

            this.spriteTasks.push(objectState);
          }

        }
        else if(objectState.op == 'updated') {
          console.log('Object Updated');
          var sprite = this.objectList[objectState.id] as GameSprite;
          var pixel = Util.hex_to_pixel(objectState.hexX, objectState.hexY);

          sprite.x = pixel.x;
          sprite.y = pixel.y;

          if(objectState.subclass == 'hero') {
            this.centerOnHero(sprite.x, sprite.y);
          }
        }
    }
  }

  addSprite(objectState : ObjectState) {
    var pixel = Util.hex_to_pixel(objectState.hexX, objectState.hexY);

    var sprite = new GameSprite({
      scene: this,
      x: pixel.x,
      y: pixel.y,
      id: objectState.id,
      imageName: objectState.image
    });
    
    this.add.existing(sprite);

    if(Util.isSprite(objectState.image)) {
      var animName = objectState.image + '_' + objectState.state;
    } else {
      var animName = objectState.image;
    }
    sprite.anims.play(animName);

    this.objectList[objectState.id] = sprite;

    console.log(sprite);

    if(objectState.subclass == 'hero') {
      Global.heroId = objectState.id;
      
      this.centerOnHero(sprite.x, sprite.y);
   }
  }

  addLoadedSprites(imageName) {
    console.log('addLoadedSprites: ');
    var spritesToAdd = this.spriteTasks.filter(obj => obj.image === imageName);

    for(var i = 0; i < spritesToAdd.length; i++) {
      var spriteObj = spritesToAdd[i];
      console.log(spriteObj);
      this.addSprite(spriteObj);
    }
  }

  loadSpriteSheet(key, type, raw) {
    var imageName = key;
    console.log('Loading spritesheet: ' + imageName + ', ' + type);

    if(Util.isSprite(imageName)) {

      var animsData = Global.imageDefList[imageName].animations;

      for(var animName in animsData) {
        var anim = animsData[animName];

        if(Array.isArray(anim)) {
          if(anim.length > 1) {
            var start = anim[0];
            var end = anim[1];
            var next = anim[2];
            var repeat = 1;

            if(animName == next) {
              repeat = -1;
            }

            var frameRate = 1 / parseFloat(anim[3]);
            var frames = this.anims.generateFrameNumbers(imageName, { start: start, end: end});

          } else {
            var frameRate = 1;
            var repeat = 1;
            var frames = this.anims.generateFrameNumbers(imageName, { start: anim[0], end: anim[0]});
          }
        }
        else if(Array.isArray(anim.frames)) {
          var frameRate = 1 / parseFloat(anim.speed);
          var repeat = 1;
          var frames = this.anims.generateFrameNumbers(imageName, { frames: anim.frames});
        } else {
          console.log('Should never reach here')
        }

        var config = {
          key: imageName + '_' + animName,
          frames: frames,
          repeat: repeat,
          frameRate: frameRate
        };
        
        console.log(config);
        this.anims.create(config);
      }
    } else {
      //No animations use case
      var configNone = {
        key: imageName,
        frames: this.anims.generateFrameNumbers(imageName, { start: 0, end: 0}),
        repeat: 1,
        frameRate: 1
      }

      this.anims.create(configNone);
    }

    this.addLoadedSprites(imageName);
  }

  centerOnHero(heroX, heroY) : void {
      var centerX = heroX + 36;
      var centerY = heroY + 36;

      var mapScene = this.scene.get('MapScene') as MapScene;
      mapScene.centerOn(centerX, centerY);

      this.cameras.main.centerOn(centerX, centerY);    
  }

}