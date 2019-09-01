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
import { MultiImage } from '../multiImage';
import { Network } from '../network';
import { HERO, DEAD } from '../config';

export class ObjectScene extends Phaser.Scene {

  private renderToggle = false;

  public objectList = {};

  private spriteTasks = [];
  private containerTasks = [];

  constructor() {
    super({
      key: "ObjectScene",
      active: true
    });
  }

  preload(): void {
    this.load.image('selecthex', './static/art/hover-hex.png');
    this.load.image('foundation', './static/art/foundation.png');
  }

  create(): void {
    console.log('Object Scene Create');
    this.time.addEvent({ delay: 200, callback: this.processRender, callbackScope: this, loop: true });
   
    this.onJumpComplete = this.onJumpComplete.bind(this);
    this.onMoveComplete = this.onMoveComplete.bind(this);
    this.onDmgTextComplete = this.onDmgTextComplete.bind(this);

    Global.gameEmitter.on(NetworkEvent.PERCEPTION, this.setRender, this);
    Global.gameEmitter.on(NetworkEvent.CHANGES, this.setRender, this);
    Global.gameEmitter.on(NetworkEvent.IMAGE_DEF, this.processImageDefMessage, this);
    Global.gameEmitter.on(NetworkEvent.DMG, this.processDmgMessage, this);
    
    this.load.on('filecomplete', this.fileLoadComplete, this);
    this.load.on('complete', this.loadComplete, this);

    this.add.image(100, 100, 'selecthex');
  }

  processImageDefMessage(message) {
    console.log('image_def')
    
    if(message.result != '404') {
      console.log(message.name);
      console.log(message.data);
      Global.imageDefList[message.name] = message.data;

      if(Array.isArray(message.data.images)) {
        console.log(message.data.images);
        for(var i = 0; i < message.data.images.length; i++) {

          var multiImage : MultiImage = {
            imageName : message.data.images[i],
            width : message.data.frames[i][2],
            height : message.data.frames[i][3],
            regX : message.data.frames[i][5],
            regY : message.data.frames[i][6]
          }

          Global.multiImages[message.name] = multiImage;

          this.load.image(message.name + i, multiImage.imageName);
          this.load.start();
        }

        //Push task        
        this.containerTasks.push(message.name);
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
      //console.log('Draw Objects');
      this.drawObjects()
      this.renderToggle = false;
    }
  }

  setRender() : void {
    //console.log('Object Scene Set Render')
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
          var pixel = Util.hex_to_pixel(objectState.x, objectState.y);

          if(objectState.state == 'moving') {
            sprite.play(objectState.image + '_moving');  
            sprite.x = pixel.x;
            sprite.y = pixel.y;
          } else if(objectState.class == 'structure' && objectState.state == 'none') {
            sprite.setTexture(objectState.image);
          } else {
            var animation;

            if(objectState.state == DEAD && objectState.prevstate != DEAD) {
              animation = 'die';
            } else {
              animation = objectState.state;
            }

            sprite.play(objectState.image + '_' + animation);

            //Only follow if Hero
            if(objectState.subclass == HERO) {

              var mapScene = this.scene.get('MapScene') as MapScene;
              mapScene.cameras.main.startFollow(sprite, true);
              mapScene.cameras.main.followOffset.x = -36;
              mapScene.cameras.main.followOffset.y = -36;

              this.cameras.main.startFollow(sprite, true);
              this.cameras.main.followOffset.x = -36;
              this.cameras.main.followOffset.y = -36;
            }
           
            //Move completed, add tween to new location
            if(sprite.x != pixel.x || sprite.y != pixel.y) {
              var tween = this.tweens.add({
                targets: sprite,
                x: pixel.x,
                y: pixel.y,
                ease: 'Power1',
                duration: 500,
                onComplete: this.onMoveComplete
              });

              tween.play();
            }
          }
        } else if(objectState.op == 'deleted') {
            var sprite = this.objectList[objectState.id] as GameSprite;
            sprite.destroy();
        }
    }
  }

  addSprite(objectState : ObjectState) {
    var pixel = Util.hex_to_pixel(objectState.x, objectState.y);
    var imageName = '';

    if(objectState.class == 'structure' && objectState.state == 'founded') {
      imageName = 'foundation';
    } else {
      imageName = objectState.image;
    }

    var sprite = new GameSprite({
      scene: this,
      x: pixel.x,
      y: pixel.y,
      id: objectState.id,
      imageName: imageName
    });
  
    if(objectState.class == 'structure') {
      sprite.setDepth(1);
    } else {
      sprite.setDepth(2);
    }

    this.add.existing(sprite);

    if(Util.isSprite(imageName)) {
      var animName = imageName + '_' + objectState.state;
      sprite.anims.play(animName);
    }

    this.objectList[objectState.id] = sprite;

    console.log(sprite);

    if(objectState.subclass == 'hero') {
      var mapScene = this.scene.get('MapScene') as MapScene;
      mapScene.cameras.main.centerOn(sprite.x + 36, sprite.y + 36);
      this.cameras.main.centerOn(sprite.x + 36, sprite.y + 36);
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

  fileLoadComplete(key, type, raw) {
    var imageName = key;
    console.log('Loaded file: ' + imageName + ', ' + type);

    if(Util.isSprite(imageName)) {

      var animsData = Global.imageDefList[imageName].animations;

      for(var animName in animsData) {
        var anim = animsData[animName];
        var repeat = 0;
        var duration;
        var frames;

        if(Array.isArray(anim)) {
          if(anim.length > 1) {
            var start = anim[0];
            var end = anim[1];

            repeat = anim[2];
            duration = anim[3];

            frames = this.anims.generateFrameNumbers(imageName, { start: start, end: end});

          } else {
            duration = 10000;
            frames = this.anims.generateFrameNumbers(imageName, { start: anim[0], end: anim[0]});
          }
        }
        else if(Array.isArray(anim.frames)) {
          duration = anim.speed;
          repeat = anim.repeat;
          frames = this.anims.generateFrameNumbers(imageName, { frames: anim.frames});
        } else {
          console.log('Should never reach here')
        }

        var config = {
          key: imageName + '_' + animName,
          frames: frames,
          repeat: repeat,
          duration: duration
        };
        
        console.log(config);
        this.anims.create(config);
      }
    } else {
      console.log('No animation')
    }

    this.addLoadedSprites(imageName);
  }

  loadComplete() {
    for(var i = 0; i < this.containerTasks.length; i++) {

    }
  }

  processDmgMessage(message) {
    console.log('Dmg Message: ' + message);
    if(message.sourceid in this.objectList && 
       message.targetid in this.objectList) {
      var source = this.objectList[message.sourceid] as GameSprite;
      var target = this.objectList[message.targetid] as GameSprite;

      if(Global.objectStates[message.sourceid].subclass == HERO) {

        var mapScene = this.scene.get('MapScene') as MapScene;
        mapScene.cameras.main.stopFollow();

        this.cameras.main.stopFollow();
      }

      console.log('Play attack');
      source.play(source.imageName + '_attack');
      source.anims.chain(source.imageName + '_none');

      var diffX = (target.x - source.x) * 0.5;
      var diffY = (target.y - source.y) * 0.5;

      var destX = source.x + diffX;
      var destY = source.y + diffY;

      var tween = this.tweens.add({
        targets: source,
        x: destX,
        y: destY,
        ease: 'Power2',
        duration: 750,
        onComplete: this.onJumpComplete
      });

      tween.play();

      var dmgText = this.add.text(target.x + 33, target.y - 10, message.dmg, { fontFamily: 'Verdana', fontSize: 24, color: '#FF0000' });
      dmgText.setDepth(10);

      var textTween = this.tweens.add({
        targets: dmgText,
        alpha: 0,
        ease: 'Power1',
        duration: 5000,
        onComplete: this.onDmgTextComplete
      });

      textTween.play();

      if(message.state == 'dead') {
        target.play(target.imageName + '_die');
      }
    }
  }

  onJumpComplete(tween, targets) {
    var startX = tween.data[0].start;
    var startY = tween.data[1].start;

    var returnTween = this.tweens.add({
        targets: targets[0],
        x: startX,
        y: startY,
        ease: 'Power2',
        duration: 200,
      });

    returnTween.play();
  }

  onMoveComplete(tween, targets) {
    var sprite = targets[0]

    if(Global.objectStates[sprite.id].subclass == HERO) {

      var mapScene = this.scene.get('MapScene') as MapScene;
      mapScene.cameras.main.stopFollow();
      this.cameras.main.stopFollow();
    }
  }

  onDmgTextComplete(tween, targets){
    var sprite = targets[0];
    sprite.destroy();
  }

  isContainer(imageName) : boolean {
    //Strip numbers
    var name = imageName.replace(/[0-9]/g, '');

    return (this.containerTasks.includes(name));
  }
}