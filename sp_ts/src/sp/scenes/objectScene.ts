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
import { HERO, DEAD, SPRITE, CONTAINER, IMAGE, FOUNDED, WALL } from '../config';
import { GameImage } from '../objects/gameImage';
import { GameContainer } from '../objects/gameContainer';

export class ObjectScene extends Phaser.Scene {

  private renderToggle = false;

  public objectList = {};

  private imageDefTasks = [];
  private containerTasks = [];

  private shroudTiles = [];

  private wallList: Array<ObjectState> = [];
  
  private multiImages: Record<string, Array<MultiImage>> = {};

  constructor() {
    super({
      key: "ObjectScene",
      active: true
    });
  }

  preload(): void {
    this.load.image('selecthex', './static/art/hover-hex.png');
    this.load.image('foundation', './static/art/foundation.png');
    this.load.image('shroud', './static/art/shroud.png');
  }

  create(): void {
    console.log('Object Scene Create');
   
    this.onJumpComplete = this.onJumpComplete.bind(this);
    this.onMoveComplete = this.onMoveComplete.bind(this);
    this.onDmgTextComplete = this.onDmgTextComplete.bind(this);

    Global.gameEmitter.on("VISIBLE", this.drawAllObjects, this);

    Global.gameEmitter.on(NetworkEvent.PERCEPTION, this.drawInit, this);
    Global.gameEmitter.on(NetworkEvent.CHANGES, this.setRender, this);
    Global.gameEmitter.on(NetworkEvent.IMAGE_DEF, this.processImageDefMessage, this);
    Global.gameEmitter.on(NetworkEvent.DMG, this.processDmgMessage, this);
    Global.gameEmitter.on(NetworkEvent.SPEECH, this.processSpeech, this);
    
    this.load.on('filecomplete', this.fileLoadComplete, this);
    this.load.on('complete', this.loadComplete, this);
  }

  processImageDefMessage(message) {
    console.log('image_def')
    
    if(message.result != '404') {
      console.log(message.name);
      console.log(message.data);

      if(Array.isArray(message.data.images)) {
        console.log(message.data.images);

        //Check if already loaded
        if(!(message.name in Global.imageDefList)) {

          for(var i = 0; i < message.data.images.length; i++) {

            var multiImage : MultiImage = {
              key : message.name + i,
              imageName : message.data.images[i],
              width : message.data.frames[i][2],
              height : message.data.frames[i][3],
              regX : message.data.frames[i][5],
              regY : message.data.frames[i][6]
            }

            if(!this.multiImages.hasOwnProperty(message.name)) {
              this.multiImages[message.name] = new Array();
            }

            this.multiImages[message.name].push(multiImage);

            this.load.image(message.name + i, multiImage.imageName);
            this.load.start();
          }
          
          this.containerTasks.push(message.name);
        }
      } else {
        console.log(message.name);
        this.load.spritesheet(message.name, './static/art/' + message.name + '.png',
                              {frameWidth: message.data.frames.width, 
                                frameHeight: message.data.frames.height})
        this.load.start();
      }

      Global.imageDefList[message.name] = message.data;
    }
  }

  processRender() : void {
    console.log('processRender');
    if(this.renderToggle) {
      console.log('Draw Objects');
      this.drawObjects()
      this.renderToggle = false;
    }
  }

  setRender() : void {
    console.log('ObjectScene setRender')
    this.renderToggle = true;
  }

  drawInit() : void {
    console.log('drawInit');
    //TODO revisit this hack, possible issue if the 'added' is overwritten during drawing
    this.drawAllObjects();
    this.drawObjects();

    this.time.addEvent({ delay: 200, callback: this.processRender, callbackScope: this, loop: true });
  }

  drawAllObjects() : void {
    //Clear all objects
    for(var key in this.objectList) {
      var obj = this.objectList[key];
      obj.destroy();
    }

    for(var objectId in Global.objectStates) {
      var objectState = Global.objectStates[objectId] as ObjectState;
      objectState.op = 'added';
    }

    this.setRender();
  }

  drawObjects() : void {
    //Clear visibleTiles & shroud
    Global.visibleTiles = [];
    this.clearShroud();

    for(var objectId in Global.objectStates) {
        var objectState = Global.objectStates[objectId] as ObjectState;

        if(objectState.op == 'added') {
          console.log('Object Added');

          if(Global.imageDefList.hasOwnProperty(objectState.image)) {
            const imageType = Util.getImageType(objectState.image);

            if(imageType == SPRITE) {
              this.addSprite(objectState);
            } else if(imageType == IMAGE) {
              this.addImage(objectState);
            } else if (imageType == CONTAINER) {
              this.addContainer(objectState);
            }
          } else {
            Network.sendImageDef(objectState.image);
            
            this.imageDefTasks.push(objectState);
          }
        }
        else if(objectState.op == 'updated') {
          console.log('Object Updated');

          if(Global.imageDefList.hasOwnProperty(objectState.image)) {
            const imageType = Util.getImageType(objectState.image);

            if(imageType == IMAGE) {
              this.updateImage(objectState);
            } else if(imageType == SPRITE) {
              this.updateSprite(objectState);
            } else if(imageType == CONTAINER) {
              this.updateContainer(objectState);
            }
          } else {
            Network.sendImageDef(objectState.image);
            
            this.imageDefTasks.push(objectState);
 
          }

        } else if(objectState.op == 'deleted') {
            var obj = this.objectList[objectState.id];
            obj.destroy();
        }

        this.processVisibleTiles(objectState);
    }

    //Call processWall here for loaded wall images
    this.processWallList();

    //Add Shroud tiles
    this.addShroud();
  }

  processVisibleTiles(objectState : ObjectState) {
    if(objectState.player == Global.playerId) {
      if(objectState.vision > 0) {
        var visibleTiles = Util.range(objectState.x, 
                                      objectState.y,
                                      objectState.vision);

        Global.visibleTiles = Global.visibleTiles.concat(visibleTiles);
      }
    }
  }

  processWallList() {
    //Hide overlapping containers images
    for(var wallKey in this.wallList) {
      var wall = this.wallList[wallKey];
      var neighbours = Util.getNeighbours(wall.x, wall.y);

      for(var neighbourId in neighbours) {
        var neighbour = neighbours[neighbourId];

        for(var otherId in this.wallList) {
          var other = this.wallList[otherId];

          if((neighbour.q == other.x) && (neighbour.r == other.y)) {
            var container = this.objectList[wall.id] as GameContainer;

            if(neighbour.d == 'nw') {
              (container.getAt(2) as Phaser.GameObjects.Image).setVisible(false);
              (container.getAt(4) as Phaser.GameObjects.Image).setVisible(false);
            } else if(neighbour.d == 'ne') {
              (container.getAt(3) as Phaser.GameObjects.Image).setVisible(false);
              (container.getAt(5) as Phaser.GameObjects.Image).setVisible(false);
            } else if(neighbour.d == 'n') {
              (container.getAt(0) as Phaser.GameObjects.Image).setVisible(false);
              (container.getAt(1) as Phaser.GameObjects.Image).setVisible(false);
              (container.getAt(4) as Phaser.GameObjects.Image).setVisible(false);
              (container.getAt(5) as Phaser.GameObjects.Image).setVisible(false);
            } else if(neighbour.d == 's') {
              (container.getAt(8) as Phaser.GameObjects.Image).setVisible(false);
              (container.getAt(9) as Phaser.GameObjects.Image).setVisible(false);
            } else if(neighbour.d == 'sw') {
              (container.getAt(6) as Phaser.GameObjects.Image).setVisible(false);
            } else if(neighbour.d == 'se') {
              (container.getAt(7) as Phaser.GameObjects.Image).setVisible(false);
            }
          }
        }
      }
    }
  }

  addShroud() {
    for(var index in Global.tileStates) {
      var tileState = Global.tileStates[index];
    
      if(Util.isVisible(tileState.hexX, tileState.hexY) == false) {
        var pixel = Util.hex_to_pixel(tileState.hexX, tileState.hexY);

        var shroud = new GameImage({
          scene: this,
          x: pixel.x,
          y: pixel.y,
          id: 'shroud' + pixel.x + pixel.y,
          imageName: 'shroud'
        });

        this.add.existing(shroud);

        this.shroudTiles.push(shroud);
      }
    }
  }

  clearShroud() {
    for(var i = 0; i < this.shroudTiles.length; i++) {
      var shroud = this.shroudTiles[i];

      shroud.destroy();
    }

    this.shroudTiles = [];
  }

  updateImage(objectState: ObjectState) {
    var image = this.objectList[objectState.id] as GameImage;
    var pixel = Util.hex_to_pixel(objectState.x, objectState.y);

    //Structure construction complete
    if(objectState.state == 'none') {
      if(objectState.image != image.imageName) {
        image.setTexture(objectState.image);
      }
    }

    //Move completed, add tween to new location
    if(image.x != pixel.x || image.y != pixel.y) {
      var tween = this.tweens.add({
        targets: image,
        x: pixel.x,
        y: pixel.y,
        ease: 'Power1',
        duration: 500,
        onComplete: this.onMoveComplete
      });

      tween.play();
    }
  }

  updateSprite(objectState: ObjectState) {
    var sprite = this.objectList[objectState.id] as GameSprite;
    var pixel = Util.hex_to_pixel(objectState.x, objectState.y);

    if(objectState.state == 'moving') {
      sprite.play(objectState.image + '_moving');  
      sprite.x = pixel.x;
      sprite.y = pixel.y;
    } else {
      var animState;
      var anim;

      if(objectState.state == DEAD && objectState.prevstate != DEAD) {
        animState = 'die';
      } else {
        animState = objectState.state;
      }

      anim = objectState.image + '_' + animState;

      if(this.anims.exists(anim)) {
        sprite.play(anim);
      }

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
  }

  updateContainer(objectState: ObjectState) {

    //Structure construction complete
    if(objectState.state == 'none') {
      var multiImageList = this.multiImages[objectState.image];
      var container = this.objectList[objectState.id] as GameContainer;
      container.removeAll();
      
      for(var i = 0; i < multiImageList.length; i++) {
        var multiImage = multiImageList[i] as MultiImage;

        var image = new GameImage({
          scene: this,
          x: -1 * multiImage.regX,
          y: -1 * multiImage.regY,
          id: objectState.id,
          imageName: multiImage.key
        });
        container.add(image);
      }

      if(objectState.subclass == WALL) {
       this.wallList.push(objectState);
      }
    }
  }

  addSprite(objectState : ObjectState) {
    var pixel = Util.hex_to_pixel(objectState.x, objectState.y);
    var imageName = objectState.image;

    var sprite = new GameSprite({
      scene: this,
      x: pixel.x,
      y: pixel.y,
      id: objectState.id,
      imageName: imageName
    });
 
    sprite.setDepth(2);

    this.add.existing(sprite);

    var anim = objectState.image + '_' + objectState.state;

    if(this.anims.exists(anim)) {
      sprite.anims.play(anim);
    }

    this.objectList[objectState.id] = sprite;

    if(objectState.subclass == 'hero') {
      var mapScene = this.scene.get('MapScene') as MapScene;
      mapScene.cameras.main.centerOn(sprite.x + 36, sprite.y + 36);
      this.cameras.main.centerOn(sprite.x + 36, sprite.y + 36);
   }
  }

  addLoadedSprites(imageName) {
    var spritesToAdd = this.imageDefTasks.filter(obj => obj.image === imageName);

    for(var i = 0; i < spritesToAdd.length; i++) {
      var spriteObj = spritesToAdd[i];
      this.addSprite(spriteObj);
    }
  }

  createSpriteAnimation(imageName) {
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
  }

  addImage(objectState : ObjectState) {
    var pixel = Util.hex_to_pixel(objectState.x, objectState.y);
    var imageName = '';

    if(objectState.state == FOUNDED) {
      imageName = 'foundation';
    } else {
      imageName = objectState.image;
    }

    var image = new GameImage({
      scene: this,
      x: pixel.x,
      y: pixel.y,
      id: objectState.id,
      imageName: imageName
    });
  
    if(objectState.class == 'structure') {
      image.setDepth(1);
    } else {
      image.setDepth(2);
    }

    this.add.existing(image);

    this.objectList[objectState.id] = image;
  }

  addLoadedImages(imageName) {
    var imagesToAdd = this.imageDefTasks.filter(obj => obj.image === imageName);

    for(var i = 0; i < imagesToAdd.length; i++) {
      var imageObj = imagesToAdd[i];
      this.addImage(imageObj);
    }
  }

  addContainer(objectState : ObjectState) {
    var pixel = Util.hex_to_pixel(objectState.x, objectState.y);

    var container = new GameContainer({
        scene: this,
        x: pixel.x,
        y: pixel.y,
        id: objectState.id,
        containerName: objectState.image
    });

    if(objectState.class == 'structure') {
      container.setDepth(1);
    } else {
      container.setDepth(2);
    }

    this.add.existing(container);

    this.objectList[objectState.id] = container;

    if(objectState.state == FOUNDED) {
      var image = new GameImage({
        scene: this,
        x: 0,
        y: 0,
        id: objectState.id,
        imageName: "foundation"
      });

      container.add(image);
    } else {

      var multiImageList = this.multiImages[objectState.image];

      for(var i = 0; i < multiImageList.length; i++) {
        var multiImage = multiImageList[i] as MultiImage;

        var image = new GameImage({
          scene: this,
          x: -1 * multiImage.regX,
          y: -1 * multiImage.regY,
          id: objectState.id,
          imageName: multiImage.key
        });
        container.add(image);
      }

      if(objectState.subclass == WALL) {
       this.wallList.push(objectState);
      } 
    }
 }

  addLoadedContainerImages(imageName) {
    var containersToAdd = this.imageDefTasks.filter(obj => obj.image === imageName);

    for(var i = 0; i < containersToAdd.length; i++) {
      var containerObj = containersToAdd[i];
      this.addContainer(containerObj);
    }
  }

  fileLoadComplete(key, type, raw) {
    console.log('Loaded file: ' + key + ', ' + type);
    var imageName = key;
    var imageType = Util.getImageType(imageName);

    if(imageType == SPRITE) {
      this.createSpriteAnimation(imageName);
      this.addLoadedSprites(imageName);
    } else if(imageType == IMAGE) {
      this.addLoadedImages(imageName);
    } else if(imageType == CONTAINER) {
      //this.addLoadedContainers(imageName);
    }

  }

  loadComplete() {
    console.log('loadComplete')
    for(var i = 0; i < this.containerTasks.length; i++) {
      var containerName = this.containerTasks[i];

      var containersToAdd = this.imageDefTasks.filter(obj => obj.image === containerName);

      for(var j = 0; j < containersToAdd.length; j++) {
        var objState = containersToAdd[j] as ObjectState;
        this.addContainer(objState);
      }
    }

    //Call process wall for images finished loading
    this.processWallList();
  }

  processDmgMessage(message) {
    console.log('Dmg Message: ' + message);
    if(message.sourceid in this.objectList && 
       message.targetid in this.objectList) {
      var source = this.objectList[message.sourceid] as GameSprite;
      var target = this.objectList[message.targetid];

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

      var dmgText = this.add.text(target.x + 36, target.y - 5, message.dmg, { fontFamily: 'Verdana', fontSize: 24, color: '#FF0000' });
      dmgText.setDepth(10);
      dmgText.setOrigin(0.5, 0.5);

      var textTween = this.tweens.add({
        targets: dmgText,
        alpha: 0,
        ease: 'Power1',
        duration: 5000,
        onComplete: this.onDmgTextComplete
      });

      textTween.play();

      //TODO Check subclass 
      if(message.state == 'dead') {
        //Set object state to dead because an update is not sent to save on messages
        Global.objectStates[message.targetid].state = DEAD;

        target.play(target.imageName + '_die');
      }
    }
  }

  onJumpComplete(tween, targets) {
    var objectState = Global.objectStates[targets[0].id];
    var origin = Util.hex_to_pixel(objectState.x, objectState.y); 

    var returnTween = this.tweens.add({
        targets: targets[0],
        x: origin.x,
        y: origin.y,
        ease: 'Power2',
        duration: 200,
      });

    returnTween.play();
  }

  onMoveComplete(tween, targets) {
    var sprite = targets[0]
    var objectState = Global.objectStates[sprite.id];

    if(objectState.subclass == HERO) {

      var mapScene = this.scene.get('MapScene') as MapScene;
      mapScene.cameras.main.stopFollow();
      this.cameras.main.stopFollow();

      for(var targetId in Global.objectStates) {
        var otherState = Global.objectStates[targetId];

        if(Util.isVisible(otherState.x, otherState.y) == false) {
          var otherSprite = this.objectList[targetId];
          otherSprite.destroy();          
        }

      }
    } else if(objectState.player != Global.playerId) {

      if(Util.isVisible(objectState.x, objectState.y) == false) {
        sprite.destroy();
      }
    }
  }

  onDmgTextComplete(tween, targets){
    targets[0].destroy();
  }

  processSpeech(message) {
    var objectState = Global.objectStates[message.source];
    var source = Util.hex_to_pixel(objectState.x, objectState.y);
    var graphics = this.add.graphics()
    var container = this.add.container(source.x - 24, source.y - 20);

    var speechText = this.add.text(60, 20, message.text, { fontFamily: 'Alegreya', fontSize: 14, color: '#FFFFFF' });
    speechText.setWordWrapWidth(120);
    speechText.setOrigin(0.5, 0.5);
    speechText.setAlign('center');

    container.add(graphics);
    container.add(speechText);
    container.setDepth(20);

    graphics.fillStyle(0x000000, 0.50);
    graphics.fillRoundedRect(0, 
                             0,
                             120,
                             40,
                             5);

    if(message.text.length < 5) {
      graphics.setVisible(false);
    }

    var textTween = this.tweens.add({
      targets: container,
      alpha: 0,
      ease: 'Power1',
      delay: 5000,
      duration: 5000,
      onComplete: this.onSpeechComplete
    });

    textTween.play();
  }

  onSpeechComplete(tween, targets) {
    targets[0].destroy();
  }

}