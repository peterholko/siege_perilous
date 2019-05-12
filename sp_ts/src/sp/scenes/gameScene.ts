/**
 * @author       Peter Holko
 * @copyright    2018 - 2019 Peter Holko
 */

import { Util } from '../util';
import { GlobalVars } from '../globalvars';
import { Tile } from '../objects/tile';
import { GameSprite } from '../objects/gameSprite';

export class GameScene extends Phaser.Scene {
  private map : Phaser.GameObjects.Container;
  private ui : Phaser.GameObjects.Container;
  private imageDefList = [];
  private spriteTasks = [];

  private selectHex : Phaser.GameObjects.Image;

  private selectedObjectId = -1;

  private objectStates = [];

  constructor() {
    super({
      key: "GameScene"
    });
  }

  preload(): void {
    console.log('Preload');
    this.load.once('filecomplete', this.tilesetComplete, this)
    this.load.json('tileset', './static/tileset.json');

    this.load.image('playerframe', './static/art/ui/playerframe.png');
    this.load.image('targetframe', './static/art/ui/targetframe.png');
    this.load.image('framemask', './static/art/ui/framemask.png');
    this.load.image('movecompass', './static/art/ui/movecompass.png');
    this.load.image('selecthex', './static/art/hover-hex.png');
  }

  tilesetComplete(): void {
    console.log('Tileset Complete');
    let tileset = this.cache.json.get('tileset');
    console.log(tileset);
  
    for(var key in tileset) {
      var tile = tileset[key];
      this.load.image('tileset' + tile.tile, './static/art/' + tile.image);
    }
  }

  create(): void {
    console.log('Create');

    var playerFrame = new Phaser.GameObjects.Image(this, 10, 10, 'playerframe');
    playerFrame.setOrigin(0); 

    var targetFrame = new Phaser.GameObjects.Image(this, 420, 10, 'targetframe');
    targetFrame.setOrigin(0); 

    var moveCompass = new Phaser.GameObjects.Image(this, 10, 225, 'movecompass').setInteractive();
    moveCompass.setOrigin(0);

    moveCompass.on('pointerdown', function(pointer, localX, localY) {
      console.log('MoveCompass Click');
      console.log(pointer.x + ', ' + pointer.y);
      console.log(localX + ', ' + localY);

      var centerX = moveCompass.width / 2;
      var centerY = moveCompass.height / 2;

      var angleRads = Math.atan2(localX - centerX, localY - centerY)
      var angleDegrees = ((angleRads * 180) / Math.PI) + 180;

      console.log('Angle: ' + angleDegrees);

      if(angleDegrees < 30 || angleDegrees >= 330) {
        console.log('N');
      } else if(angleDegrees < 90 && angleDegrees >= 30) {
        console.log('NW');
      } else if(angleDegrees < 150 && angleDegrees >= 90) {
        console.log('SW');
      } else if(angleDegrees < 210 && angleDegrees >= 150) {
        console.log('S');
      } else if(angleDegrees < 270 && angleDegrees >= 210) {
        console.log('SE');
      } else if(angleDegrees < 330 && angleDegrees >= 270) {
        console.log('NE');
      }
      
      /*if(this.selectedObjectId != -1) {
        this.sendMove()
      }*/
    });

    this.selectHex = new Phaser.GameObjects.Image(this, 0, 0, 'selecthex');
    this.selectHex.setOrigin(0);

    this.map = this.add.container(0, 0);
    this.ui = this.add.container(0, 0);

    this.ui.add(playerFrame);
    this.ui.add(targetFrame);
    this.ui.add(moveCompass);

    this.map.add(this.selectHex);
  }

  update() : void {
    if(GlobalVars.messages.length > 0) {
      var message = GlobalVars.messages.shift();
      //console.log(message);
      
      if(message.packet == 'perception') {
        this.drawMap(message.data.map);
        this.drawObjects(message.data.objs);
      } else if (message.packet == 'changes') {
        this.processChangeEvents(message);
      } else if(message.packet == 'image_def') {
        this.processImageDefMessage(message);
      } 
    }
  }

  processChangeEvents(message) {
    console.log("updateObj");
    var events = message.events;

    //Reset the operation
    for(var objectId in this.objectStates) {
        var objectState = this.objectStates[objectId];
        objectState.op = 'none';
    }

    for(var i = 0; i < events.length; i++) {
        var eventType = events[i].event;

        if(eventType == "obj_create") {
            var obj = events[i].obj;

            this.objectStates[obj.id] = obj;
            this.objectStates[obj.id].op = 'added';
            
        } else if(eventType == "obj_update") {
            var obj_id = events[i].obj_id;
            var attr = events[i].attr;
            var value = events[i].value;

            if(attr == 'state') {
                this.objectStates[obj_id].state = value;
            }

            this.objectStates[obj_id].op = 'updated';
        } else if(eventType == "obj_move") {
            var obj = events[i].obj;
            var src_x = events[i].src_x;
            var src_y = events[i].src_y;      

            if(obj.id in this.objectStates) {
                this.objectStates[obj.id].state = obj.state;
                this.objectStates[obj.id].x = obj.x;
                this.objectStates[obj.id].y = obj.y;
                this.objectStates[obj.id].op = 'updated';
            } else {
                this.objectStates[obj.id] = obj;
                this.objectStates[obj.id].eventType = 'obj_move';
                this.objectStates[obj.id].prev_x = src_x;
                this.objectStates[obj.id].prev_y = src_y;
                this.objectStates[obj.id].op = 'added';
            }            
        } else if(eventType =="obj_delete") {            
            var obj_id = events[i].obj_id;

            this.objectStates[obj_id].op = 'deleted';
        } 
    }
};

  processImageDefMessage(message) {
    console.log('image_def')
    
    if(message.result != '404') {
      console.log(message.name);
      console.log(message.data);
      this.imageDefList[message.name] = message.data;

      if(Array.isArray(message.data.images)) {
        console.log(message.data.images);
        //TODO handle stockade type images
      } else {
        console.log(message.name);
        this.load.spritesheet(message.name, './static/art/' + message.name + '.png',
                              {frameWidth: message.data.frames.width, 
                                frameHeight: message.data.frames.height})
                              .once('filecomplete', this.loadSpriteSheet, this);
        this.load.start();
      }
    }
  }

  drawMap(map) : void {
    for(var key in map) {
      var tile = map[key];
      console.log(tile);

      var pixel = Util.hex_to_pixel(tile.x, tile.y);
      var imageName = 'tileset'  + tile.t[0];
      console.log(imageName);

      var mapTile = new Tile({
        scene: this,
        x: pixel.x,
        y: pixel.y,
        key: 'tileset' + tile.t[0]
      })

      var _this = this;

      mapTile.setInteractive();
      mapTile.on('pointerdown', function(pointer, localX, localY, event) {
        console.log(this.x + ', ' + this.y);
        console.log(this.depth);

        _this.selectHex.x = this.x;
        _this.selectHex.y = this.y;
        
        _this.map.moveTo(_this.selectHex, _this.map.list.length - 1);


      });

      this.map.add(mapTile);
    }
  }

  drawObjects(objects) : void {
    console.log(objects);
    for(var key in objects) {
      var obj = objects[key];
      console.log(obj);

      if(this.textures.exists(obj.image)) {
        this.addSprite(obj);
      } else {
        var getImage = '{"cmd": "image_def", "name": "' + obj.image + '"}';
        GlobalVars.socket.sendMessage(getImage);
        console.log(getImage);

        this.spriteTasks.push(obj);
      }
    }
  }

  addSprite(obj) {
    var pixel = Util.hex_to_pixel(obj.x, obj.y);

    var sprite = new GameSprite({
      scene: this,
      x: pixel.x,
      y: pixel.y,
      id: obj.id,
      name: obj.name,
      player: obj.player,
      class: obj.class,
      subclass: obj.subclass,
      template: obj.template,
      state: obj.state,
      vision: obj.vision,
      hsl: obj.hsl,
      hexX: obj.x,
      hexY: obj.y,
      imageName: obj.image
    });

    var animName = sprite.imageName + '_' + sprite.state;

    console.log(animName);
    sprite.anims.play(animName);

    this.map.add(sprite);

    if(sprite.subclass.search('hero') != -1) {
      this.selectedObjectId = obj.id;
      
      console.log('hero x: ' + pixel.x + ', ' + pixel.y)

      this.map.x = -1 * pixel.x + GlobalVars.gameWidth / 2 - 36;
      this.map.y = -1 * pixel.y + GlobalVars.gameHeight / 2 - 36;
    }
  }

  addLoadedSprites(imageName) {
    console.log(this.spriteTasks);
    var spritesToAdd = this.spriteTasks.filter(obj => obj.image === imageName);


    for(var i = 0; i < spritesToAdd.length; i++) {
      var spriteObj = spritesToAdd[i];
      console.log(spriteObj);
      this.addSprite(spriteObj);
    }
  }

  loadSpriteSheet(imageName) {
    console.log('Loading spritesheet: ' + imageName);

    if('animations' in this.imageDefList[imageName]) {

      var animsData = this.imageDefList[imageName].animations;

      for(var animName in animsData) {
        var anim = animsData[animName];
        //console.log(animName);
        //console.log(anim);

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
        key: imageName + '_' + 'none',
        frames: this.anims.generateFrameNumbers(imageName, { start: 0, end: 0}),
        repeat: 1,
        frameRate: 1
      }

      this.anims.create(configNone);
    }

    this.addLoadedSprites(imageName);
  }

  sendMove(newX, newY) {
    var move = '{"cmd": "move_unit", "id": ' + this.selectedObjectId + ', "x": ' + newX + ', "y": ' + newY + '}';
    GlobalVars.socket.sendMessage(move);
  }


  createMoveCompass() : void {
    
  }
}
