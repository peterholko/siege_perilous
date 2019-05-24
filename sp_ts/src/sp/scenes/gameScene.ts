/**
 * @author       Peter Holko
 * @copyright    2018 - 2019 Peter Holko
 */

import { Util } from '../util';
import { GlobalVars } from '../globalvars';
import { Tile } from '../objects/tile';
import { GameSprite } from '../objects/gameSprite';
import { GameEvent } from '../gameEvent';
import { ObjectState } from '../objectState';

export class GameScene extends Phaser.Scene {
  private map : Phaser.GameObjects.Container;
  private imageDefList = [];
  private spriteTasks = [];

  private selectHex : Phaser.GameObjects.Image;
  private selectedObjectId = "-1";

  private updateQueue = [];
  private updateTrigger = false;
  private updateTimer;

  public objectList = {};
  public mapTiles = {};
  
  public objectStates : Record<string, ObjectState> = {};
  public heroId = "-1";

  constructor() {
    super({
      key: "GameScene"
    });
  }

  preload(): void {
    console.log('Preload');
    this.load.once('filecomplete', this.tilesetComplete, this)
    this.load.json('tileset', './static/tileset.json');

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
    this.updateTimer = this.time.addEvent({ delay: 200, callback: this.processUpdate, callbackScope: this, loop: true });

    this.selectHex = new Phaser.GameObjects.Image(this, 0, 0, 'selecthex');
    this.selectHex.setOrigin(0);

    this.map = this.add.container(0, 0);
    this.map.add(this.selectHex);

    var _this = this;

    this.input.on('gameobjectdown', function(pointer, gameObject) {
      console.log(gameObject);
       
      _this.selectHex.x = gameObject.x;
      _this.selectHex.y = gameObject.y;

      GlobalVars.gameEmitter.emit(GameEvent.TILE_CLICK, gameObject);

      _this.map.moveTo(_this.selectHex, _this.map.list.length - 1);
    });

  }

  update() : void {
    if(GlobalVars.messages.length > 0) {
      var message = GlobalVars.messages.shift();
      //console.log(message);
      
      if(message.packet == 'perception') {
        console.log('Perception')
        console.log(message.data.objs);
        this.drawMap(message.data.map);

        this.processInitObjStates(message.data.objs);
        this.drawObjects();
      } else if (message.packet == 'changes') {
        console.log('process changes packet')
        console.log(message.events);

        this.updateQueue.push(message.events);
        this.updateTrigger = true;
      } else if(message.packet == 'image_def') {
        this.processImageDefMessage(message);
      } 
    }
  }

  processInitObjStates(objs) {
    for(var i = 0; i < objs.length; i++) {
      var objectState : ObjectState = {
        id: objs[i].id,
        player: objs[i].player,
        name: objs[i].name,
        class: objs[i].class,
        subclass: objs[i].subclass,
        template: objs[i].template,
        state: objs[i].state,
        hexX: objs[i].x,
        hexY: objs[i].y,
        vision: objs[i].vision,
        image: objs[i].image,
        op: 'added'
      }

      console.log(objectState);

      this.objectStates[objectState.id] = objectState; 
    }
  }

  processUpdateObjStates(events) {
    console.log("processUpdateStates");
    console.log(events);

    //Reset the operation
    for(var objectId in this.objectStates) {
        var objectState = this.objectStates[objectId] as ObjectState;
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
                this.objectStates[obj.id].hexX = obj.x;
                this.objectStates[obj.id].hexY = obj.y;
                this.objectStates[obj.id].op = 'updated';
            } else {
                this.objectStates[obj.id] = obj;
                this.objectStates[obj.id].eventType = 'obj_move';
                this.objectStates[obj.id].prevHexX = src_x;
                this.objectStates[obj.id].prevHexY = src_y;
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
        key: 'tileset' + tile.t[0],
        hexX: tile.x,
        hexY: tile.y
      });

      mapTile.setInteractive();

      this.map.add(mapTile);
    }
  }

  /*drawObjects(objects) : void {
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
  }*/

  drawObjects() : void {
    for(var objectId in this.objectStates) {
        var objectState = this.objectStates[objectId] as ObjectState;
        
        if(objectState.op == 'added') {
          console.log('Object Added');

          if(this.textures.exists(objectState.image)) {
            this.addSprite(objectState);
          } else {
            var getImage = '{"cmd": "image_def", "name": "' + objectState.image + '"}';
            GlobalVars.socket.sendMessage(getImage);
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
        }
    }
  }

  processUpdate() : void {
    if(this.updateQueue.length > 0) {
      console.log('Queue Length: ' + this.updateQueue.length);
      this.processUpdateObjStates(this.updateQueue.shift());
      this.drawObjects()
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

    var animName = objectState.image + '_' + objectState.state;

    console.log(animName);
    sprite.anims.play(animName);

    this.map.add(sprite);

    this.objectList[sprite.id] = sprite;

    if(objectState.subclass.search('hero') != -1) {
      this.heroId = objectState.id;
      this.selectedObjectId = objectState.id;
      
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

  public getObjsAt(hexX, hexY) : Array<Object> {
    var objsAt = []

    for(var objId in this.objectStates) {
      var objectState = this.objectStates[objId];

      if(objectState.hexX == hexX && objectState.hexY == hexY) {
        objsAt.push(this.objectList[objId]);
      }
    }

    return objsAt;
  }
}
