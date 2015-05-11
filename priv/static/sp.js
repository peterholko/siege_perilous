var websocket;
var stage;
var loaderQueue;
var imagesQueue = [];
var spritesQueue = [];
var assets = [];
var canvas;
var map;
var battlePanel;
var localPanel;
var infoPanels = [];
var activeInfoPanel;
var dialogPanel;
var selectPanel;
var portraitPanel;

var explored = {};
var objs = {};
var localObjs = {};
var localTiles = [];
var units = {};
var battles = [];
var battleUnits = [];

var playerId;
var playerPos;
var heroPos;

var selectedPortrait = false;
var selectedUnit = false;

var attackToggled = false;
var buildButton = false;

var origX;
var origY;
var pressmove = false;

var mapWidth = 4;
var mapHeight = 4;
var hexSize = 72;
var stageWidth = 1280;
var stageHeight = 800;

var infoPanelBg = new Image();
var dialogPanelBg = new Image();
var selectPanelBg = new Image();
var close_rest = new Image();
var selectHexImage = new Image();
var selectIconImage = new Image();
var actionBarBgImage = new Image();
var portraitBg = new Image();

var attackActive = new Image();
var attackRest = new Image();
var attackRoll = new Image();

var detailsActive = new Image();
var detailsRest = new Image();
var detailsRoll = new Image();

var buttonRestImg = new Image();
var buttonHoverImg = new Image();
var buttonClickedImg = new Image();

var btnBuildRestImg = new Image();
var btnBuildClickedImg = new Image();

var h1Font = "14px Verdana"
var textColor = "#FFFFFF";

var tl = new Image();
var tr = new Image();
var l = new Image();
var r = new Image();
var br = new Image();
var bl = new Image();

tl.src = "/static/art/regular-concave-tl.png";
tr.src = "/static/art/regular-concave-tr.png";
l.src = "/static/art/regular-concave-l.png";
r.src = "/static/art/regular-concave-r.png";
br.src = "/static/art/regular-concave-br.png";
bl.src = "/static/art/regular-concave-bl.png";

var tileImages = [];

tileImages[0] = "/static/art/basic-tile.png";
tileImages[1] = "/static/art/desert.png";
tileImages[2] = "/static/art/green.png";
tileImages[3] = "/static/art/regular.png";

var tileset;

var shroud = "/static/art/shroud.png";

infoPanelBg.src = "/static/art/ui_pane.png";
dialogPanelBg.src = "/static/art/dialog.png";
selectPanelBg.src = "/static/art/selectpanel.png";
close_rest.src = "/static/art/close_rest.png";
selectHexImage.src = "/static/art/hover-hex.png";
selectIconImage.src = "/static/art/select2.png";
portraitBg.src = "/static/art/selected_bg.png";
actionBarBgImage.src = "/static/art/actionbar_bg.png";

attackActive.src = "/static/art/ActionBar_Attack_Active.png";
attackRest.src = "/static/art/ActionBar_Attack_Rest.png";
attackRoll.src = "/static/art/ActionBar_Attack_Roll.png";

detailsActive.src = "/static/art/ActionBar_Details_Active.png";
detailsRest.src = "/static/art/ActionBar_Details_Rest.png";
detailsRoll.src = "/static/art/ActionBar_Details_Roll.png";

btnBuildRestImg.src = "/static/art/ButtonBuildRest.png";
btnBuildClickedImg.src = "/static/art/ButtonBuildClicked.png";

var zombie = {
    images: ['/static/art/zombie_ss.png'],
    frames: {width:72, height:72},
    animations: {
        die:[0,3,"dead", 0.05],
        dead:[3],
        stand:[4]
    }
};

var zombieSS;

$(document).ready(init);

function init() {
    $('body').on('contextmenu', '#map', function(e){ return false; });
    $('#map').css('background-color', 'rgba(0, 0, 0, 1)');
    $("#map").hide();
    $("#navigation").hide();

    $.getJSON("/static/tileset.json", function(data) {
        tileset = data.reverse();
    });

    canvas = document.getElementById("map");

    stage = new createjs.Stage(canvas);
    stage.autoClear = true;

    map = new createjs.Container();
    map.x = $("#map").width() / 2;
    map.y = $("#map").height() / 2;

    stage.addChild(map)

    zombieSS = new createjs.SpriteSheet(zombie);
 
    initImages();
    initUI();

    createjs.Ticker.timingMode = createjs.Ticker.RAF_SYNCHED;
    createjs.Ticker.setFPS(30);
    createjs.Ticker.addEventListener("tick", stage);

    $('#server').val("ws://" + window.location.host + "/websocket");
    if(!("WebSocket" in window)){  
        $('#status').append('<p><span style="color: red;">websockets are not supported </span></p>');
        $("#actions").hide();  
    } else {
        $('#status').append('<p><span style="color: green;">websockets are supported </span></p>');
        connect();
    }
    
    $("#connected").hide(); 	
    $("#content").hide(); 	
};

function initImages() {
    var manifest = [{src: "shroud.png", id: "shroud",
                     src: "white-mage.png", id: "white-mage",
                     src: "ui_pane.png", id: "ui_pane",
                     src: "close_rest.png" , id: "close_rest"}];
                
    loaderQueue = new createjs.LoadQueue(false);
    loaderQueue.on("complete", handleQueueComplete);
    loaderQueue.on("fileerror", handleQueueFileError);
    loaderQueue.on("error", handleQueueError);
    //loaderQueue.on("fileload", handleQueueFileLoad);
    loaderQueue.loadManifest(manifest, true, "/static/art/");
};

function handleQueueFileLoad(event) {
    assets.push(event);    
};

function handleQueueComplete()
{
    while(imagesQueue.length > 0) {
        var imageTask = imagesQueue.shift();
        var image = loaderQueue.getResult(imageTask.id);

        if(image) {
            var bitmap = new createjs.Bitmap(image);

            bitmap.x = imageTask.x;
            bitmap.y = imageTask.y;

            if(imageTask.hasOwnProperty("index")) {
                imageTask.target.addChildAt(bitmap, imageTask.index);
            }
            else {
                imageTask.target.addChild(bitmap);
            }
        }
    }

    while(spritesQueue.length > 0) {
        var spriteTask = spritesQueue.shift();
        var spriteSheet = loaderQueue.getResult(spriteTask.id);
        
        if(spriteSheet) {
            if(spriteSheet.animations.length > 0) {
                var sprite = new createjs.Sprite(spriteSheet, spriteTask.animation);

                sprite.x = spriteTask.x;
                sprite.y = spriteTask.y;
                sprite.name = "sprite";
            
                sprite.gotoAndPlay(spriteTask.animation);
            
                spriteTask.target.addChild(sprite);
            } 
            else {  
                for(var i = 0; i < spriteSheet.getNumFrames(); i++) {
                    var sprite = new createjs.Sprite(spriteSheet);
                    sprite.gotoAndStop(i);

                    sprite.x = spriteTask.x;
                    sprite.y = spriteTask.y;

                    spriteTask.target.addChild(sprite);
                }     
            }
        }
    }

};

function handleQueueFileError(evt) {
    console.log("handleQueueFileError");
};

function handleQueueError(evt) {
    console.log("handleQueueError");
};

function addSprite(spriteTask) {
     var spriteSheet = loaderQueue.getResult(spriteTask.id);

    if(spriteSheet) {
        console.log("Sprite loaded");

        if(spriteSheet.animations.length > 0) {
            var sprite = new createjs.Sprite(spriteSheet, spriteTask.animation);
            //var sprite = new createjs.Sprite(spriteSheet);
            
            sprite.x = spriteTask.x;
            sprite.y = spriteTask.y;
            sprite.name = "sprite";

            sprite.gotoAndPlay(spriteTask.animation);
        
            spriteTask.target.addChild(sprite);
        } 
        else {
            for(var i = 0; i < spriteSheet.getNumFrames(); i++) {
                var sprite = new createjs.Sprite(spriteSheet);
                                
                sprite.gotoAndStop(i);
                sprite.x = spriteTask.x;
                sprite.y = spriteTask.y;

                spriteTask.target.addChild(sprite);
            }
        }
    }
    else {
        spritesQueue.push(spriteTask);
        loaderQueue.loadFile({id: spriteTask.id, src: spriteTask.path, type: "spritesheet"});
    }
};

function addImage(imageTask) {
    var image = loaderQueue.getResult(imageTask.id);

    if(image) {
        var bitmap = new createjs.Bitmap(image);
        
        bitmap.x = imageTask.x;
        bitmap.y = imageTask.y;
        
        if(imageTask.hasOwnProperty("index")) {
            imageTask.target.addChildAt(bitmap, imageTask.index);
        }
        else {
            imageTask.target.addChild(bitmap);
        }        
    }
    else {        
        imagesQueue.push(imageTask);
        loaderQueue.loadFile({id: imageTask.id, src: imageTask.path});        
    }
};

function connect()
{
  wsHost = $("#server").val()
  websocket = new WebSocket(wsHost);
  showScreen('<b>Connecting to: ' +  wsHost + '</b>'); 
  websocket.onopen = function(evt) { onOpen(evt) }; 
  websocket.onclose = function(evt) { onClose(evt) }; 
  websocket.onmessage = function(evt) { onMessage(evt) }; 
  websocket.onerror = function(evt) { onError(evt) }; 
};  

function disconnect() {
  websocket.close();
}; 

function toggle_connection(){
  if(websocket.readyState == websocket.OPEN){
      disconnect();
  } else {
      connect();
  };
};

function sendTxt() {
  if(websocket.readyState == websocket.OPEN){
      txt = $("#send_txt").val();
      websocket.send(txt);
      showScreen('sending: ' + txt); 
  } else {
      showScreen('websocket is not connected'); 
  };
};

function sendLogin() {
    if(websocket.readyState == websocket.OPEN) {
        username = $("#username").val();
        password = $("#password").val();

        var login = '{"cmd": "login", "username": "' + username + 
                    '", "password": "' + password + '"}';

        websocket.send(login);
        showScreen('sending: ' + login);
    }
};

function sendMove(newX, newY) {
    if(localPanel.visible == false) {
        playerObj = getObjByPlayer(playerId);
        
        var cmd = "move";
        var id = playerObj.id;
    }
    else {
        unit = getLocalObj(selectedPortrait);

        var cmd = "move_unit";
        var id = unit.id;
    }
    
    var move = '{"cmd": "' + cmd + '", "id": "' + id + 
        '", "x": ' + newX + ', "y": ' + newY + '}';

    websocket.send(move);
};

function sendBuild() {
    console.log("sendBuild");
    var e = '{"cmd": "build", "sourceid": "' + selectedPortrait + '", "structure": "Stockade"}';
    websocket.send(e);
};

function sendFinishBuild(structureid) {
    var e = '{"cmd": "finish_build", "sourceid": "' + selectedPortrait + '", "structureid": "' + structureid + '"}';
    websocket.send(e);
};

function sendLoot(sourceid, item) {
    var e = '{"cmd": "loot", "sourceid": "' + sourceid + '", "item": "' + item + '"}';
    websocket.send(e);
};

function sendItemTransfer(targetid, item) {
    console.log("targetid: " + targetid);
    var e = '{"cmd": "item_transfer", "targetid": "' + targetid + '", "item": "' + item + '"}';
    websocket.send(e);
};

function sendHarvest(sourceid, resource) {
    var e = '{"cmd": "harvest", "sourceid": "' + selectedPortrait + '", "resource": "Copper Ore"}';    
    websocket.send(e);
};

function sendExplore() {
    var e = '{"cmd": "explore", "sourceid": "123123", "x": 1, "y": 1}';
    websocket.send(e);
};

function sendExitLocal() {
    var e = '{"cmd": "exit_local", "attr": "val"}';
    websocket.send(e);

    showDialogPanel();
    
    var title = new createjs.Text("Leaving Local Area...", h1Font, textColor);
    title.x = Math.floor(dialogPanelBg.width / 2);
    title.y = 20;
    title.textAlign = "center";

    addChildDialogPanel(title);
};

function sendInfoObj(id) {
    var info = '{"cmd": "info_obj", "id": "' + id + '"}';
    websocket.send(info);
};

function sendInfoUnit(id) {
    var info = '{"cmd": "info_unit", "id": "' + id + '"}';
    websocket.send(info);
};

function sendInfoItem(id) {
    var info = '{"cmd": "info_item", "id": "' + id + '"}';
    websocket.send(info);
};

function sendInfoItemByName(name) {
    var info = '{"cmd": "info_item_by_name", "name": "' + name + '"}';
    websocket.send(info);
}

function sendInfoTile(type, pos) {
    var info = '{"cmd": "info_tile", "type": ' + type + ', "pos": ' + pos + '}';
    websocket.send(info);
};

function sendInfoBattle(id) {
    var info = '{"cmd": "info_battle", "id": "' + id + '"}';
    websocket.send(info);
};

function onOpen(evt) { 
  showScreen('<span style="color: green;">CONNECTED </span>'); 
  $("#connected").fadeIn('slow');
  $("#content").fadeIn('slow');
  $("#connecting").hide();
};  

function onClose(evt) { 
  showScreen('<span style="color: red;">DISCONNECTED </span>');
};  

function onMessage(evt) { 
    console.log("Before JSON Parse: "+ evt.data);
    var jsonData = JSON.parse(evt.data);
    console.log("After JSON Parse");

    if(jsonData.hasOwnProperty("packet")) {
        
        if(jsonData.packet == "login") {
            $("#login").hide();        
            $("#navigation").fadeIn('slow');
            $("#map").fadeIn('slow');
            
            playerId = jsonData.player;
            explored = jsonData.explored;
            objs = jsonData.objs;

            setPlayerPos();
            sendExplore();
            //drawMap();
            //drawObjs();
        }
        else if(jsonData.packet == "map_perception") {
            explored = jsonData.explored;
            objs = jsonData.objs;

            setPlayerPos();
            drawMap();
            drawObjs();

        }
        else if(jsonData.packet == "local_perception") {
            drawLocal(jsonData);
        }
        else if(jsonData.packet == "explore") {
            clearLocal();
            drawExplore(jsonData);
            drawLocal(jsonData);
            drawPortraits();
        }
        else if(jsonData.packet == "item_perception") {
            dialogPanel.visible = false;
        }
        else if(jsonData.packet == "item_transfer") {
            if(jsonData.result == "success") {
                for(var i = infoPanels.length - 1; i >= 0; i--) {
                    if(infoPanels[i].hasOwnProperty("unitName")) {
                        infoPanels[i].visible = false;
                        sendInfoUnit(infoPanels[i]._id);
                    }
                }
            }
        }
        else if(jsonData.packet == "new_items") {
           drawNewItemsDialog(jsonData); 
        }
        else if(jsonData.packet == "exit_local") {
            localPanel.visible = false;            
            dialogPanel.visible = false;
        }
        else if(jsonData.packet == "dmg") {
            drawDmg(jsonData);
        }
        else if(jsonData.packet == "info_obj") {
            drawInfoObj(jsonData);
        }
        else if(jsonData.packet == "info_unit") {
            if(jsonData.state == "dead") {
                drawLootDialog(jsonData);
            }
            else {
                drawInfoUnit(jsonData);
            }
        }
        else if(jsonData.packet == "info_item") {
            drawInfoItem(jsonData);
        }
    }

    showScreen('<span style="color: blue;">RESPONSE: ' + evt.data+ '</span>'); 
};

function setPlayerPos() {
    var i;
    playerPos = {}

    for(i = 0; i < objs.length; i++) {

        if(objs[i].player == playerId) {
            playerPos.x = objs[i].x;
            playerPos.y = objs[i].y;
        }
    }
};

function drawMap() {
    var bitmap;
    var neighbours = getNeighbours(playerPos.x, playerPos.y);

    for(var i = 0; i < explored.length; i++) {
        var tile = explored[i];
        
        var pixel = hex_to_pixel(tile.x, tile.y);
        var tileType = tile.t;

        bitmap = new createjs.Bitmap(tileImages[tile.t]);
        bitmap.tile = tile.t;
        bitmap.tileX = tile.x;
        bitmap.tileY = tile.y;
        bitmap.x = pixel.x;
        bitmap.y = pixel.y;
        bitmap.on("mousedown", function(evt) {
            if(evt.nativeEvent.button == 2) {
                sendMove(this.tileX, this.tileY);
            } 
            else {
                var objList = getObjOnTile(this.tileX, this.tileY);
                drawInfoOnTile(this.tile, this.tileX, this.tileY, objList);
            }
        });

        map.addChild(bitmap);

        if(tile.x != playerPos.x || tile.y != playerPos.y) {
            if(!isNeighbour(tile.x, tile.y, neighbours)) {
                
                bitmap = new createjs.Bitmap(shroud);
                bitmap.x = pixel.x;
                bitmap.y = pixel.y;
                map.addChild(bitmap);
            }
        }
    }

    var pixel = hex_to_pixel(1,1);

    var b = new createjs.Bitmap(tr);
    var c = new createjs.Bitmap(tl);
    var d = new createjs.Bitmap(l);
    var e = new createjs.Bitmap(r);
    var f = new createjs.Bitmap(br);
    var g = new createjs.Bitmap(bl);

    b.x = pixel.x;
    b.y = pixel.y - 72;

    c.x = pixel.x - 54;
    c.y = pixel.y - 108;

    d.x = pixel.x - 54;
    d.y = pixel.y - 36;

    e.x = pixel.x;
    e.y = pixel.y - 72;

    f.x = pixel.x;
    f.y = pixel.y;

    g.x = pixel.x - 54;
    g.y = pixel.y - 36;

    map.addChild(b);
    map.addChild(c);
    map.addChild(d);
    map.addChild(e);
    map.addChild(f);
    map.addChild(g);
};

function drawObjs() {
    var bitmap;
    var c_x;
    var c_y;
    var halfwidth = $("#map").width() / 2;
    var halfheight = $("#map").height() / 2;

    for(i = 0; i < objs.length; i++) {
        var pixel = hex_to_pixel(objs[i].x, objs[i].y);
        var objName = objs[i].type;
        if(objs[i].state != "dead") {
            var imagePath =  "/static/art/" + objName + ".png";
            var imageContainer = new createjs.Container();

            imageContainer.x = pixel.x;
            imageContainer.y = pixel.y;

            map.addChild(imageContainer);

            imagesQueue.push({id: objName, x: 0, y: 0, target: imageContainer});
            loaderQueue.loadFile({id: objName, src: imagePath});
        }

        if(objs[i].player == playerId) {
            c_x = halfwidth - 36 - pixel.x;
            c_y = halfheight - 36 - pixel.y;
            createjs.Tween.get(map).to({x: c_x, y: c_y}, 500, createjs.Ease.getPowInOut(2))
        }
    }
};

function clearLocal() {
    var localMapCont = localPanel.getChildByName("localMap");
    var localTilesCont = localMapCont.getChildByName("localTiles");
    var localShroudCont = localMapCont.getChildByName("localShroud");
    var localObjsCont = localMapCont.getChildByName("localObjs"); 
    var selectHex = localMapCont.getChildByName("selectHex");
    
    localTilesCont.removeAllChildren();
    localShroudCont.removeAllChildren();
    localObjsCont.removeAllChildren();
    
    selectHex.visible = false;
};

function drawExplore(jsonData) {
    var localMapCont = localPanel.getChildByName("localMap");

    for(var i = 0; i < jsonData.objs.length; i++) {
        var obj = jsonData.objs[i];
        var pixel = hex_to_pixel(obj.x, obj.y);

        if(obj.player == playerId) {
            if(is_hero(obj.type)) {
                c_x = 640 - 36 - pixel.x;
                c_y = 400 - 36 - pixel.y;

                localMapCont.x = c_x;
                localMapCont.y = c_y;
            }
        }
    }
};

function drawLocal(jsonData) {
    showLocalPanel();

    var localMapCont = localPanel.getChildByName("localMap");
    var localTilesCont = localMapCont.getChildByName("localTiles");
    var localShroudCont = localMapCont.getChildByName("localShroud");
    var localObjsCont = localMapCont.getChildByName("localObjs"); 
    var selectHex = localMapCont.getChildByName("selectHex");

    for(var i = 0; i < jsonData.explored.length; i++) {
        var tile = jsonData.explored[i];
        var pixel = hex_to_pixel(tile.x, tile.y);
        var tiles = tile.t.reverse();

        var icon = new createjs.Container();

        icon.x = pixel.x;
        icon.y = pixel.y;
        icon.tileX = tile.x;
        icon.tileY = tile.y;
        icon.on("mousedown", function(evt) {
            if(evt.nativeEvent.button == 2) {
                console.log("Right click");
                
                if(selectedPortrait != false) {
                    sendMove(this.tileX, this.tileY);
                }
            }
            else {
                selectHex.x = this.x;
                selectHex.y = this.y;
                selectHex.visible = true;

                drawLocalSelectPanel(this.tileX, this.tileY);
            }
        });

        addChildLocalMap(icon, "localTiles");

        for(var j = 0; j < tiles.length; j++) {
            var tileImageId = tiles[j] - 1;
            var imagePath = "/static/" + tileset[tileImageId].image;
            var offsetX = tileset[tileImageId].offsetx;
            var offsetY = -1 * tileset[tileImageId].offsety;
         
            addImage({id: tileImageId, path: imagePath, x: offsetX, y: offsetY, target: icon, index: j});
        }
    
        addLocalTile(tile);
    }
 
    localObjsCont.removeAllChildren();

    var visibleTiles = [];

    for(var i = 0; i < jsonData.objs.length; i++) {
        var obj = jsonData.objs[i];
        var pixel = hex_to_pixel(obj.x, obj.y);
        var unitName = obj.type;
        unitName = unitName.toLowerCase().replace(/ /g, '');
        var imagePath =  "/static/art/" + unitName + ".json";
        var icon = new createjs.Container();

        icon.x = pixel.x;
        icon.y = pixel.y;
        icon.player = obj.player;
        icon.name = unitName;
        icon.id = obj.id;
        
        addChildLocalMap(icon, "localObjs");

        if(obj.player == playerId) {

            visibleTiles = range(obj.x, obj.y, 2);

            if(is_hero(obj.type)) {
                c_x = 640 - 36 - pixel.x;
                c_y = 400 - 36 - pixel.y;

                createjs.Tween.get(localMapCont).to({x: c_x, y: c_y}, 500, createjs.Ease.getPowInOut(2));
            }
        }

        addSprite({id: unitName + "_ss", path: imagePath, x: 0, y: 0, target: icon, animation: obj.state}); 

        obj.icon = icon;
        localObjs[obj.id] = obj;
    }

    localShroudCont.removeAllChildren();

    for(var tileKey in localTiles) {
        var tile = localTiles[tileKey];

        if(!is_visible(tile.x, tile.y, visibleTiles)) {
            var pixel = hex_to_pixel(tile.x, tile.y);
            var bitmap = new createjs.Bitmap(shroud);
            bitmap.x = pixel.x;
            bitmap.y = pixel.y;
            localShroudCont.addChild(bitmap);
        }
    }

};

function drawLocalSelectPanel(tileX, tileY) {
    var tile = getLocalTile(tileX, tileY);
    var localObjs = getLocalObjsAt(tileX, tileY);
    var icons = [];

    var content = selectPanel.getChildByName("content");
    content.removeAllChildren();

    for(var i = 0; i < localObjs.length; i++) {
        var unitName = localObjs[i].type;
        unitName = unitName.toLowerCase().replace(/ /g, '');
        var imagePath =  "/static/art/" + unitName + ".png";

        var icon = new createjs.Container();
        var selectIcon = new createjs.Bitmap(selectIconImage);

        icon.x = 0; 
        icon.y = i * 75;
        icon.id = localObjs[i].id;
        icon.mouseChildren = false;

        selectIcon.x = 7;
        selectIcon.y = 7;
        selectIcon.name = "selectIcon";
        selectIcon.visible = false; 
        
        icon.addChild(selectIcon);

        icon.on("mousedown", function(evt) {
            for(var i = 0; i < icons.length; i++) {
                var selectIcon = icons[i].getChildByName("selectIcon");
                selectIcon.visible = false;
            }

            var selectIcon = this.getChildByName("selectIcon");
            selectIcon.visible = true;

            selectedUnit = this.id;

            if(attackToggled) {
                var attack_unit = '{"cmd": "attack_unit", "sourceid": "' + selectedPortrait + '", "targetid": "' + selectedUnit + '"}';    
                websocket.send(attack_unit);

                attackToggled = false;
            }
        });
        
        content.addChild(icon);
        addImage({id: unitName, path: imagePath, x: 0, y: 0, target: icon});
        
        icons.push(icon); 
    }
};

function drawPortraits() {
    var numPortraits = 0;
    var portraits = [];

    for(var localObjId in localObjs) {
        var objName = localObjs[localObjId].type;
        objName = objName.toLowerCase().replace(/ /g, '');
        var imagePath =  "/static/art/" + objName + ".png";

        if(playerId == localObjs[localObjId].player) {
            var portrait = new createjs.Container();
            var bg = new createjs.Bitmap(portraitBg);
            var selectPortrait = new createjs.Bitmap(selectIconImage);
            
            bg.x = 0;
            bg.y = 0;

            selectPortrait.x = 7;
            selectPortrait.y = 7;

            selectPortrait.name = "selectPortrait";

            if(numPortraits == 0) {
                selectPortrait.visible = true;
                selectedPortrait = localObjId;
            } 
            else {
                selectPortrait.visible = false;
            }

            portrait.addChild(bg);
            portrait.addChild(selectPortrait);

            portrait.x = numPortraits * 84 + 75;
            portrait.y = 0;
            portrait.id = localObjId;

            portrait.on("mousedown", function(evt) {                 
                for(var i = 0; i < portraits.length; i++) {
                    var selectPortrait = portraits[i].getChildByName("selectPortrait");
                    selectPortrait.visible = false;
                }

                var selectPortrait = this.getChildByName("selectPortrait");
                selectPortrait.visible = true;

                selectedPortrait = this.id;
            });

            portraitPanel.addChild(portrait);

            addImage({id: objName, path: imagePath, x: 0, y: 0, target: portrait});

            portraits.push(portrait);
            numPortraits += 1;
        }
    } 
};

function drawDmg(jsonData) {
    if(localPanel.visible) {
        var source = getLocalObj(jsonData.sourceid);
        var target = getLocalObj(jsonData.targetid);
        var origX = source.icon.x;
        var origY = source.icon.y;

        if(source && target) {
            createjs.Tween.get(source.icon).to({x: target.icon.x, y: target.icon.y}, 500, createjs.Ease.getPowInOut(4))
                                           .to({x: origX, y: origY}, 100, createjs.Ease.getPowInOut(2));
        }

        if(jsonData.state == "dead") {
            var sprite = target.icon.getChildByName("sprite");
            sprite.gotoAndPlay("die");
        }
    }
};

function drawLootDialog(jsonData) {
    showDialogPanel();

    var title = new createjs.Text("Loot", h1Font, textColor);
    title.x = Math.floor(dialogPanelBg.width / 2);
    title.y = 5;
    title.textAlign = "center";

    addChildDialogPanel(title);

    for(var i = 0; i < jsonData.items.length; i++) {
        var itemName = jsonData.items[i].name;            
        itemName = itemName.toLowerCase().replace(/ /g,'');

        var imagePath = "/static/art/" + itemName + ".png";
        var icon = new createjs.Container();
        
        icon._id = jsonData.items[i]._id;
        icon.on("mousedown", function(evt) {
            if(evt.nativeEvent.button == 2) {
                console.log("Sending loot");
                sendLoot(selectedPortrait, this._id);
            }
            else {
                sendInfoItem(this._id);
            }
        });

        icon.x = dialogPanelBg.width / 2 - 24;
        icon.y = dialogPanelBg.height / 2 + 5 - 24;

        addChildDialogPanel(icon);
        addImage({id: itemName, path: imagePath, x: 0, y: 0, target: icon});
    }

};

function drawNewItemsDialog(jsonData) {
    showDialogPanel();

    var title = new createjs.Text("Rewards", h1Font, textColor);
    title.x = Math.floor(dialogPanelBg.width / 2);
    title.y = 5;
    title.textAlign = "center";

    addChildDialogPanel(title);

    for(var i = 0; i < jsonData.items.length; i++) {
        var itemName = jsonData.items[i].name;            
        itemName = itemName.toLowerCase().replace(/ /g,'');

        var imagePath = "/static/art/" + itemName + ".png";
        var icon = new createjs.Container();
       
        icon.id = jsonData.items[i]._id;
        icon.itemName = jsonData.items[i].name;

        if(jsonData.items[i].hasOwnProperty("_id")) {
            icon.by_name = false;
        } 
        else {
            icon.by_name = true;
        }

        icon.on("mousedown", function(evt) {
            if(this.by_name) {
                sendInfoItemByName(this.itemName);
            }
            else {
                sendInfoItem(this.id);
            }
        });

        icon.x = dialogPanelBg.width / 2 - 24;
        icon.y = dialogPanelBg.height / 2 + 5 - 24;

        addChildDialogPanel(icon);
        addImage({id: itemName, path: imagePath, x: 0, y: 0, target: icon});
    }

};

function drawInfoOnTile(tileType, tileX, tileY, objsOnTile) {
    showInfoPanel();

    var bandText = new createjs.Text("(" + tileX + ", " + tileY + ")", h1Font, textColor);
    bandText.x = Math.floor(infoPanelBg.width / 2);
    bandText.y = 10;
    bandText.textAlign = "center";

    addChildInfoPanel(bandText);

    var tile = new createjs.Bitmap(tileImages[tileType]);
    
    tile.type = tileType;
    tile.tileX = tileX;
    tile.tileY = tileY;
    tile.on("mousedown", function(evt) {
        sendInfoTile(this.type, this.tileX, this.tileY);
    });

    tile.x = (infoPanelBg.width / 2) - 36;
    tile.y = 52;

    addChildInfoPanel(tile);

    for(var i = 0; i < objsOnTile.length; i++) {
    
        var objName = objsOnTile[i].type;
        var imagePath =  "/static/art/" + objName + ".png";
        var icon = new createjs.Container();

        icon.type = objsOnTile[i].type;
        icon.obj_id = objsOnTile[i].id;
        icon.x = i * hexSize;
        icon.y = 130;
        icon.on("mousedown", function(evt) {
            if(this.type == "battle") {
                sendInfoBattle(this.obj_id);
            } else { 
                sendInfoObj(this.obj_id);
            }
        });

        addChildInfoPanel(icon);

        imagesQueue.push({id: objName, x: 0, y: 0, target: icon});
        loaderQueue.loadFile({id: objName, src: imagePath});
    }
};

function drawInfoObj(jsonData) {
    showInfoPanel();

    var bandText = new createjs.Text(jsonData.name, h1Font, textColor);
    bandText.x = Math.floor(infoPanelBg.width / 2);
    bandText.y = 10;
    bandText.textAlign = "center";

    var unitText = new createjs.Text("Units", h1Font, textColor);
    unitText.x = 20;
    unitText.y = 125;

    addChildInfoPanel(bandText);
    addChildInfoPanel(unitText);

    if(jsonData.units.length > 0) {
        for(var i = 0; i < jsonData.units.length; i++) {
            var unitName = jsonData.units[i].name;
            unitName = unitName.toLowerCase().replace(/ /g, '');
            
            var imagePath =  "/static/art/" + unitName + ".png";
            var icon = new createjs.Container();

            icon._id = jsonData.units[i]._id;

            if(jsonData.units[i].hero == true) {
                icon.x = Math.floor(infoPanelBg.width / 2) - hexSize/2;
                icon.y = 40;
            } else {
                icon.x = 20 + i * 50;
                icon.y = 145;
            }

            icon.on("mousedown", function(evt) {
                sendInfoUnit(this._id);
            });

            addChildInfoPanel(icon);

            imagesQueue.push({id: unitName, x: 0, y: 0, target: icon});
            loaderQueue.loadFile({id: unitName, src: imagePath});
        }

        var itemText = new createjs.Text("Items", h1Font, textColor);
        itemText.x = 20;
        itemText.y = 225;

        addChildInfoPanel(itemText);

        for(var i = 0; i < jsonData.items.length; i++) {
            var itemName = jsonData.items[i].name;
            itemName = itemName.toLowerCase().replace(/ /g,'');
            var imagePath = "/static/art/" + itemName + ".png";
            var icon = new createjs.Container();
            
            icon._id = jsonData.items[i]._id;
            icon.on("mousedown", function(evt) {
                sendInfoItem(this._id);
            });

            icon.x = 20 + i * 50;
            icon.y = 250;

            addChildInfoPanel(icon);

            imagesQueue.push({id: itemName, x: 0, y: 0, target: icon});
            loaderQueue.loadFile({id: itemName, src: imagePath});
        }
    }
    else {
        var unitText = new createjs.Text("Unknown", h1Font, textColor);
        unitText.x = 20;
        unitText.y = 145;

        addChildInfoPanel(unitText);    
    }
};

function drawInfoUnit(jsonData) {
    showInfoPanel();

    var unitName = jsonData.name
    activeInfoPanel.unitName = unitName;   
    activeInfoPanel._id = jsonData._id; 

    var nameText = new createjs.Text(unitName, h1Font, textColor);

    var nameBounds = nameText.getBounds();
    nameText.x =  Math.floor(infoPanelBg.width / 2) - nameBounds.width / 2;
    nameText.y = 12;

    addChildInfoPanel(nameText);

    unitName = unitName.toLowerCase().replace(/ /g, '');
    var imagePath =  "/static/art/" + unitName + ".png";

    imagesQueue.push({id: unitName, 
                      x: Math.floor(infoPanelBg.width / 2) - 24, 
                      y: 50, target: getInfoPanelContent()});
    loaderQueue.loadFile({id: unitName, src: imagePath});

    var stats = "State: " + jsonData.state + "\n" 
              + "Hp: " + jsonData.hp + " / " + jsonData.base_hp + "\n"
              + "Damage: " + jsonData.base_dmg + " - " + jsonData.dmg_range + "\n" 
              + "Defense: " + jsonData.base_def + "\n"
              + "Speed: " + jsonData.base_speed + "\n";
           
    var statsText = new createjs.Text(stats, h1Font, textColor);

    statsText.lineHeight = 20;
    statsText.x = 10;
    statsText.y = 125;
    
    addChildInfoPanel(statsText);

    if(jsonData.hasOwnProperty("req")) {
        var req = "Requirements: \n";

        for(var i = 0; i < jsonData.req.length; i++) {
            req += "  " + jsonData.req[i].quantity + " " + jsonData.req[i].type + "\n";
        }

        var reqText = new createjs.Text(req, h1Font, textColor);

        reqText.lineHeight = 20;
        reqText.x = 10;
        reqText.y = 225;

        addChildInfoPanel(reqText);
    }

    var itemText = new createjs.Text("Items: ", h1Font, textColor);
    itemText.x = 10;
    itemText.y = 300;
    
    addChildInfoPanel(itemText);

    for(var i = 0; i < jsonData.items.length; i++) {
        var itemName = jsonData.items[i].name;
        itemName = itemName.toLowerCase().replace(/ /g,'');
        var imagePath = "/static/art/" + itemName + ".png";

        var icon = new createjs.Container();

        icon.x = 10 + i * 50;
        icon.y = 325;
        icon._id = jsonData.items[i]._id;
        icon.owner = jsonData.items[i].owner;

        icon.on("click", function(evt) {
            if(!pressmove) {
                sendInfoItem(this._id);
            }
        });

        icon.on("pressmove", function(evt) {
            pressmove = true;

            evt.target.x = evt.localX - 25;
            evt.target.y = evt.localY - 25;
            
            stage.setChildIndex(this.parent.parent, stage.numChildren - 1);
        });
        icon.on("pressup", function(evt) { 
            pressmove = false;
            
            var transfer = false;

            for(var i = 0; i < infoPanels.length; i++) {
                var pt = infoPanels[i].globalToLocal(evt.stageX, evt.stageY);
                if(infoPanels[i].hitTest(pt.x, pt.y)) {
                    if(infoPanels[i]._id != this.owner) {
                        if(infoPanels[i]._id != undefined) {
                            console.log("Transfering item: " + infoPanels[i]._id, this._id);
                            transfer = true;
                            sendItemTransfer(infoPanels[i]._id, this._id);        
                        }
                    }
                }
            }

            if(!transfer) {
                evt.target.x = 0;
                evt.target.y = 0;
            }
    
        });

        addChildInfoPanel(icon);
        addImage({id: itemName, path: imagePath, x: 0, y: 0, target: icon});
    }

    if(jsonData.hasOwnProperty("req")) {
        var btnBuild = activeInfoPanel.getChildByName("btnBuild");
        btnBuild.visible = true;
        
        btnBuild.on("mousedown", function(evt) {
            console.log("drawInfoUnit btnBuild mousedown");
            sendFinishBuild(activeInfoPanel._id);
        });
    }
};

function drawInfoItem(jsonData) {
    showInfoPanel();

    var itemName = jsonData.name;

    var nameText = new createjs.Text(itemName, h1Font, textColor);
    nameText.x = Math.floor(infoPanelBg.width / 2);
    nameText.y = 10;
    nameText.textAlign = "center";
  
    addChildInfoPanel(nameText);

    itemName = itemName.toLowerCase().replace(/ /g, '');
    var imagePath =  "/static/art/" + itemName + ".png";

    imagesQueue.push({id: itemName, 
                      x: Math.floor(infoPanelBg.width / 2) - 24, 
                      y: 50, target: getInfoPanelContent()});
    loaderQueue.loadFile({id: itemName, src: imagePath});

    var stats = "";

    for(attr in jsonData) {
        if(attr != "_id" && attr != "owner" && attr != "packet") {
            var stat = attr + ": " + jsonData[attr] + "\n";
            stats += stat;
        }
    }

    var statsText = new createjs.Text(stats, h1Font, textColor);

    statsText.lineHeight = 20;
    statsText.x = 10;
    statsText.y = 125;
    
    addChildInfoPanel(statsText);
     
};

function initUI() {
    localPanel = new createjs.Container();
    localPanel.visible = false;
    localPanel.x = 0;
    localPanel.y = 0;

    var bg = new createjs.Shape();
    var close = new createjs.Bitmap(close_rest);
    var localMapCont = new createjs.Container();
    var localTilesCont = new createjs.Container();
    var localObjsCont = new createjs.Container();
    var localShroudCont = new createjs.Container();

    var selectHex = new createjs.Bitmap(selectHexImage);

    localMapCont.width = 1280;
    localMapCont.height = 800;

    bg.graphics.beginFill("#000000").drawRect(0,0,1280,800);

    close.x = 1250;
    close.y = 10;
    close.on("mousedown", function(evt) {
        console.log('Close mousedown')
        this.parent.visible = false;
    });

    localMapCont.name = "localMap";
    localTilesCont.name = "localTiles";
    localShroudCont.name = "localShroud";
    localObjsCont.name = "localObjs";

    
    selectHex.name = "selectHex";
    selectHex.visible = false;

    localPanel.addChild(bg);
    localPanel.addChild(close);
    localPanel.addChild(localMapCont);
    
    localMapCont.addChild(localTilesCont);
    localMapCont.addChild(localShroudCont);
    localMapCont.addChild(localObjsCont);
    localMapCont.addChild(selectHex);

    stage.addChild(localPanel);

   //Initialize dialogPanel
    dialogPanel = new createjs.Container();
    dialogPanel.x = stageWidth / 2 - 200;
    dialogPanel.y = stageHeight / 2 - 150; 
    dialogPanel.visible = false;

    var bg = new createjs.Bitmap(dialogPanelBg);
    var close = new createjs.Bitmap(close_rest);
    var content = new createjs.Container();

    close.x = 385;
    close.y = 10;
    close.on("mousedown", function(evt) {
        this.parent.visible = false;
    });

    content.name = 'content';

    dialogPanel.addChild(bg); 
    dialogPanel.addChild(close);
    dialogPanel.addChild(content);

    stage.addChild(dialogPanel);

    var actionBar = new createjs.Container();
    var actionBarBg = new createjs.Bitmap(actionBarBgImage);
    var detailsButton = new createjs.Container();
    var attackButton = new createjs.Container();

    actionBar.x = stageWidth / 2 - 274 / 2;
    actionBar.y = stageHeight - 184;

    detailsButton.x = 27;
    detailsButton.y = 21;
    detailsButton.mouseChildren = false;
    detailsButton.addChild(new createjs.Bitmap(detailsRest));
    
    attackButton.x = 103;
    attackButton.y = 21;
    attackButton.mouseChildren = false;
    attackButton.addChild(new createjs.Bitmap(attackRest));

    detailsButton.on("mouseover", function(evt) {
        this.removeAllChildren();
        this.addChild(new createjs.Bitmap(detailsRoll));
    });

    detailsButton.on("mousedown", function(evt) {
        if(selectedUnit != false) {
            sendInfoUnit(selectedUnit);
        }
    
        this.removeAllChildren();
        this.addChild(new createjs.Bitmap(detailsActive));
    });

    attackButton.on("mouseover", function(evt) {
        this.removeAllChildren();
        this.addChild(new createjs.Bitmap(attackRoll));
    });

    attackButton.on("mousedown", function(evt) {
        if(selectedPortrait != false) {           
            attackToggled = ! attackToggled;
        }
        
        this.removeAllChildren();
        this.addChild(new createjs.Bitmap(attackActive));
    });

    actionBar.addChild(actionBarBg);
    actionBar.addChild(detailsButton);
    actionBar.addChild(attackButton);

    stage.addChild(actionBar);

    selectPanel = new createjs.Container();
    var bgPanel = new createjs.Bitmap(selectPanelBg);
    var content = new createjs.Container();

    content.name = "content";

    selectPanel.addChild(bgPanel);
    selectPanel.addChild(content);
    
    selectPanel.x = 1000;
    selectPanel.y = 200;

    stage.addChild(selectPanel);

    portraitPanel = new createjs.Container();
    portraitPanel.x = 0;
    portraitPanel.y = stageHeight - 83;

    stage.addChild(portraitPanel);

    //Initialize infoPanels
    for(var i = 0; i < 4; i++) {
        var panel = new createjs.Container();
        var bg = new createjs.Bitmap(infoPanelBg);
        var close = new createjs.Bitmap(close_rest);
        var content = new createjs.Container();
        var btnBuild = new createjs.Container();
        var btnBuildRest = new createjs.Bitmap(btnBuildRestImg);
        var btnBuildClicked = new createjs.Bitmap(btnBuildClickedImg);

        panel.visible = false;

        close.x = 300;
        close.y = 10;

        btnBuild.visible = false;
        btnBuild.x = 500 / 2 - 133 / 2;
        btnBuild.y = 440;

        btnBuild.name = "btnBuild";
        btnBuildRest.name = "rest";
        btnBuildClicked.name = "clicked";

        btnBuildRest.visible = true;
        btnBuildClicked.visible = false;

        btnBuild.addChild(btnBuildRest);
        btnBuild.addChild(btnBuildClicked);

        close.on("mousedown", function(evt) {
            console.log('Close mousedown')
            this.parent.visible = false;
        });

        btnBuild.on("mousedown", function(evt) {
            console.log("initUI btnBuild mousedown");
            var rest = this.getChildByName("rest");
            var clicked = this.getChildByName("clicked");

            rest.visible = false;
            clicked.visible = true;
        });

        btnBuild.on("mouseup", function(evt) {
            var rest = this.getChildByName("rest");
            var clicked = this.getChildByName("clicked");

            rest.visible = true;
            clicked.visible = false;
        });

        content.name = 'content';

        panel.addChild(bg);
        panel.addChild(close);
        panel.addChild(content);
        panel.addChild(btnBuild);

        stage.addChild(panel);

        infoPanels.push(panel);
    }

 
};

/*function createButton(text) {
    var button = new createjs.Container();
    var rest = new createjs.Bitmap(buttonRestImg);
    var hover = new createjs.Bitmap(buttonHoverImg);
    var clicked = new createjs.Bitmap(buttonClickedImg);

    rest.name = "rest";
    hover.name = "hover";
    clicked.name = "clicked";
    
    rest.visible = true;
    hover.visible = false;
    clicked.visible = false;

    button.addChild(rest);
    button.addChild(hover);
    button.addChild(clicked);

    var buttonText = new createjs.Text();
    buttonText.font = "bold 14px Play";
    buttonText.color = "#e1a972";
    buttonText.text = text;

    buttonText.x = 0;
    buttonText.y = 0;
    buttonText.textAlign = "center";

    button.addChild(buttonText);

    return button;
};*/

function showBattlePanel() {
    var content = battlePanel.getChildByName('content');
    content.removeAllChildren();

    battlePanel.visible = true;
};

function showLocalPanel() {
    localPanel.visible = true;
};

function showInfoPanel() {
    var xCoords = [0, infoPanelBg.width, infoPanelBg.width * 2];

    for(var i = 0; i < infoPanels.length; i++) {
        if(infoPanels[i].visible == false) {
            activeInfoPanel = infoPanels[i]; 
        }
        else {
            var index = xCoords.indexOf(infoPanels[i].x);
            xCoords.splice(index, 1);    
        }
    }

    var content = activeInfoPanel.getChildByName('content');
    content.removeAllChildren();

    if(xCoords.length > 0) {
        activeInfoPanel.x = xCoords[0];
    } 
    else {
        activeInfoPanel.x = 0;
    }

    activeInfoPanel.visible = true;    
};

function showDialogPanel() {
    var content = dialogPanel.getChildByName('content');
    content.removeAllChildren();

    dialogPanel.visible = true;
};

function addChildInfoPanel(item) {
    var content = activeInfoPanel.getChildByName('content');
    content.addChild(item);
};

function getInfoPanelContent() {
    return activeInfoPanel.getChildByName('content');
};

function addChildLocalMap(item, childName) {
    var localMapCont = localPanel.getChildByName('localMap');
    var child = localMapCont.getChildByName(childName);
    child.addChild(item);
};


function addChildDialogPanel(item) {
    var content = dialogPanel.getChildByName('content');
    content.addChild(item);
};

function isNeighbour(q, r, neighbours) {
    var i;

    for(i = 0; i < neighbours.length; i++) {
        if(q == neighbours[i].q && r == neighbours[i].r) {
            return true;
        }
    }

    return false;
};

function onError(evt) {
  showScreen('<span style="color: red;">ERROR: ' + evt.data+ '</span>');
};

function showScreen(txt) { 
  $('#output').prepend('<p>' + txt + '</p>');
};

function clearScreen() { 
  $('#output').html("");
};

function odd_q_to_cube(Q, R) {
  var cube = {};
  var X = Q;
  var Z = parseInt(R - (Q - (Q & 1)) / 2);
  var Y = (-1*X) - Z;
  
  cube["x"] = X;
  cube["y"] = Y;
  cube["z"] = Z;
  
  return cube;         
};

function cube_to_odd_q(X, Y, Z) {
  var odd_q = {};
  var Q = X;
  var R = parseInt(Z + (X - (X & 1)) / 2);
  
  odd_q["q"] = Q;
  odd_q["r"] = R;

  return odd_q;
};

function getNeighbours(Q, R) {
  var conversion = [ [1, -1, 0], [1, 0, -1], [0, 1, -1], [-1, 1, 0], [-1, 0, 1], [0, -1, 1] ];
  var cube = odd_q_to_cube(Q, R);
  var i; 
  var neighbours = [];

  for(i = 0; i < conversion.length; i++) {
      var offset = conversion[i];
      var odd_q = cube_to_odd_q(cube.x + offset[0], cube.y + offset[1], cube.z + offset[2]);

      neighbours.push(odd_q)
  }

  return neighbours; 
};

function getObjByPlayer(playerId) {
    for(var i = 0; i < objs.length; i++) {
        if(objs[i].player == playerId) {
            return objs[i];
        }
    }
};

function getObj(objId) {
    for(var i = 0; i < objs.length; i++) {
        if(objs[i].id == objId) {
            return objs[i];
        } 
    }
};

function getObjOnTile(x, y) {
    var objsOnTile = [];

    for(var i = 0; i < objs.length; i++) {
        if(objs[i].x == x && objs[i].y == y) {
            objsOnTile.push(objs[i]);
        }
    }

    return objsOnTile;
};

function getLocalObj(id) {
    for(var localObjId in localObjs) {
        if(localObjId == id) {
            return localObjs[localObjId];
        }
    }
    return false;
};

function getLocalObjsAt(x, y) {
    var objsAt = [];
    
    for(var localObjId in localObjs) {
        var localObj = localObjs[localObjId];
        
        if(localObj.x == x && localObj.y == y)
            objsAt.push(localObj);
    }

    return objsAt;
};

function addLocalTile(tile) {
    var xy = tile.x + "_" + tile.y;
    localTiles[xy] = tile;
};

function getLocalTile(x, y) {
    var xy = tile.x + "_" + tile.y;

    if(xy in localTiles) {
        return localTiles[xy];
    }

    return false;
};

function to_hex(x, y) {
    return {q: x, r: y};
};

function hex_to_pixel(q, r) {
    var x = hexSize * 0.75 * q;
    var y = hexSize * (r + 0.5 * (q & 1));

    return {x: x, y: y};
};

function distance(srcX, srcY, dstX, dstY) {
    var srcCube = odd_q_to_cube(srcX, srcY);
    var dstCube = odd_q_to_cube(dstX, dstY);

    return (Math.abs(srcCube["x"] - dstCube["x"]) +
            Math.abs(srcCube["y"] - dstCube["y"]) +
            Math.abs(srcCube["z"] - dstCube["z"])) / 2;
};

function range(srcX, srcY, dist) {

    var srcCube = odd_q_to_cube(srcX, srcY);
    var results = [];

    for(var x = -1 * dist; x <= dist; x++) {
        for(var y = -1 * dist; y <= dist; y++) {
            for(var z = -1 * dist; z <= dist; z++) {

                if((x + y + z) == 0) {
                    var cube = {};

                    cube["x"] = srcCube["x"] + x;
                    cube["y"] = srcCube["y"] + y;
                    cube["z"] = srcCube["z"] + z;

                    var oddq = cube_to_odd_q(cube["x"], cube["y"], cube["z"]);
                    results.push(oddq);
                }
            }
        }
    }

    return results;
};

function is_visible(x, y, visibleTiles) {
    for(var i = 0; i < visibleTiles.length; i++) {
        var visibleTile = visibleTiles[i];
            
        if(visibleTile["q"] == x && visibleTile["r"] == y) {                
            return true;
        }
    }

    return false;
};

function is_hero(type) {
    if(type.toLowerCase().indexOf("hero") > -1) {
        return true;
    } 

    return false;
};
