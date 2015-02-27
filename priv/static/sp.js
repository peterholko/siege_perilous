var websocket;
var stage;
var loaderQueue;
var imagesQueue = [];
var canvas;
var map;
var battlePanel;
var infoPanels = [];
var activeInfoPanel;
var dialogPanel;

var explored = {};
var objs = {};
var units = {};
var battles = [];
var battleUnits = [];

var playerId;
var playerPos;
var selectedUnit = false;

var mapWidth = 4;
var mapHeight = 4;
var hexSize = 72;
var stageWidth = 1000;
var stageHeight = 500;

var castle1 = new Image();
var castle2 = new Image();
var castle3 = new Image();
var castle4 = new Image();
var castle5 = new Image();
var castle6 = new Image();

castle1.src = '/static/art/regular-concave-bl.png';
castle2.src = '/static/art/regular-concave-br.png';
castle3.src = '/static/art/regular-concave-l.png';
castle4.src = '/static/art/regular-concave-r.png';
castle5.src = '/static/art/regular-concave-tl.png';
castle6.src = '/static/art/regular-concave-tr.png';

var infoPanelBg = new Image();
var dialogPanelBg = new Image();
var close_rest = new Image();

var h1Font = "14px Verdana"
var textColor = "#FFFFFF";

var tileImages = [];

tileImages[0] = "/static/art/basic-tile.png";
tileImages[1] = "/static/art/desert.png";
tileImages[2] = "/static/art/green.png";
tileImages[3] = "/static/art/regular.png";

var shroud = "/static/art/shroud.png";

infoPanelBg.src = '/static/art/ui_pane.png';
dialogPanelBg.src = '/static/art/dialog.png';
close_rest.src = '/static/art/close_rest.png';

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
    $('#map').css('background-color', 'rgba(0, 0, 0, 1)');
    $("#map").hide();
    $("#navigation").hide();

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
    loaderQueue.addEventListener("complete", handleQueueComplete);
    loaderQueue.loadManifest(manifest, true, "/static/art/");
};

function handleQueueComplete()
{
    console.log("Queue complete");

    while(imagesQueue.length > 0) {
        var imageTask = imagesQueue.shift();
        var image = loaderQueue.getResult(imageTask.id)

        if(image) {
            var bitmap = new createjs.Bitmap(image);

            bitmap.x = imageTask.x;
            bitmap.y = imageTask.y;

            imageTask.target.addChild(bitmap);
        }
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

function sendMove(direction) {
    playerObj = getObjByPlayer(playerId);

    var q = playerObj.x;
    var r = playerObj.y;
    var cube = odd_q_to_cube(q,r);

    var x, y, z;

    if(direction == 'NW') {
        x = cube.x - 1;
        y = cube.y + 1;
        z = cube.z;
    }
    else if(direction == 'N') {
        x = cube.x;
        y = cube.y + 1;
        z = cube.z - 1;
    }
    else if(direction == 'NE') {
        x = cube.x + 1;
        y = cube.y;
        z = cube.z - 1;
    }
    else if(direction == 'SW') {
        x = cube.x - 1;
        y = cube.y;
        z = cube.z + 1;
    }
    else if(direction == 'S') {
        x = cube.x;
        y = cube.y - 1;
        z = cube.z + 1;
    }
    else if(direction == 'SE') {
        x = cube.x + 1;
        y = cube.y - 1;
        z = cube.z;
    }

    var odd_q = cube_to_odd_q(x, y, z);
    var move = '{"cmd": "move", "id": "' + playerObj.id + 
        '", "x": ' + odd_q.q + ', "y": ' + odd_q.r + '}';

    websocket.send(move);
};

function sendEquip() {
    console.log("sendEquip");
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
            drawMap();
            drawObjs();
        }
        else if(jsonData.packet == "map_perception") {
            explored = jsonData.explored;
            objs = jsonData.objs;

            setPlayerPos();
            drawMap();
            drawObjs();
        }
        else if(jsonData.packet == "battle_perception") {
            drawBattle(jsonData);
        }
        else if(jsonData.packet == "item_perception") {
            drawItemDialog(jsonData);
        }
        else if(jsonData.packet == "battle_dmg") {
            drawDmg(jsonData);
        }
        else if(jsonData.packet == "battle_move") {
            console.log("battle_move");
            drawBattleMove(jsonData);
        }
        else if(jsonData.packet == "info_obj") {
            drawInfoObj(jsonData);
        }
        else if(jsonData.packet == "info_unit") {
            drawInfoUnit(jsonData);
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
        bitmap.tile_x = tile.x;
        bitmap.tile_y = tile.y;
        bitmap.x = pixel.x;
        bitmap.y = pixel.y;
        bitmap.on("mousedown", function(evt) {
            var objList = getObjOnTile(this.tile_x, this.tile_y);
            drawInfoOnTile(this.tile, this.tile_x, this.tile_y, objList);
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

    var pixel = hex_to_pixel(3,3);

    bitmap = new createjs.Bitmap(castle5);
    bitmap.x = pixel.x - 55;
    bitmap.y = pixel.y - 110;

    map.addChild(bitmap);
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

function drawBattle(jsonData) {
    showBattlePanel();
    selectedUnit = false;
    battleUnits = jsonData.units;

    for(var i = 0; i < jsonData.map.length; i++) {
        var tile = jsonData.map[i];
        var pixel = hex_to_pixel(tile.x, tile.y);

        var bitmap = new createjs.Bitmap(tileImages[tile.t]);
        bitmap.tileX = tile.x;
        bitmap.tileY = tile.y;
        bitmap.x = pixel.x;
        bitmap.y = pixel.y;
        bitmap.on("mousedown", function(evt) {
            if(selectedUnit != false) {
                var move_unit = '{"cmd": "move_unit", "sourceid": "' + selectedUnit + '", "x": ' + this.tileX + ', "y": ' + this.tileY + '}';    
                websocket.send(move_unit);
            } 
        });

        addChildBattlePanel(bitmap);
    }
    
    drawBattleUnits();
};

function drawBattleUnits() {
    
    for(var i = 0; i < battleUnits.length; i++) {
        var pixel = hex_to_pixel(battleUnits[i].x, battleUnits[i].y);
        var obj = getObj(battleUnits[i].obj);
        var unitName = battleUnits[i].type;

        unitName = unitName.toLowerCase().replace(/ /g, '');

        var imagePath =  "/static/art/" + unitName + ".png";
        var icon = new createjs.Container();

        battleUnits[i].icon = icon;

        icon.player = obj.player;
        icon.unit = battleUnits[i].unit;
        icon.x = pixel.x;
        icon.y = pixel.y;

        icon.on("mousedown", function(evt) {
            if(selectedUnit == false) {
                if(this.player == playerId) {
                    selectedUnit = this.unit;
                } 
            } else {
                if(this.player != playerId) {
                    var attack_unit = '{"cmd": "attack_unit", "sourceid": "' + selectedUnit + '", "targetid": "' + this.unit + '"}';    
                    websocket.send(attack_unit);
                }
            }
        });

        addChildBattlePanel(icon);

        imagesQueue.push({id: unitName, x: 0, y: 0, target: icon});
        loaderQueue.loadFile({id: unitName, src: imagePath});
    }
};

function drawBattleMove(jsonData) {
    var unit = getBattleUnit(jsonData.sourceid);
    var pixel = hex_to_pixel(jsonData.x, jsonData.y);

    unit.icon.x = pixel.x;
    unit.icon.y = pixel.y;
};

function drawDmg(jsonData) {
    if(battlePanel.visible) {
        var source = getBattleUnit(jsonData.sourceid);
        var target = getBattleUnit(jsonData.targetid);
        var origX = source.icon.x;
        var origY = source.icon.y;

        if(source && target) {
            createjs.Tween.get(source.icon).to({x: target.icon.x, y: target.icon.y}, 500, createjs.Ease.getPowInOut(4))
                                           .to({x: origX, y: origY}, 100, createjs.Ease.getPowInOut(2));
        }

        if(jsonData.state == "dead") {
            target.icon.removeAllChildren();
            var die = new createjs.Sprite(zombieSS, "die");
            target.icon.addChild(die);
        }
    }
};

function drawItemDialog(jsonData) {
    if(battlePanel.visible) {
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
                sendInfoItem(this._id);
            });

            icon.x = dialogPanelBg.width / 2 - 24;
            icon.y = dialogPanelBg.height / 2 + 5 - 24;

            addChildDialogPanel(icon);

            imagesQueue.push({id: itemName, x: 0, y: 0, target: icon});
            loaderQueue.loadFile({id: itemName, src: imagePath});
        }
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

    var stats = "Hp: " + jsonData.hp + " / " + jsonData.base_hp + "\n"
              + "Damage: " + jsonData.base_dmg + " - " + jsonData.dmg_range + "\n" 
              + "Defense: " + jsonData.base_def + "\n"
              + "Speed: " + jsonData.base_speed + "\n";
           
    var statsText = new createjs.Text(stats, h1Font, textColor);

    statsText.lineHeight = 20;
    statsText.x = 10;
    statsText.y = 125;
    
    addChildInfoPanel(statsText);

    var itemText = new createjs.Text("Items: ", h1Font, textColor);
    itemText.x = 10;
    itemText.y = 250;
    
    addChildInfoPanel(itemText);

    for(var i = 0; i < jsonData.items.length; i++) {
        var itemName = jsonData.items[i].type;
        itemName = itemName.toLowerCase().replace(/ /g,'');
        var imagePath = "/static/art/" + itemName + ".png";

        //imagesQueue.push({id: itemName, x: 20, y: 276, target: getInfoPanelContent()});
        //loaderQueue.loadFile({id: itemName, src: imagePath});
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

    //Initialize battle panel
    battlePanel = new createjs.Container();
    battlePanel.visible = false;
    battlePanel.x = stageWidth / 2 - 500 / 2;
    battlePanel.y = stageHeight / 2 - 460 / 2;

    var bg = new createjs.Shape();
    var close = new createjs.Bitmap(close_rest);
    var content = new createjs.Container();

    var battleText = new createjs.Text("Battle", h1Font, textColor);
    battleText.x = 500 / 2;
    battleText.y = 10;
    battleText.textAlign = "center";

    bg.graphics.beginFill("#1c1c1c").drawRect(0,0,500,460);

    close.x = 480;
    close.y = 10;
    close.on("mousedown", function(evt) {
        console.log('Close mousedown')
        this.parent.visible = false;
    });

    content.name = 'content'

    battlePanel.addChild(bg);
    battlePanel.addChild(battleText);
    battlePanel.addChild(close);
    battlePanel.addChild(content);

    stage.addChild(battlePanel);

    //Initialize infoPanels
    for(var i = 0; i < 4; i++) {
        var panel = new createjs.Container();
        var bg = new createjs.Bitmap(infoPanelBg);
        var close = new createjs.Bitmap(close_rest);
        var content = new createjs.Container();

        panel.visible = false;

        close.x = 300;
        close.y = 10;

        content.name = 'content';

        close.on("mousedown", function(evt) {
            console.log('Close mousedown')
            this.parent.visible = false;
        });

        panel.addChild(bg);
        panel.addChild(close);
        panel.addChild(content);

        stage.addChild(panel);

        infoPanels.push(panel);
    }

    //Initialize dialogPanel
    dialogPanel = new createjs.Container();
    dialogPanel.x = stageWidth / 2 - 124;
    dialogPanel.y = stageHeight / 2 - 95; 
    dialogPanel.visible = false;

    var bg = new createjs.Bitmap(dialogPanelBg);
    var close = new createjs.Bitmap(close_rest);
    var content = new createjs.Container();

    close.x = 228;
    close.y = 10;
    close.on("mousedown", function(evt) {
        this.parent.visible = false;
    });

    content.name = 'content';

    dialogPanel.addChild(bg); 
    dialogPanel.addChild(close);
    dialogPanel.addChild(content);

    stage.addChild(dialogPanel);
};

function showBattlePanel() {
    var content = battlePanel.getChildByName('content');
    content.removeAllChildren();

    battlePanel.visible = true;
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

function addChildDialogPanel(item) {
    var content = dialogPanel.getChildByName('content');
    content.addChild(item);
};

function getInfoPanelContent() {
    return activeInfoPanel.getChildByName('content');
};

function addChildBattlePanel(item) {
    var content = battlePanel.getChildByName('content');
    content.addChild(item);
};

function getBattlePanelContent() {
    return battlePanel.getChildByName('content');
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

function getBattleUnit(unit) {
    for(var i = 0; i < battleUnits.length; i++) {
        if(battleUnits[i].unit == unit) {
            return battleUnits[i];
        }
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
