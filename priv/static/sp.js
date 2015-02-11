var websocket;
var stage;
var loaderQueue;
var imagesQueue = [];
var canvas;
var map;
var infoPanels = [];
var activeInfoPanel;

var explored = {};
var objs = {};
var units = {};
var playerId;
var playerPos;
var selectedUnit;

var mapWidth = 4;
var mapHeight = 4;
var hexSize = 72;

var obj1 = new Image();
var obj2 = new Image();
var ui_bg = new Image();
var close_rest = new Image();

var unit1;
var unit2;

var tileImages = [];

tileImages[0] = "/static/art/green.png";
tileImages[1] = "/static/art/basic-tile.png";
tileImages[2] = "/static/art/regular.png";
tileImages[3] = "/static/art/desert.png";

var shroud = "/static/art/shroud.png";

obj1.src = "/static/art/white-mage.png";
obj2.src = "/static/art/zombie.png";
ui_bg.src = '/static/art/ui_pane.png';
close_rest.src = '/static/art/close_rest.png';

$(document).ready(init);

function init() {
    $('#map').css('background-color', 'rgba(0, 0, 0, 1)');
    $("#map").hide();
    $("#navigation").hide();

    canvas = document.getElementById("map");
    stage = new createjs.Stage(canvas);
    stage.autoClear = true;

    map = new createjs.Container();
    stage.addChild(map)
    
    initImages();
    initUI();

    createjs.Ticker.setFPS(60);
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

    var q = playerObj.pos % 4;
    var r = parseInt(playerObj.pos / 4, 10);
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
    var newPos = odd_q.r * 4 + odd_q.q;

    var move = '{"cmd": "move", "id": "' + playerObj.id + '", "pos": ' + newPos + '}';

    websocket.send(move);
};

function sendInfoObj(id) {
    var info = '{"cmd": "info_obj", "id": "' + id + '"}';
    websocket.send(info);
};

function sendInfoUnit(id) {
    var info = '{"cmd": "info_unit", "id": "' + id + '"}';
    websocket.send(info);
};

function sendInfoTile(type, pos) {
    var info = '{"cmd": "info_tile", "type": ' + type + ', "pos": ' + pos + '}';
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
    var jsonData = JSON.parse(evt.data);

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
            $("#battle").fadeIn('slow');
            drawUnits(jsonData.units);
        }
        else if(jsonData.packet == "battle_event") {
            drawDmg(jsonData.dmg);
        }
        else if(jsonData.packet == "info_obj") {
            drawInfoObj(jsonData);
        }
        else if(jsonData.packet == "info_unit") {
            drawInfoUnit(jsonData);
        }
    }

    showScreen('<span style="color: blue;">RESPONSE: ' + evt.data+ '</span>'); 
};

function setPlayerPos() {
    var i;

    for(i = 0; i < objs.length; i++) {

        if(objs[i].player == playerId) {
            playerPos = objs[i].pos;
        }
    }
};

function drawMap() {
    var bitmap;

    var playerQ = playerPos % 4;
    var playerR = parseInt(playerPos / 4, 10);
    var neighbours = getNeighbours(playerQ, playerR);

    for(var pos in explored) {
        var hex = pos_to_hex(pos);
        var pixel = hex_to_pixel(hex.q, hex.r);
        var tileType = explored[pos];

        bitmap = new createjs.Bitmap(tileImages[tileType]);
        bitmap.tile = tileType;
        bitmap.pos = pos;
        bitmap.x = pixel.x;
        bitmap.y = pixel.y;
        bitmap.on("mousedown", function(evt) {

            var objList = getObjOnTile(this.pos);
            drawInfoOnTile(this.tile, this.pos, objList);
        });

        map.addChild(bitmap);

        if(pos != playerPos) {
            if(!isNeighbour(hex.q, hex.r, neighbours)) {
                
                bitmap = new createjs.Bitmap(shroud);
                bitmap.x = pixel.x;
                bitmap.y = pixel.y;
                map.addChild(bitmap);
            }
        }
    }

};

function drawObjs() {
    var bitmap;
    var c_x;
    var c_y;
    var halfwidth = $("#map").width() / 2;
    var halfheight = $("#map").height() / 2;

    for(i = 0; i < objs.length; i++) {
        var hex = pos_to_hex(objs[i].pos);
        var pixel = hex_to_pixel(hex.q, hex.r);

        if(objs[i].player == 1) {
            bitmap = new createjs.Bitmap(obj1);
            c_x = halfwidth - 36 - pixel.x;
            c_y = halfheight - 36 - pixel.y;
            map.x = c_x;
            map.y = c_y;
        }
        else {
            bitmap = new createjs.Bitmap(obj2);
        }

        bitmap.mouseEnabled = false;
        bitmap.x = pixel.x;
        bitmap.y = pixel.y;
        
        map.addChild(bitmap);
    }
};

function drawUnits(unit_data) {

    for(i = 0; i < unit_data.length; i++) {
        
        var obj = getObj(unit_data[i].obj_id);
        console.log("Unit obj.player: " + obj.player);

        if(obj.player == playerId) {
            unit1 = new createjs.Bitmap(obj1);
            unit1.x = 25;
            unit1.y = 200;
            unit1.on("mousedown", function(evt) {
                selectedUnit = unit_data[0]._id;    
            });
            stage_battle.addChild(unit1);
        }
        else {
            unit2 = new createjs.Bitmap(obj2);
            unit2.x = 425;
            unit2.y = 200;
            unit2.on("mousedown", function(evt) {
                var attack_unit = '{"cmd": "attack_unit", "sourceid": "' + selectedUnit + '", "targetid": "' + unit_data[1]._id + '"}';
                websocket.send(attack_unit);
            });
            stage_battle.addChild(unit2);
        }
        
    }
};

function drawDmg() {
    console.log("stage_battle numChildren: " + stage_battle.numChildren);
    createjs.Tween.get(unit1).to({x: 425}, 1000, createjs.Ease.getPowInOut(4)).to({x: 25}, 1000, createjs.Ease.getPowInOut(2));
};

function drawInfoOnTile(tileType, tilePos, objsOnTile) {
    showInfoPanel();

    var tile = new createjs.Bitmap(tileImages[tileType]);
    
    tile.type = tileType;
    tile.pos = tilePos;
    tile.on("mousedown", function(evt) {
        sendInfoTile(this.type, this.pos);
    });


    tile.x = (ui_bg.width / 2) - 36;
    tile.y = 52;

    addChildInfoPanel(tile);

    for(var i = 0; i < objsOnTile.length; i++) {
        if(objsOnTile[i].player == 1) {
            var obj = new createjs.Bitmap(obj1);
        } else {
            var obj = new createjs.Bitmap(obj2);
        }
    
        obj.obj_id = objsOnTile[i].id;
        obj.on("mousedown", function(evt) {
            sendInfoObj(this.obj_id);
        });
    
        obj.x = i * 72
        obj.y = 130;
 
        addChildInfoPanel(obj);
    }

};


function drawInfoObj(jsonData) {
    showInfoPanel();

    //createjs.Tween.get(infoPanels[lastActivePanel]).to({x: 0}, 500, createjs.Ease.getPowInOut(4));

    var bitmap = new createjs.Bitmap(obj1);
    bitmap.x = 166 - obj1.width/2;
    bitmap.y = 40;
    
    var unitText = new createjs.Text("Units", "14px Verdana", "#FFFFFF");
    unitText.x = 20;
    unitText.y = 125;

    addChildInfoPanel(bitmap);
    addChildInfoPanel(unitText);

    for(var i = 0; i < jsonData.units.length; i++) {
        var unitName = jsonData.units[i].name;
        unitName = unitName.toLowerCase().replace(/ /g, '');
        
        var imagePath =  "/static/art/" + unitName + ".png";

        imagesQueue.push({id: unitName, x: 20, y: 145, target: getInfoPanelContent()});
        loaderQueue.loadFile({id: unitName, src: imagePath});
    }

    var itemText = new createjs.Text("Items", "14px Verdana", "#FFFFFF");
    itemText.x = 20;
    itemText.y = 225;

    addChildInfoPanel(itemText);

    for(var i = 0; i < jsonData.items.length; i++) {
        var itemName = jsonData.items[i].type;
        itemName = itemName.toLowerCase().replace(/ /g,'');
        var imagePath = "/static/art/" + itemName + ".png";

        imagesQueue.push({id: itemName, x: 20, y: 245, target: getInfoPanelContent()});
        loaderQueue.loadFile({id: itemName, src: imagePath});
    }
};

function drawInfoUnit(jsonData) {
    console.log("drawInfoUnit");
    container_ui[2].x = 0;
    container_ui[2].y = 0;
    container_ui[2].visible = true;

    createjs.Tween.get(container_ui[2]).to({x: 333}, 500, createjs.Ease.getPowInOut(4));

    var unitName = jsonData.name
    var nameText = new createjs.Text(unitName, "14px Verdana", "#FFFFFF");
    var nameBounds = nameText.getBounds();
    nameText.x =  166 - nameBounds.width / 2;
    nameText.y = 12;

    container_ui[2].addChild(nameText);

    unitName = unitName.toLowerCase().replace(/ /g, '');
    var imagePath =  "/static/art/" + unitName + ".png";
    var unitImage = new createjs.Bitmap(imagePath);

    unitImage.x = 166 - 24;
    unitImage.y = 50;

    container_ui[2].addChild(unitImage);

    var hp = "Hp: " + jsonData.hp + " / " + jsonData.base_hp + "\n"
           + "Damage: " + jsonData.base_dmg + " - " + jsonData.dmg_range + "\n" 
           + "Defense: " + jsonData.base_def + "\n"
           + "Speed: " + jsonData.base_speed + "\n";
           
    var statsText = new createjs.Text(hp, "14px Verdana", "#FFFFFF");
    statsText.lineHeight = 20;
    
    statsText.x = 10;
    statsText.y = 125;
    
    container_ui[2].addChild(statsText);

    var itemText = new createjs.Text("Items: ", "14px Verdana", "#FFFFFF");
    itemText.x = 10;
    itemText.y = 250;
    
    container_ui[2].addChild(itemText);

    for(var i = 0; i < jsonData.items.length; i++) {
        var itemName = jsonData.items[i].type;
        itemName = itemName.toLowerCase().replace(/ /g,'');
        var imagePath = "/static/art/" + itemName + ".png";

        var bitmap = new createjs.Bitmap(imagePath);
        bitmap.x = 20;
        bitmap.y = 275;
        
        container_ui[2].addChild(bitmap);
    }
};

function initUI() {
    for(var i = 0; i < 4; i++) {
        var panel = new createjs.Container();
        var bg = new createjs.Bitmap(ui_bg);
        var close = new createjs.Bitmap(close_rest);
        var content = new createjs.Container();

        panel.visible = false;

        close.x = 300;
        close.y = 10;

        content.name = 'content'

        close.on("mousedown", function(evt) {
            this.parent.visible = false;
        });

        panel.addChild(bg);
        panel.addChild(close);
        panel.addChild(content)

        stage.addChild(panel);

        infoPanels.push(panel);
    }
};

function showInfoPanel() {
    
    var xCoords = [0, 333, 666];

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

    activeInfoPanel.x = xCoords[0];
    activeInfoPanel.visible = true;    
};

function addChildInfoPanel(item) {
    var content = activeInfoPanel.getChildByName('content');
    content.addChild(item);
}

function getInfoPanelContent() {
    return activeInfoPanel.getChildByName('content');
}

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

function getObjOnTile(pos) {
    var objsOnTile = [];

    for(var i = 0; i < objs.length; i++) {
        if(objs[i].pos == pos) {
            objsOnTile.push(objs[i]);
        }
    }

    return objsOnTile;
};

function getTileImage(tileType) {

    switch(tileType) {
        case 0:
            return tile0;
        case 1: 
            return tile1;
        case 2:
            return tile2;
        case 3:
            return tile3;
    }

};

function pos_to_hex(pos) {
    var q = pos % mapWidth;
    var r = parseInt(pos / mapHeight, 10);

    return {q: q, r: r};
};

function hex_to_pixel(q, r) {
    var x = hexSize * 0.75 * q;
    var y = hexSize * (r + 0.5 * (q & 1));

    return {x: x, y: y};
};
