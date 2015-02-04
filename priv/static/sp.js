var websocket;
var stage_map;
var stage_battle;
var stage_ui;
var canvas_map;
var canvas_battle;
var canvas_ui;
var container_map;

var explored = {};
var objs = {};
var units = {};
var playerId;
var playerPos;
var selectedUnit;

var mapWidth = 4;
var mapHeight = 4;
var hexSize = 72;

var tile0 = new Image();
var tile1 = new Image();
var tile2 = new Image();
var tile3 = new Image();
var shroud = new Image();

var obj1 = new Image();
var obj2 = new Image();

var unit1;
var unit2;

tile0.src = "/static/green.png";
tile1.src = "/static/basic-tile.png";
tile2.src = "/static/regular.png";
tile3.src = "/static/desert.png";
shroud.src = "/static/shroud.png";

obj1.src = "/static/white-mage.png";
obj2.src = "/static/zombie.png";

$(document).ready(init);

function init() {
    $('#map').css('background-color', 'rgba(0, 0, 0, 1)');
    $("#map").hide();
    $("#battle").hide();
    $('#battle').css('background-color', 'rgba(0, 0, 0, 1)');
    $("#ui").hide();
    $('#ui').css('background-color', 'rgba(0, 0, 0, 1)');
    $("#navigation").hide();

    canvas_map = document.getElementById("map");
    stage_map = new createjs.Stage(canvas_map);
    stage_map.autoClear = true;

    canvas_battle = document.getElementById("battle");
    stage_battle = new createjs.Stage(canvas_battle);
    stage_battle.autoClear = true;

    canvas_ui = document.getElementById("ui");
    stage_ui = new createjs.Stage(canvas_ui);
    stage_ui.autoClear = true;
    
    createjs.Ticker.setFPS(60);
    createjs.Ticker.addEventListener("tick", stage_map);
    createjs.Ticker.addEventListener("tick", stage_battle);
    createjs.Ticker.addEventListener("tick", stage_ui);

    container_map = new createjs.Container();
    stage_map.addChild(container_map)

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

function sendInfo(id, type) {
    var info = '{"cmd": "info", "id": "' + id + '", "type": "' + type + '"}';
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
            $("#ui").fadeIn('slow');
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

        if(explored[pos] == 0) {
            bitmap = new createjs.Bitmap(tile0);
        }
        else if(explored[pos] == 1) {
            bitmap = new createjs.Bitmap(tile1);
        }
        else if(explored[pos] == 2) {
            bitmap = new createjs.Bitmap(tile2);
        }
        else if(explored[pos] == 3) {
            bitmap = new createjs.Bitmap(tile3);
        }

        bitmap.pos = pos;
        bitmap.x = pixel.x;
        bitmap.y = pixel.y;
        bitmap.on("mousedown", function(evt) {
            sendInfo(this.pos, "tile");
        });
        container_map.addChild(bitmap);

        if(pos != playerPos) {
            if(!isNeighbour(hex.q, hex.r, neighbours)) {
                
                bitmap = new createjs.Bitmap(shroud);
                bitmap.x = pixel.x;
                bitmap.y = pixel.y;
                container_map.addChild(bitmap);
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
            container_map.x = c_x;
            container_map.y = c_y;        
        }
        else {
            bitmap = new createjs.Bitmap(obj2);
        }

        bitmap.mouseEnabled = false;
        bitmap.x = pixel.x;
        bitmap.y = pixel.y;
        
        container_map.addChild(bitmap);
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

function drawInfoObj(jsonData) {
    stage_ui.removeAllChildren();
    stage_ui.update();

    var infoText = new createjs.Text("Info", "16px Arial", "#000000");
    infoText.x = 20;
    infoText.y = 20;
    
    stage_ui.addChild(infoText);

    var bitmap = new createjs.Bitmap(obj1);
    bitmap.x = 20;
    bitmap.y = 40;
    
    stage_ui.addChild(bitmap);

    var unitText = new createjs.Text("Units", "14px Arial", "#000000");
    unitText.x = 20;
    unitText.y = 125;

    stage_ui.addChild(unitText);

    for(var i = 0; i < jsonData.units.length; i++) {
        var unitName = jsonData.units[i].name;
        unitName = unitName.toLowerCase().replace(/ /g, '');
        
        var imagePath =  "/static/" + unitName + ".png";

        var bitmap = new createjs.Bitmap(imagePath);

        bitmap._id = jsonData.units[i]._id;
        bitmap.x = 20;
        bitmap.y = 145;
        bitmap.on("mousedown", function(evt) {
            sendInfo(this._id, "unit"); 
        });

        stage_ui.addChild(bitmap);
    }

    var itemText = new createjs.Text("Items", "14px Arial", "#000000");
    itemText.x = 20;
    itemText.y = 225;

    stage_ui.addChild(itemText);

    for(var i = 0; i < jsonData.items.length; i++) {
        var itemName = jsonData.items[i].type;
        itemName = itemName.toLowerCase().replace(/ /g,'');
        var imagePath = "/static/" + itemName + ".png";

        var bitmap = new createjs.Bitmap(imagePath);
        bitmap.x = 20;
        bitmap.y = 245;
        
        stage_ui.addChild(bitmap);
    }
};

function drawInfoUnit(jsonData) {
    console.log("drawInfoUnit");
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
