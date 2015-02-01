var websocket;
var stage_map;
var stage_battle;
var canvas_map;
var canvas_battle;
var container_map;

var explored = {};
var objs = {};
var units = {};
var playerId;
var playerPos;
var selectedUnit;

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
    $("#navigation").hide();

    canvas_map = document.getElementById("map");
    stage_map = new createjs.Stage(canvas_map);
    stage_map.autoClear = true;

    canvas_battle = document.getElementById("battle");
    stage_battle = new createjs.Stage(canvas_battle);
    stage_battle.autoClear = true;
    
    createjs.Ticker.setFPS(60);
    createjs.Ticker.addEventListener("tick", stage_map);
    createjs.Ticker.addEventListener("tick", stage_battle);

    container_map = new createjs.Container();
    stage_map.addChild(container_map)

    $('#server').val("ws://" + window.location.host + "/websocket");
    if(!("WebSocket" in window)){  
        $('#status').append('<p><span style="color: red;">websockets are not supported </span></p>');
        $("#ui").hide();  
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
    var q, r;
    var bitmap;

    var playerQ = playerPos % 4;
    var playerR = parseInt(playerPos / 4, 10);
    var neighbours = getNeighbours(playerQ, playerR);

    for(var pos in explored) {
        console.log("pos: " + pos + " tile: " + explored[pos]);

        q = pos % 4;
        r = parseInt(pos / 4, 10);

        console.log("q: " + q + " r: " + r);

        x = 36 * 3/2 * q;
        y = 36 * Math.sqrt(3) * (r + 0.5 * (q & 1));
      
        console.log("x: " + x + " y:" + y); 

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

        bitmap.x = x;
        bitmap.y = y;
        container_map.addChild(bitmap);

        if(pos != playerPos) {
            if(!isNeighbour(q, r, neighbours)) {
                
                bitmap = new createjs.Bitmap(shroud);
                bitmap.x = x;
                bitmap.y = y;
                container_map.addChild(bitmap);
            }
        }
    }

    //stage.update();
};

function drawObjs() {
    var bitmap;
    var c_x;
    var c_y;

    for(i = 0; i < objs.length; i++) {
        q = objs[i].pos % 4;
        r = parseInt(objs[i].pos / 4, 10);

        x = 36 * 3/2 * q;
        y = 36 * Math.sqrt(3) * (r + 0.5 * (q & 1));

        console.log("(x,y): " + x + "," + y);     

        if(objs[i].player == 1) {
            bitmap = new createjs.Bitmap(obj1);
            c_x = 250 - 36 - x;
            c_y = 250 - 36 - y;
            container_map.x = c_x;
            container_map.y = c_y;        
        }
        else {
            bitmap = new createjs.Bitmap(obj2);
        }

        bitmap.obj_id = objs[i].id;
        bitmap.on("mousedown", function(evt) {
            sendInfo(this.obj_id, "obj"); 
        });
        bitmap.x = x;
        bitmap.y = y;
        
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
