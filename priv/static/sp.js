var websocket;
var stage;

var startRender = false;
var rendering = false;
var renderQueue = [];

var lastRenderTime = 0;
var loaderQueue;
var imagesQueue = [];
var spritesQueue = [];
var assets = [];
var canvas;
var map;
var battlePanel;
var localPanel;
var infoPanels = [];
var dialogPanel;
var smallDialogPanel;
var selectPanel;
var selectHex;
var portraitPanel;
var reventPanel;

var textLog;
var textLogLines = [];

var explored = {};
var objs = {};
var localObjs = {};
var localTiles = {};
var perception = {};
var units = {};
var stats = {};

var visibleTiles = [];

var playerId;
var playerPos;
var heroId;
var heroPos;

var lastSelectedPos;
var selectedPortrait = -1;
var selectedUnit = -1;
var selectedTile = -1;

var hpBar;
var staminaBar;

var attackToggled = false;
var moveToggled = false;

var origX;
var origY;
var pressmove = false;

var mapWidth = 4;
var mapHeight = 4;
var hexSize = 72;
var stageWidth = 1334;
var stageHeight = 750;

var hover;
var clicked;

var infoPanelBg = new Image();
var dialogPanelBg = new Image();
var smallDialogPanelBg = new Image();
var selectPanelBg = new Image();
var close_rest = new Image();
var selectHexImage = new Image();
var selectIconImage = new Image();
var actionBarBgImage = new Image();
var portraitBg = new Image();
var reventBg = new Image();
var reventOkButton = new Image();
var reventButtonBg = new Image();

var leftImage = new Image();
var rightImage = new Image();

var attackActive = new Image();
var attackRest = new Image();
var attackRoll = new Image();

var gatherActive = new Image();
var gatherRest = new Image();
var gatherRoll = new Image();

var buildActive = new Image();
var buildRest = new Image();
var buildRoll = new Image();

var moveActive = new Image();
var moveRest = new Image();
var moveRoll = new Image();

var detailsActive = new Image();
var detailsRest = new Image();
var detailsRoll = new Image();

var hideActive = new Image();
var hideRest = new Image();
var hideRoll = new Image();

var followActive = new Image();
var followRest = new Image();
var followRoll = new Image();

var exploreActive = new Image();
var exploreRest = new Image();
var exploreRoll = new Image();

var quick = new Image();
var precise = new Image();
var fierce = new Image();
var attack_hover = new Image();
var attack_clicked = new Image();

var dodge = new Image();
var parry = new Image();
var brace = new Image();

var buttonRestImg = new Image();
var buttonHoverImg = new Image();
var buttonClickedImg = new Image();

var btnBuildRestImg = new Image();
var btnBuildClickedImg = new Image();

var btnCraftRestImg = new Image();
var btnAssignRestImg = new Image();
var btnSplitRestImg = new Image();
var btnEquipRestImg = new Image();

var oreIcon = new Image();
var woodIcon = new Image();
var stoneIcon = new Image();
var gameIcon = new Image();
var waterIcon = new Image();
var plantIcon = new Image();

var detailsButton = new createjs.Container();
var gatherButton = new createjs.Container();
var buildButton = new createjs.Container();
var moveButton = new createjs.Container();
var hideButton = new createjs.Container();
var followButton = new createjs.Container();
var exploreButton = new createjs.Container();

var quickButton = new createjs.Container();
var preciseButton = new createjs.Container();
var fierceButton = new createjs.Container();
var dodgeButton = new createjs.Container();
var parryButton = new createjs.Container();
var braceButton = new createjs.Container();

var twoComboButton = new createjs.Container();
var threeComboButton = new createjs.Container();
var fourComboButton = new createjs.Container();

var quickCooldown;
var preciseCooldown;
var fierceCooldown;
var dodgeCooldown;
var parryCooldown;
var braceCooldown;

var gravestone = new Image();

var forests = [18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30, 31];

var h1Font = "14px Verdana"
var textColor = "#FFFFFF";

var tileset;

var shroud = "/static/art/shroud.png";

var bank_n = "/static/art/tileset/flat/bank-n.png";
var bank_ne = "/static/art/tileset/flat/bank-ne.png";
var bank_nw = "/static/art/tileset/flat/bank-nw.png";
var bank_s = "/static/art/tileset/flat/bank-s.png";
var bank_se = "/static/art/tileset/flat/bank-se.png";
var bank_sw = "/static/art/tileset/flat/bank-sw.png";

infoPanelBg.src = "/static/art/ui_pane.png";
dialogPanelBg.src = "/static/art/dialog.png";
smallDialogPanelBg.src = "/static/art/small_dialog.png";
selectPanelBg.src = "/static/art/select_bar_bg.png";
leftImage.src = "/static/art/select_bar_left.png";
rightImage.src = "/static/art/select_bar_right.png";

close_rest.src = "/static/art/close_rest.png";
selectHexImage.src = "/static/art/hover-hex.png";
selectIconImage.src = "/static/art/select2.png";
portraitBg.src = "/static/art/unit_badge.png";
actionBarBgImage.src = "/static/art/ab_bg.png";
reventBg.src = "/static/art/revent_bg.png";
reventButtonBg.src = "/static/art/revent_button.png";
reventOkButton.src = "/static/art/revent_ok_button.png";

attackActive.src = "/static/art/ab_attack_active.png";
attackRest.src = "/static/art/ab_attack_rest.png";
attackRoll.src = "/static/art/ab_attack_roll.png";

gatherActive.src = "/static/art/ab_gather_active.png";
gatherRest.src = "/static/art/ab_gather_rest.png";

buildActive.src = "/static/art/ab_build_active.png";
buildRest.src = "/static/art/ab_build_rest.png";

detailsActive.src = "/static/art/ab_details_active.png";
detailsRest.src = "/static/art/ab_details_rest.png";
detailsRoll.src = "/static/art/ab_details_roll.png";

moveActive.src = "/static/art/ab_move_active.png";
moveRest.src = "/static/art/ab_move_rest.png";
moveRoll.src = "/static/art/ab_move_roll.png";

hideActive.src = "/static/art/ab_move_active.png";
hideRest.src = "/static/art/ab_move_rest.png";
hideRoll.src = "/static/art/ab_move_roll.png";

followActive.src = "/static/art/ab_follow_active.png";
followRest.src = "/static/art/ab_follow_active.png";
followRoll.src = "/static/art/ab_follow_active.png";

exploreActive.src = "/static/art/ab_details_active.png";
exploreRest.src = "/static/art/ab_details_rest.png";

reventBg.src = "/static/art/revent_bg.png";
quick.src = "/static/art/quick_rest.png"; 
precise.src = "/static/art/precise_rest.png"; 
fierce.src = "/static/art/fierce_rest.png"; 
attack_hover.src = "/static/art/attack_hover.png"; 
attack_clicked.src = "/static/art/attack_clicked.png"; 

dodge.src = "/static/art/dodge_rest.png"; 
parry.src = "/static/art/parry_rest.png"; 
brace.src = "/static/art/brace_rest.png"; 

btnBuildRestImg.src = "/static/art/ButtonBuildRest.png";
btnBuildClickedImg.src = "/static/art/ButtonBuildClicked.png";

btnCraftRestImg.src = "/static/art/btn_craft_rest.png";
btnSplitRestImg.src = "/static/art/btn_split_rest.png";
btnAssignRestImg.src = "/static/art/btn_assign_rest.png";
btnEquipRestImg.src = "/static/art/btn_equip_rest.png";

oreIcon.src = "/static/art/icons/ore.png";
woodIcon.src = "/static/art/icons/wood.png";
stoneIcon.src = "/static/art/icons/stone.png";
waterIcon.src = "/static/art/icons/water.png";
gameIcon.src = "/static/art/icons/game.png";
plantIcon.src = "/static/art/icons/plant.png";

gravestone.src = "/static/art/gravestone.png";

var villager = new Image();
villager.src = "/static/art/humanvillager2.png";

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

    var font = "https://fonts.googleapis.com/css?family=Alegreya";
    var loader = new createjs.FontLoader({src: font, type: "fontcss"}, true);
    loader.load();

    stage = new createjs.Stage(canvas);
    stage.autoClear = true;
    stage.snapToPixelEnabled = true;
    stage.enableMouseOver(10);

    createjs.Touch.enable(stage);

    map = new createjs.Container();
    map.x = $("#map").width() / 2;
    map.y = $("#map").height() / 2;

    stage.addChild(map)

    initImages();
    initUI();

    createjs.Ticker.timingMode = createjs.Ticker.RAF_SYNCH;
    createjs.Ticker.setFPS(60);
    createjs.Ticker.addEventListener("tick", stage);
    createjs.Ticker.addEventListener("tick", handleRender);

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

function handleRender(event) {
    if(startRender) {

        var currTime = createjs.Ticker.getTime();
        if((currTime - lastRenderTime) >= 200) {

            if(renderQueue.length > 0) {
                updateObjs(renderQueue.shift());
            }

            drawAllObj();

            lastRenderTime = currTime;
            startRender = false;
        }
    }
};

function initImages() {
    var manifest = [{src: "shroud.png", id: "shroud",
                     src: "white-mage.png", id: "white-mage",
                     src: "ui_pane.png", id: "ui_pane",
                     src: "close_rest.png" , id: "close_rest"}];
                
    loaderQueue = new createjs.LoadQueue(true);
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
        var image = loaderQueue.getResult(imageTask.id + "img");

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
        console.log(spriteTask);

        var spriteId = spriteTask.id.toLowerCase().replace(/ /g, '');
        var spriteImage = spriteTask.image.toLowerCase().replace(/ /g, '');
        console.log("JSON: " + spriteId + "json IMG: " + spriteImage + "ss");
        var spriteSheetJSON = loaderQueue.getResult(spriteId + "json");
        var spriteSheetIMG = loaderQueue.getResult(spriteImage + "ss");

        console.log("queueComplete: " + spriteSheetJSON + " " + spriteSheetIMG);
        if(spriteSheetJSON && spriteSheetIMG) {
            var json = {
                "frames": spriteSheetJSON.frames
            };
          
            if(spriteSheetJSON.hasOwnProperty("animations")) {
                json.animations = spriteSheetJSON.animations;
            }
            
            if(spriteSheetJSON.hasOwnProperty("images")) {           
                json.images = spriteSheetJSON.images;
            } else {
                json.images = [spriteSheetIMG];
            }
           
            var spriteSheet = new createjs.SpriteSheet(json)
            console.log(spriteSheet);

            if(spriteSheet.animations.length > 0) {
                var sprite = new createjs.Sprite(spriteSheet, spriteTask.animation);

                sprite.x = spriteTask.x;
                sprite.y = spriteTask.y;
                sprite.name = "sprite";
            
                if(!in_array(spriteSheet.animations, spriteTask.animation)) {
                    if(spriteTask.animation == 'dead') {
                        spriteTask.target.addChild(new createjs.Bitmap(gravestone));
                    }
                    else {
                        sprite.gotoAndPlay(spriteTask.animation);
                        spriteTask.target.addChild(sprite);
                    }
                } else {
                    sprite.gotoAndPlay(spriteTask.animation);
                    spriteTask.target.addChild(sprite);
                }
            } 
            else { 
                //Required to support spritesheet with multiple images and frames
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
    var spriteId = spriteTask.id.toLowerCase().replace(/ /g, '');
    var spriteImage = spriteTask.image.toLowerCase().replace(/ /g, '');
    var artPath = "/static/art/";

    console.log("JSON: " + spriteId + "json IMG: " + spriteImage + "ss");

    var spriteSheetJSON = loaderQueue.getResult(spriteId + "json");
    var spriteSheetIMG = loaderQueue.getResult(spriteImage + "ss");

    console.log("addSprite: " + spriteSheetJSON + " " + spriteSheetIMG);
    if(spriteSheetJSON && spriteSheetIMG) {

        var json = {
            "frames": spriteSheetJSON.frames
        };
      
        if(spriteSheetJSON.hasOwnProperty("animations")) {
            json.animations = spriteSheetJSON.animations;
        }
        
        if(spriteSheetJSON.hasOwnProperty("images")) {           
            json.images = spriteSheetJSON.images;
        } else {
            json.images = [spriteSheetIMG];
        }
       
        var spriteSheet = new createjs.SpriteSheet(json)

        console.log("Sprite loaded");

        if(spriteSheet.animations.length > 0) {
            
            var sprite = new createjs.Sprite(spriteSheet, spriteTask.animation);
            
            sprite.x = spriteTask.x;
            sprite.y = spriteTask.y;
            sprite.name = "sprite";

            if(!in_array(spriteSheet.animations, spriteTask.animation)) {
                if(spriteTask.animation == 'dead') {
                    spriteTask.target.addChild(new createjs.Bitmap(gravestone));
                }
                else {
                    sprite.gotoAndPlay(spriteTask.animation);
                    spriteTask.target.addChild(sprite);
                }
            } else {
                sprite.gotoAndPlay(spriteTask.animation);
                spriteTask.target.addChild(sprite);
            }
        } 
        else {
            //Required to support spritesheet with multiple images and frames
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
        loaderQueue.loadFile({id: spriteImage + "ss", src: artPath + spriteImage + '.png'});
        loaderQueue.loadFile({id: spriteId + "json", src: artPath + spriteId + ".json", type: "json"});
    }
};

function addImage(imageTask) {
    var image = loaderQueue.getResult(imageTask.id + "img");

    if(image) {
        
        var bitmap = new createjs.Bitmap(image);
        
        if(imageTask.hasOwnProperty("scale")) {
            var rect = bitmap.getBounds();
            
            bitmap.regX = -1 * rect.width / 2 - 10;
            bitmap.regY = -1 * rect.height / 2 - 10;

            bitmap.scaleX = imageTask.scale;
            bitmap.scaleY = imageTask.scale;
        }

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
        loaderQueue.loadFile({id: imageTask.id + "img", src: imageTask.path});        
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
    var move = '{"cmd": "move_unit", "id": ' + selectedPortrait + ', "x": ' + newX + ', "y": ' + newY + '}';

    websocket.send(move);
};

function sendStructureList() {
    var e = '{"cmd": "structure_list"}';
    websocket.send(e);
};

function sendAssignList() {
    var e = '{"cmd": "assign_list"}';
    websocket.send(e);
}

function sendAttack(attackType) {
    if(selectedUnit != -1) {
        var attack = '{"cmd": "attack", "attacktype": "' + attackType + '", "sourceid": ' + selectedPortrait + ', "targetid": ' + selectedUnit + '}';    
        websocket.send(attack);
    }
};

function sendDefend(defendType) {
    var guard = '{"cmd": "defend", "defendtype": "' + defendType + '", "sourceid": ' + selectedPortrait + '}';    
    websocket.send(guard);
};

function sendBuild(structureName) {
    var e = '{"cmd": "build", "sourceid": ' + selectedPortrait + ', "structure": "' + structureName + '"}';
    websocket.send(e);
};

function sendFinishBuild(structureid) {
    console.log("sendFinishBuild");
    var e = '{"cmd": "finish_build", "sourceid": ' + selectedPortrait + ', "structureid": ' + structureid + '}';
    websocket.send(e);
};

function sendRefine(structureid) {
    console.log("sendRefine");
    var e = '{"cmd": "refine", "structureid": ' + structureid + '}';
    websocket.send(e);
};

function sendFord(sourceid, x, y) {
    console.log("sendFord");
    var e = '{"cmd": "ford", "id": ' + selectedPortrait + ', "x": ' + selectedTile['x'] + ', "y": ' + selectedTile['y'] + '}';
    websocket.send(e);
};

function sendRecipeList(sourceid) {
    var e = '{"cmd": "recipe_list", "sourceid": ' + sourceid + '}';
    websocket.send(e);
};

function sendAssign(sourceid, targetid) {
    var e = '{"cmd": "assign", "sourceid": ' + sourceid + ', "targetid": ' + targetid + '}';
    websocket.send(e);
};

function sendFollow(sourceid) {
    var e = '{"cmd": "order_follow", "sourceid": ' + selectedPortrait + '}';
    websocket.send(e);
};

function sendOrderExplore(sourceid) {
    var e = '{"cmd": "order_explore", "sourceid": ' + selectedPortrait + '}';
    websocket.send(e);
};

function sendOrderGather(sourceid, restype) {
    var e = '{"cmd": "order_gather", "sourceid": ' + selectedPortrait + ', "restype": "' + restype + '"}';
    websocket.send(e);
};

function sendOrderHarvest(sourceid) {
    var e = '{"cmd": "order_harvest", "sourceid": ' + selectedPortrait + '}';
    websocket.send(e);
};

function sendCraft(sourceid, recipe) {
    var e = '{"cmd": "craft", "sourceid": ' + sourceid + ', "recipe": "' + recipe + '"}';
    websocket.send(e);
};

function sendLoot(sourceid, item) {
    var e = '{"cmd": "loot", "sourceid": ' + sourceid + ', "item": ' + item + '}';
    websocket.send(e);
};

function sendEquip(item) {
    var e = '{"cmd": "equip", "item": ' + item + '}';
    websocket.send(e);
};

function sendRest(sourceid) {
    var e = '{"cmd": "rest", "sourceid": ' + selectedPortrait + '}';
    websocket.send(e);
};

function sendHide(sourceid) {
    var e = '{"cmd": "order_attack", "sourceid": ' + selectedPortrait + ', "targetid": ' + selectedUnit + '}';
    websocket.send(e);
};

function sendCombo(comboType) {
    var e = '{"cmd": "combo", "sourceid": ' + selectedPortrait + ', "combotype": "' + comboType + '"}';
    websocket.send(e);
};

function sendItemTransfer(targetid, item) {
    console.log("targetid: " + targetid);
    var e = '{"cmd": "item_transfer", "targetid": ' + targetid + ', "item": ' + item + '}';
    websocket.send(e);
};

function sendItemSplit(item, quantity) {
    console.log("quantity: " + quantity);
    var e = '{"cmd": "item_split", "item": ' + item + ', "quantity": ' + quantity + '}';
    websocket.send(e);
};

function sendExplore(sourceid) {
    var e = '{"cmd": "explore", "sourceid": ' + selectedPortrait + '}';    
    websocket.send(e);
};

function sendSurvey(sourceid) {
    var e = '{"cmd": "survey", "sourceid": ' + selectedPortrait + '}';    
    websocket.send(e);
};

function sendHarvest(sourceid, resource) {
    var e = '{"cmd": "harvest", "sourceid": ' + selectedPortrait + ', "resource": "' + resource + '"}';    
    websocket.send(e);
};

function sendGetStats(id) {
    var e = '{"cmd": "get_stats", "id": ' + id + '}';
    websocket.send(e);
};

function sendInfoUnit(id) {
    var info = '{"cmd": "info_unit", "id": ' + id + '}';
    websocket.send(info);
};

function sendInfoItem(id) {
    var info = '{"cmd": "info_item", "id": ' + id + '}';
    websocket.send(info);
};

function sendInfoItemByName(name) {
    var info = '{"cmd": "info_item_by_name", "name": "' + name + '"}';
    websocket.send(info);
}

function sendInfoTile(x, y) {
    var info = '{"cmd": "info_tile", "x": ' + x + ', "y": ' + y + '}';
    websocket.send(info);
};

function sendReventResponse(responseNum) {
    var revent = '{"cmd": "revent_response", "response_num": ' + responseNum + '}';
    websocket.send(revent);
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

            playerId = jsonData.player;
            //explored = jsonData.explored;

            //setObjs(jsonData.objs);

            //startRender = true;

            //setPlayer();
            //drawMap(jsonData.map);
        }
        else if(jsonData.packet == "perception") {
            var data = jsonData.data;

            drawMap(data.map);

            setObjs(data.objs);

            setPlayer();

            startRender = true;
        }
        else if(jsonData.packet == "events") {
            processEvents(jsonData.data);
        }
        else if(jsonData.packet == "map") {
            drawMap(jsonData.data);
        }
        else if(jsonData.packet == "changes") {
            renderQueue.push(jsonData);

            startRender = true;
        }
        else if(jsonData.packet == "loot_perception") {
            drawLootDialog(jsonData);
        }        
        else if(jsonData.packet == "item_transfer") {
            if(jsonData.result == "success") {
                /* for(var i = infoPanels.length - 1; i >= 0; i--) {
                    if(infoPanels[i].hasOwnProperty("unitTemplate")) {
                        infoPanels[i].visible = false;
                        sendInfoUnit(infoPanels[i].id);
                    }
                }*/

            }
        }
        else if(jsonData.packet == "new_items") {
            items = jsonData.data 
            drawNewItemsDialog(items);

            for(var i = 0; i < items.length; i++) {
                updateTextLog("You acquired item [" + items[i].name + "]x" + items[i].quantity);
            }
        }
        else if(jsonData.packet == "revent") {
            drawReventPanel(jsonData, "responses");
        }
        else if(jsonData.packet == "revent_resolution") {
            drawReventPanel(jsonData, "resolution");
        }
        else if(jsonData.packet == "stats") {
            drawStats(jsonData.data);
        }
        else if(jsonData.packet == "dmg") {
            drawDmg(jsonData);
        }
        else if(jsonData.packet == "sound") {
            updateTextLog(jsonData.text);
        } 	
        else if(jsonData.packet == "speech") {
            drawSpeech(jsonData);
        } 	
        else if(jsonData.packet == "info_tile") {
            drawInfoTile(jsonData);
        }
        else if(jsonData.packet == "info_unit") {
            if(jsonData.state == "dead") {
                drawLootDialog(jsonData);
            }
            else {
                drawInfoObj(jsonData);
            }
        }
        else if(jsonData.packet == "info_item") {
            drawInfoItem(jsonData);
        }
        else if(jsonData.packet == "info_effect_update") {
            updateEffectsObjPanel(jsonData);
        }
        else if(jsonData.packet == "villager_change") {
            updateVillagerObjPanel(jsonData);
        }        
        else if(jsonData.packet == "info_item_update") {
            updateItemObjPanel(jsonData);
        }
        else if(jsonData.packet == "info_item_transfer") {
            transferItemObjPanel(jsonData);
        }
        else if(jsonData.packet == "survey") {
            drawSurveyDialog(jsonData.data);
        }
        else if(jsonData.packet == "structure_list") {
            drawStructureListDialog(jsonData);
        }     
        else if(jsonData.packet == "assign_list") {
            drawAssignListDialog(jsonData);
        }     
        else if(jsonData.packet == "recipe_list") {
            drawCraftListDialog(jsonData);
        }
        else if(jsonData.packet == "finish_build") {
            drawProgressBar(jsonData);
        }
        else if(jsonData.packet == "move") {
            if(jsonData.hasOwnProperty("errmsg")) {
                updateTextLog(jsonData.errmsg);
            } 
        }
        else if(jsonData.packet == "attack") {
            if(jsonData.hasOwnProperty("errmsg")) {
                updateTextLog(jsonData.errmsg);
            }
            else {
                drawAttackClicked(jsonData.attacktype);
                drawAttackCooldown(jsonData.cooldown);
            }
        }
        else if(jsonData.packet == "defend") {
            if(jsonData.hasOwnProperty("errmsg")) {
                updateTextLog(jsonData.errmsg);
            }
            else {
                updateTextLog("You begin to " + jsonData.defendtype + " incoming attacks");
                drawDefendCooldown(jsonData.cooldown);
            }
       }
       else if(jsonData.packet == "world") {
            if(jsonData.hasOwnProperty("time")) {
                if(jsonData.time == "day") {
                    updateTextLog("The warmth of the sun arrives");
                } else if (jsonData.time == "night") {
                    updateTextLog("Darkness sets across the land");
                } else if (jsonData.time == "bloodmoon") {
                    updateTextLog("The Bloodmoon lifts itself into the night sky!");
                }
            }
        }
    }
    console.log(evt.data);
    showScreen('<span style="color: blue;">RESPONSE: ' + evt.data+ '</span>'); 
};

function setObjs(jsonObjs) {
    console.log("setObj");
    objs = jsonObjs;

    for(var i = 0; i < objs.length; i++) {
        var obj = objs[i];

        localObjs[obj.id] = obj;
        localObjs[obj.id].op = 'added';
    }
};

function updateObjs(packetChanges) {
    console.log("updateObj");

    var events = packetChanges.events;

    //Reset the operation
    for(var localObjId in localObjs) {
        var localObj = localObjs[localObjId];
        localObj.op = 'none';
    }

    for(var i = 0; i < events.length; i++) {
        var eventType = events[i].event;

        if(eventType == "obj_create") {
            var obj = events[i].obj;

            localObjs[obj.id] = obj;
            localObjs[obj.id].op = 'added';
            
        } else if(eventType == "obj_update") {
            var obj_id = events[i].obj_id;
            var attr = events[i].attr;
            var value = events[i].value;

            if(attr == 'state') {
                localObjs[obj_id].state = value;
                updateAttrObjPanel(obj_id, 'state', value);
            }

            localObjs[obj_id].op = 'updated';
        } else if(eventType == "obj_move") {
            var obj = events[i].obj;
            var src_x = events[i].src_x;
            var src_y = events[i].src_y;      

            if(obj.id in localObjs) {
                localObjs[obj.id].state = obj.state;
                localObjs[obj.id].x = obj.x;
                localObjs[obj.id].y = obj.y;
                localObjs[obj.id].op = 'updated';
            } else {
                localObjs[obj.id] = obj;
                localObjs[obj.id].eventType = 'obj_move';
                localObjs[obj.id].prev_x = src_x;
                localObjs[obj.id].prev_y = src_y;
                localObjs[obj.id].op = 'added';
            }            

            updateAttrObjPanel(obj.id, 'state', obj.state);

        } else if(eventType =="obj_delete") {            
            var obj_id = events[i].obj_id;

            localObjs[obj_id].op = 'deleted';
        } 

    }
};

function setPlayer() {
    for(var i = 0; i < objs.length; i++) {
        if(objs[i].player == playerId && is_hero(objs[i].subclass)) {
            heroId = objs[i].id;
            selectedUnit = heroId;
            drawSelectedPortrait();

            if(objs[i].state == "dead") {
                updateTextLog("You are dead...");
            }
        }
    }
};

function clearMap() {
    var localMapCont = localPanel.getChildByName("localMap");
    var baseCont = localMapCont.getChildByName("localTiles");
    var localShroudCont = localMapCont.getChildByName("localShroud");

    baseCont.removeAllChildren();
    localShroudCont.removeAllChildren();
};

function clearObj() {
    var localMapCont = localPanel.getChildByName("localMap");
    var localObjsCont1 = localMapCont.getChildByName("localObjs1"); 
    var localObjsCont2 = localMapCont.getChildByName("localObjs2"); 
    
    localObjsCont1.removeAllChildren();
    localObjsCont2.removeAllChildren();
    
    selectHex.visible = false;
};

function drawMap(tiles) {
    console.log("drawMap");
    showLocalPanel();
    var localMapCont = localPanel.getChildByName("localMap");
    var baseCont = localMapCont.getChildByName("base");
    var transCont = localMapCont.getChildByName("trans");
    var extraCont = localMapCont.getChildByName("extra");
    var voidCont = localMapCont.getChildByName("void");

    baseCont.removeAllChildren();
    transCont.removeAllChildren();
    extraCont.removeAllChildren();
    voidCont.removeAllChildren();

    for(var i = 0; i < tiles.length; i++)
        addLocalTile(tiles[i]);

    var tileArray = [];

    for(var tileKey in localTiles) {
        tileArray.push(localTiles[tileKey])
    }

    tileArray.sort(function(a,b) {return (a.y > b.y) ? 1 : ((b.y > a.y) ? -1 : 0);} );     


    for(var i = 0; i < tileArray.length; i++) {
        var tile = tileArray[i];
        var pixel = hex_to_pixel(tile.x, tile.y);
        var tileImages = tile.t;

        var icon = new createjs.Container();

        icon.x = pixel.x;
        icon.y = pixel.y;
        icon.tileX = tile.x;
        icon.tileY = tile.y;
        icon.tileImages = tileImages;

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
                selectHex.tileX = this.tileX;
                selectHex.tileY = this.tileY;
                selectHex.visible = true;

                if(moveToggled) {
                    sendMove(this.tileX, this.tileY);
                    moveToggled = false;
                } else {
                    drawSelectPanel(this.tileX, this.tileY, this.tileImages);
                }
            }
        });

        baseCont.addChild(icon);

        for(var j = 0; j < tile.t.length; j++) {
            var tileType = tile.t[j];

            if(tileType < 18) {
                var tileImageId = tileImages[j] - 1;
                var imagePath = "/static/art/" + tileset[tileImageId].image;
                var offsetX = tileset[tileImageId].offsetx;
                var offsetY = -1 * tileset[tileImageId].offsety;
             
                addImage({id: tileImageId, path: imagePath, x: offsetX, y: offsetY, target: icon, index: 0});
            } else if(forests.indexOf(tileType) == -1) {
                var imagePath = "/static/art/" + tileset[0].image;
                addImage({id: 0, path: imagePath, x: offsetX, y: offsetY, target: icon, index: 0});
            }
        }

        tile.icon = icon;
    }

    for(var i = 0; i < tileArray.length; i++) {
        var tile = tileArray[i];
        var tileType = tile.t[tile.t.length - 1];
        var pixel = hex_to_pixel(tile.x, tile.y);

        if(tileType == 3 || tileType == 4 || tileType == 5 || tileType == 17) { //Water
            var neighbours = getNeighbours(tile.x, tile.y);

            for(var neighbourId in neighbours) {
                var neighbour = neighbours[neighbourId];
                var otherTile = getLocalTile(neighbour.q, neighbour.r);

                if(otherTile == false)
                    continue;

                var otherTileType = otherTile.t[otherTile.t.length - 1];

                if(otherTileType != 3 && otherTileType != 4 &&
                   otherTileType != 5 && otherTileType != 17) {
                    if(otherTileType == 2 || otherTileType == 16 || otherTileType == 33 || otherTileType == 37 || otherTileType == 38) {
                        var imagePath = "/static/art/tileset/frozen/snow-" + neighbour.d + ".png";
                        addImage({id: "snow" + neighbour.d, path: imagePath, x: pixel.x, y: pixel.y, target: transCont, index: 0});
                    } else if(otherTileType == 6 || otherTileType == 7 || otherTileType == 8 || otherTileType == 9) {
                        var imagePath = "/static/art/tileset/flat/bank-to-ice-" + neighbour.d + ".png";
                        addImage({id: neighbour.d, path: imagePath, x: pixel.x, y: pixel.y, target: transCont, index: 0});

                        var imagePath = "/static/art/tileset/grass/dry-abrupt-" + neighbour.d + ".png";
                        addImage({id: "plains" + neighbour.d, path: imagePath, x: pixel.x, y: pixel.y, target: transCont, index: 1});
                    } else if(otherTileType == 10 || otherTileType == 12) {
                        var imagePath = "/static/art/tileset/sand/desert-" + neighbour.d + ".png";
                        addImage({id: "desert" + neighbour.d, path: imagePath, x: pixel.x, y: pixel.y, target: transCont, index: 0});
                    } else {
                        var imagePath = "/static/art/tileset/flat/bank-to-ice-" + neighbour.d + ".png";
                        addImage({id: neighbour.d, path: imagePath, x: pixel.x, y: pixel.y, target: transCont, index: 0});

                        var imagePath = "/static/art/tileset/grass/green-abrupt-" + neighbour.d + ".png";
                        addImage({id: "grass" + neighbour.d, path: imagePath, x: pixel.x, y: pixel.y, target: transCont, index: 1});
                    }
                }
            }
        } else if(tileType == 2 || tileType == 16) {
            var neighbours = getNeighbours(tile.x, tile.y);

            for(var neighbourId in neighbours) {
                var neighbour = neighbours[neighbourId];
                var otherTile = getLocalTile(neighbour.q, neighbour.r);

                if(otherTile == false)
                    continue;

                var otherTileType = otherTile.t[otherTile.t.length - 1];
            
                if(otherTileType == 6 || otherTileType == 7 || otherTileType == 8 || otherTileType == 9) {
                    var imagePath = "/static/art/tileset/grass/dry-abrupt-" + neighbour.d + ".png";
                    addImage({id: "plains" + neighbour.d, path: imagePath, x: pixel.x, y: pixel.y, target: transCont, index: 0});
                } else if(otherTileType == 1) {
                    var imagePath = "/static/art/tileset/grass/green-abrupt-" + neighbour.d + ".png";
                    addImage({id: "grass" + neighbour.d, path: imagePath, x: pixel.x, y: pixel.y, target: transCont, index: 0});
                } else if(otherTileType == 16) {
                    var imagePath = "/static/art/tileset/hills/snow-" + neighbour.d + ".png";
                    addImage({id: "hillssnow" + neighbour.d, path: imagePath, x: pixel.x, y: pixel.y, target: transCont, index: 0});
                }
            }
        } else if(tileType == 1) {
            var neighbours = getNeighbours(tile.x, tile.y);

            for(var neighbourId in neighbours) {
                var neighbour = neighbours[neighbourId];
                var otherTile = getLocalTile(neighbour.q, neighbour.r);

                if(otherTile == false)
                    continue;

                var otherTileType = otherTile.t[otherTile.t.length - 1];

                if(otherTileType == 13) {
                    var imagePath = "/static/art/tileset/hills/regular-" + neighbour.d + ".png";
                    addImage({id: "hillsgrass" + neighbour.d, path: imagePath, x: pixel.x, y: pixel.y, target: transCont, index: 0});
                }
            }
        }
    }

    for(var i = 0; i < tileArray.length; i++) {
        var tile = tileArray[i];
        var tileImages = tile.t;
        var pixel = hex_to_pixel(tile.x, tile.y);


        for(var j = 0; j < tile.t.length; j++) { 
            var tileType = tile.t[j];

            //tileTypes under 18 have already been drawn above
            if(tileType < 18)
                continue

            var tileImageId = tileImages[j] - 1;
            var imagePath = "/static/art/" + tileset[tileImageId].image;
            var offsetX = parseInt(tileset[tileImageId].offsetx);
            var offsetY = parseInt(-1 * tileset[tileImageId].offsety);
         
            addImage({id: tileImageId, path: imagePath, x: offsetX + pixel.x, y: offsetY + pixel.y, target: extraCont});
        } 
    }

    var directions = ['n', 'ne', 'nw', 's', 'se', 'sw'];


    for(var tileKey in localTiles) {
        var tile = localTiles[tileKey];
        var neighbours = getNeighbours(tile.x, tile.y);
        
        var tileNeighbours = [];

        for(var neighbourId in neighbours) {
            var neighbour = neighbours[neighbourId];

            var neighbourTile = getLocalTile(neighbour.q, neighbour.r);

            if(neighbourTile == false) 
                tileNeighbours.push(neighbour);
        }

        if(tileNeighbours.length > 0) {
            for(var i = 0; i < tileNeighbours.length; i++) {
                var neighbour = tileNeighbours[i];
        
                var pixel = hex_to_pixel(tile.x, tile.y);
                var icon = new createjs.Container();

                icon.x = pixel.x;
                icon.y = pixel.y;

                voidCont.addChild(icon);

                var imagePath = "/static/art/tileset/void/void-" + neighbour.d + ".png";
                addImage({id: "void" + neighbour.d, path: imagePath, x: 0, y: 0, target: icon, index: 0});
                
                var border = new createjs.Container();
                pixel = hex_to_pixel(neighbour.q, neighbour.r); 

                border.x = pixel.x;
                border.y = pixel.y;

                voidCont.addChild(border);

                var imagePath = "/static/art/tileset/void/void.png"
                addImage({id: "void", path: imagePath, x: 0, y: 0, target: border, index: 0});
            }
        }
    }

};


function drawAllObj() {
    console.log("drawAllObj - start");
    showLocalPanel();

    var localMapCont = localPanel.getChildByName("localMap");
    var localObjsCont1 = localMapCont.getChildByName("localObjs1"); 
    var localObjsCont2 = localMapCont.getChildByName("localObjs2"); 
    var localShroudCont = localMapCont.getChildByName("localShroud"); 

    localShroudCont.removeAllChildren();

    visibleTiles = [];

    for(var id in localObjs) {
        var localObj = localObjs[id];

        if(localObj.op == 'added') {

            var pixel = hex_to_pixel(localObj.x, localObj.y);
            var template = localObj.template;
            template = template.toLowerCase().replace(/ /g, '');
            var imagePath =  "/static/art/";
            var icon = new createjs.Container();
            
            if(localObj.eventType == 'obj_move') {
                var p = hex_to_pixel(localObj.prev_x, localObj.prev_y);

                icon.x = p.x;
                icon.y = p.y;
            } else {
                icon.x = pixel.x;
                icon.y = pixel.y;
            } 

            icon.player = localObj.player;
            icon.name = localObj.id;
            
            if(localObj.class == "structure") {             
                if(localObj.state == "founded") {
                    var imagePath = "/static/art/foundation.json";
                    template = "foundation";
                }

                addChildLocalMap(icon, "localObjs1");
            }
            else {
                addChildLocalMap(icon, "localObjs2");
            }

            if(localObj.player == playerId) {
                if(is_hero(localObj.subclass)) {
                    c_x = 640 - 36 - pixel.x;
                    c_y = 400 - 36 - pixel.y;
                    console.log("new c_x: " + c_x + " c_y: " + c_y);
                    
                    function showMap() {
                        $("#map").fadeIn('slow');
                    };

                    createjs.Tween.get(localMapCont).to({x: c_x, y: c_y}, 500, createjs.Ease.getPowInOut(2)).call(showMap);
                }
            }

            addSprite({id: localObj.template, x: 0, y: 0, target: icon, animation: localObj.state, image: localObj.image}); 

            localObj.icon = icon;

            if(localObj.eventType == 'obj_move') {
                createjs.Tween.get(localObj.icon).to({x: pixel.x, y: pixel.y}, 500, createjs.Ease.linear);
            }
        } 
        else if(localObj.op == 'deleted') {

            //Check if local has an icon and then delete
            if(localObj.hasOwnProperty('icon')) {       
                var cont = localObj.icon.parent;
                cont.removeChild(localObj.icon);

                delete localObjs[id];
            }
        }
        else if(localObj.op == 'updated') {

            if(localObj.state == "moving") {
                var sprite = localObj.icon.getChildByName("sprite");
                sprite.gotoAndPlay("moving");

                var pixel = hex_to_pixel(localObj.x, localObj.y);

                localObj.icon.x = pixel.x
                localObj.icon.y = pixel.y

            } else if(localObj.class == "structure") {
                if(localObj.state == "none") {
                    var template = localObj.template;

                    localObj.icon.removeAllChildren();        
                    addSprite({id: localObj.template, x: 0, y: 0, target: localObj.icon, animation: localObj.state, image: localObj.image});
                }
            } 
            else {
                var animation;

                if((localObj.state == "dead") && (localObj.prev_state != "dead")) {
                    animation = "die";
                } else {
                    animation = localObj.state;
                }

                var sprite = localObj.icon.getChildByName("sprite");

                if(sprite != null)
                    sprite.gotoAndPlay(animation);

                var pixel = hex_to_pixel(localObj.x, localObj.y);


                if(localObj.player == playerId) {

                    if(is_hero(localObj.subclass)) {
                        localObj.icon.x = pixel.x;
                        localObj.icon.y = pixel.y;

                        c_x = 640 - 36 - pixel.x;
                        c_y = 400 - 36 - pixel.y;             
                        console.log("x: " + localMapCont.x + " y: " + localMapCont.y + " - " + "c_x: " + c_x + " c_y: " + c_y);
                        createjs.Tween.get(localMapCont)
                                      .to({x: c_x, y: c_y}, 500, createjs.Ease.linear)
                                      .call(handlePlayerMoveComplete);

                    } else {
                        createjs.Tween.get(localObj.icon)
                            .to({x: pixel.x, y: pixel.y}, 500, createjs.Ease.linear)
                            .call(handlePlayerMoveComplete);
                    }
                } else {
                    createjs.Tween.get(localObj.icon)
                        .to({x: pixel.x, y: pixel.y}, 500, createjs.Ease.linear)
                        .call(handleMoveComplete);
                }
            }
        }
    }

    for(var id in localObjs) {
        var localObj = localObjs[id];

        if(localObj.player == playerId && localObj.vision > 0) {
            var visibleRangeObj = range(localObj.x, localObj.y, localObj.vision);
            visibleTiles = visibleTiles.concat(visibleRangeObj);
        }
    }

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

    var stockades = [];

    for(var id in localObjs) {
        var obj = localObjs[id];

        if(obj.template == "Stockade" && obj.state == "none") {
            stockades.push(obj);
        }

    }

    for(var stockadeId in stockades) {
        var stockade = stockades[stockadeId];
        var neighbours = getNeighbours(stockade.x, stockade.y);

        for(var neighbourId in neighbours) {
            var neighbour = neighbours[neighbourId];

            for(var otherId in stockades) {
                var other = stockades[otherId];

                if((neighbour.q == other.x) && (neighbour.r == other.y)) {
                    if(stockade.icon.numChildren > 0) {
                        if(neighbour.d == "nw") {
                            stockade.icon.getChildAt(2).visible = false;
                            stockade.icon.getChildAt(4).visible = false;
                        } else if(neighbour.d == "ne") {
                            stockade.icon.getChildAt(3).visible = false;
                            stockade.icon.getChildAt(5).visible = false;
                        } else if(neighbour.d == "n") {
                            stockade.icon.getChildAt(0).visible = false; 
                            stockade.icon.getChildAt(1).visible = false;
                            stockade.icon.getChildAt(4).visible = false;
                            stockade.icon.getChildAt(5).visible = false;
                        } else if(neighbour.d == "s") {
                            stockade.icon.getChildAt(8).visible = false;
                            stockade.icon.getChildAt(9).visible = false;
                        } else if(neighbour.d == "sw") {
                            stockade.icon.getChildAt(6).visible = false;
                        } else if(neighbour.d == "se") {
                            stockade.icon.getChildAt(7).visible = false;
                        }
                    }
                }
            }
        }
    }

    console.log("drawAllObj - end");
};

function handleMoveComplete(evt) {
    var icon = evt.target;
    var obj = localObjs[icon.name];

    if(obj != false) {
        if(!is_visible(obj.x, obj.y, visibleTiles)) {
            var cont = icon.parent;
            cont.removeChild(icon);

            delete localObjs[obj.id];
        }
    }
};

function handlePlayerMoveComplete(evt) {

    for(var id in localObjs) {
        var obj = localObjs[id];

        if(!is_visible(obj.x, obj.y, visibleTiles)) {
            var cont = obj.icon.parent;
            cont.removeChild(obj.icon);

            delete localObjs[id];
        }
    }
};

function drawSelectPanel(tileX, tileY) {
    var tile = getLocalTile(tileX, tileY);
    var tileImages = tile.t.reverse();
    var localObjs = getLocalObjsAt(tileX, tileY);
    var icons = [];

    var content = selectPanel.getChildByName("content");
    content.removeAllChildren();

    var hero = [];
    var units = [];
    var others = [];
    
    selectedUnit = -1;
    selectedTile = -1;

    for(var i = 0; i < localObjs.length; i++) {
        if(localObjs[i].subclass == 'hero') {
            hero.push(localObjs[i]);
        } else if(localObjs[i].player == playerId) {
            units.push(localObjs[i]);
        } else {
            others.push(localObjs[i]);
        }
    }

    localObjs = hero.concat(units).concat(others);
 
    for(var i = 0; i < localObjs.length; i++) {
        var template = localObjs[i].template;
        var icon = new createjs.Container();

        icon.x = 15 + i * 77; 
        icon.y = 5;
        icon.id = localObjs[i].id;
        icon.mouseChildren = false;
        icon.type = "obj";
        
        var selectIcon = new createjs.Bitmap(selectIconImage);
        
        selectIcon.x = 7;
        selectIcon.y = 7;
        selectIcon.name = "selectIcon";
        selectIcon.visible = false; 

        var hitArea  = new createjs.Shape();
        hitArea.graphics.beginFill("#000").drawRect(0,0,72,72);
        icon.hitArea = hitArea;

        icon.addChild(selectIcon);

        icon.on("mousedown", function(evt) {
            for(var i = 0; i < icons.length; i++) {
                var selectIcon = icons[i].getChildByName("selectIcon");
                selectIcon.visible = false;

                selectedUnit = -1;
                selectedTile = -1;
            }

            var selectIcon = this.getChildByName("selectIcon");
            selectIcon.visible = true;
        
            selectedUnit = this.id;
            drawSelectedPortrait();

            drawActionBar(selectedUnit);
        });
        
        content.addChild(icon);
        addSprite({id: template, x: 0, y: 0, target: icon, animation: "none", image: localObjs[i].image}); 
        
        icons.push(icon); 
    }

    var icon = new createjs.Container();
    var selectIcon = new createjs.Bitmap(selectIconImage);
    
    selectIcon.x = 7;
    selectIcon.y = 7;
    selectIcon.name = "selectIcon";
    selectIcon.visible = false; 


    //Terrain icon
    icon.x = 15 + localObjs.length * 77; 
    icon.y = 5;
    icon.mouseChildren = false;
    icon.type = "tile";
    icon.tileX = tileX;
    icon.tileY = tileY;
  
    var hitArea  = new createjs.Shape();
    hitArea.graphics.beginFill("#000").drawRect(0,0,72,72);
    icon.hitArea = hitArea;

    icon.on("mousedown", function(evt) {
        for(var i = 0; i < icons.length; i++) {
            var selectIcon = icons[i].getChildByName("selectIcon");
            selectIcon.visible = false;

            selectedUnit = -1;
            selectedTile = -1;
        }

        selectedTile = {"x": this.tileX, "y": this.tileY};

        var selectIcon = this.getChildByName("selectIcon");
        selectIcon.visible = true;
    });

    icon.addChild(selectIcon);   

    content.addChild(icon);

    var tileImageId = tileImages[tileImages.length - 1] - 1;
    var imagePath = "/static/art/" + tileset[tileImageId].image;
    var offsetX = tileset[tileImageId].offsetx;
    var offsetY = -1 * tileset[tileImageId].offsety;
     
    addImage({id: tileImageId, path: imagePath, x: offsetX, y: offsetY, target: icon, index: 0, scale: 0.5});

    icons.push(icon);

    //Select first icon by default
    selectedUnit = icons[0].id;
    drawSelectedPortrait();
    drawActionBar(selectedUnit);
};

function drawSelectedPortrait() {
    if(selectedUnit != -1) {    
        var obj = localObjs[selectedUnit];

        if(obj == null)
            return;

        if(obj.player == playerId && obj.class == "unit") {
            var content = portraitPanel.getChildByName("content");
            content.removeAllChildren();

            selectedPortrait = selectedUnit;

            var template = obj.template.toLowerCase().replace(/ /g, '');
            var text = new createjs.Text(template, h1Font, textColor);

            text.x = 160;
            text.y = 9;
            text.textAlign = "center";
            text.lineWidth = 160; 

            content.addChild(text);

            addSprite({id: obj.template, x: 10, y: 5, target: content, animation: "none", image: obj.image});

            sendGetStats(selectedPortrait);
        }
    }
};

function drawStats(stats) {
    var barWidth = 152;
    var hpRatio = stats.hp / stats.base_hp;
    var staminaRatio = stats.stamina / stats.base_stamina;

    hpBar.graphics.clear()
        .setStrokeStyle(10)
        .beginStroke('#8E0000')
        .moveTo(100, 40)
        .lineTo(100 + (hpRatio * barWidth), 40);

    staminaBar.graphics.clear()
        .setStrokeStyle(10)
        .beginStroke('#009C0A')
        .moveTo(100, 55)
        .lineTo(100 + (staminaRatio * barWidth), 55);
};

function drawAttackClicked(attacktype) {
    if(attacktype == "quick") {
        quickButton.addChild(clicked);
    }
    else if(attacktype == "precise") {
        preciseButton.addChild(clicked);
    }
    else if(attacktype == "fierce") {
        fierceButton.addChild(clicked);
    }
};

function drawDefendClicked(defendtype) {
    if(defendtype == "dodge") {
        dodgeButton.addChild(clicked);
    }
    else if(defendtype == "parry") {
        parryButton.addChild(clicked);
    }
    else if(defendtype == "brace") {
        braceButton.addChild(clicked);
    }
};

function cooldownComplete() {
    removeClicked();
};

function drawAttackCooldown(time) {
    quickCooldown.visible = true;
    preciseCooldown.visible = true;
    fierceCooldown.visible = true;

    quickCooldown.scaleY = 1;
    preciseCooldown.scaleY = 1;
    fierceCooldown.scaleY = 1;

    quickCooldown.graphics.clear()
        .beginFill('rgba(50,50,50,0.75)')
        .rect(0,0,50,50);
    preciseCooldown.graphics.clear()
        .beginFill('rgba(50,50,50,0.75)')
        .rect(0,0,50,50);
    fierceCooldown.graphics.clear()
        .beginFill('rgba(50,50,50,0.75)')
        .rect(0,0,50,50);

    createjs.Tween.get(quickCooldown).to({scaleY: 0}, time*1000).call(cooldownComplete);
    createjs.Tween.get(preciseCooldown).to({scaleY: 0}, time*1000);
    createjs.Tween.get(fierceCooldown).to({scaleY: 0}, time*1000);
};

function drawDefendCooldown(time) {
    dodgeCooldown.visible = true;
    parryCooldown.visible = true;
    braceCooldown.visible = true;

    dodgeCooldown.scaleY = 1;
    parryCooldown.scaleY = 1;
    braceCooldown.scaleY = 1;

    dodgeCooldown.graphics.clear()
        .beginFill('rgba(50,50,50,0.75)')
        .rect(0,0,50,50);
    parryCooldown.graphics.clear()
        .beginFill('rgba(50,50,50,0.75)')
        .rect(0,0,50,50);
    braceCooldown.graphics.clear()
        .beginFill('rgba(50,50,50,0.75)')
        .rect(0,0,50,50);

    createjs.Tween.get(dodgeCooldown).to({scaleY: 0}, time*1000).call(cooldownComplete);
    createjs.Tween.get(parryCooldown).to({scaleY: 0}, time*1000);
    createjs.Tween.get(braceCooldown).to({scaleY: 0}, time*1000);
};

function drawDmg(jsonData) {
    if(localPanel.visible) {
        var source = getLocalObj(jsonData.sourceid);
        var target = getLocalObj(jsonData.targetid);
        var txt = '';

        if(jsonData.countered != false) {
            if(jsonData.sourceid == heroId) {
                txt = "Your " + jsonData.attacktype + " attack has been " + jsonData.countered + " for " + jsonData.dmg + " damage to " + target.name;
            }
            else if(jsonData.targetid == heroId) {
                txt = source.name + " " + jsonData.attacktype + " attack has been " + jsonData.countered + " by you for " + jsonData.dmg + " damage";               
            }
            else {
                txt = source.name + " " + jsonData.attacktype + " attack has been " + jsonData.countered + " by " + target.name + " for " + jsonData.dmg + " damage";
            }
        }
        else if(jsonData.combo != false) {
            if(jsonData.sourceid == heroId) {
                txt = "Your " + jsonData.attacktype + " attack unleashes a " + jsonData.combo + " for " + jsonData.dmg + " damage to " + target.name;
            }
            else if(jsonData.targetid == heroId) {
                txt = source.name + " " + jsonData.attacktype + " attack unleashes a " + jsonData.combo + " on you for " + jsonData.dmg + " damage";               
            }
            else {
                txt = source.name + " " + jsonData.attacktype + " attack unleashes a " + jsonData.combo + " on " + target.name + " for " + jsonData.dmg + " damage";
            }
        }
        else {
            if(jsonData.sourceid == heroId) {
                txt = "Your "  + jsonData.attacktype + " attack deals " + jsonData.dmg + " damage to " + target.name;            
            }
            else if(jsonData.targetid == heroId) {
                txt = source.name + " " + jsonData.attacktype + " attacks you for " + jsonData.dmg + " damage";
            }
            else {
                txt = source.name + " " + jsonData.attacktype + " damages " + target.name + " for " + jsonData.dmg + " damage";
            }
        }
        
        updateTextLog(txt);

        if(source.hasOwnProperty("icon")) {
            var origX = source.icon.x + 36;
            var origY = source.icon.y + 36;

            var dmgText = new createjs.Text(jsonData.dmg, 'bold 18px Verdana', '#FF0000');
            dmgText.x = target.icon.x + 33;
            dmgText.y = target.icon.y - 10;         
            dmgText.textAlign = "center";

            addChildLocalMap(dmgText, "textLayer");
            createjs.Tween.get(dmgText).to({alpha: 0},3000);

            if(source && target) {
                
                var sprite = source.icon.getChildByName("sprite");
                sprite.gotoAndPlay("attack");

                var targetX = target.icon.x + 36;
                var targetY = target.icon.y + 36;

                var diffX = (targetX - origX) * 0.5;
                var diffY = (targetY - origY) * 0.5;

                var destX = (origX + diffX) - 36;
                var destY = (origY + diffY) - 36;

                if(jsonData.attacktype == "Shadow Bolt") {
                    
                    var projectile = new createjs.Container();
                    projectile.x = source.icon.x;
                    projectile.y = source.icon.y;
                    addChildLocalMap(projectile, "textLayer");

                    addSprite({id: "shadowbolt", x: 0, y: 0, target: projectile, animation: "projectile", image: "shadowbolt"});


                    createjs.Tween.get(projectile).to({x: target.icon.x, y: target.icon.y}, 1000, createjs.Ease.getPowInOut(4))
                                                  .call(projectileComplete);
                                                   
                } else {
                    createjs.Tween.get(source.icon).to({x: destX, y: destY}, 1000, createjs.Ease.getPowInOut(4))
                                                   .to({x: origX - 36, y: origY - 36}, 200, createjs.Ease.getPowInOut(2));
                }
            }

            if(jsonData.state == "dead") {
                if(jsonData.targetid == heroId) {
                    var sprite = target.icon.getChildByName("sprite");
                    sprite.gotoAndPlay("die");
                    txt = "You have been killed by " + source.name;
                } 
                else if(jsonData.sourceid == heroId) {
                    txt = "You have killed a " + target.name;
                }                
                else {
                    txt = target.name + " has been killed by " + source.name;
                }

                updateTextLog(txt);
            }
        }        
    }
};

function projectileComplete(evt) {
    var projectile = evt.target;
    removeChildLocalMap(projectile, "textLayer");
}

function drawSpeech(jsonData) {
    if(localPanel.visible) {
        var source = getLocalObj(jsonData.source);

        if(source != false) {

            var speechText = new createjs.Text(jsonData.text, '14px Alegreya', '#FFFFFF');
            speechText.x = source.icon.x + 36;
            speechText.y = source.icon.y - 15;
            speechText.textAlign = "center";
            speechText.lineWidth = 120;

            if(jsonData.text.length > 5) {

                var roundedRect = new createjs.Shape();
                roundedRect.graphics.setStrokeStyle(1);
                roundedRect.graphics.beginFill("rgba(0,0,0,0.5)");
                roundedRect.graphics.drawRoundRect(source.icon.x - 60 + 36, 
                                                   source.icon.y - 20,
                                                   120,
                                                   40,
                                                   5);

                addChildLocalMap(roundedRect, "textLayer");
                createjs.Tween.get(roundedRect).to({alpha: 0},6000);
            }

            addChildLocalMap(speechText, "textLayer");
            createjs.Tween.get(speechText).to({alpha: 0},6000);
            updateTextLog(source.name + ": " + jsonData.text);
        }
    }
};

function drawLootDialog(jsonData) {
    showSmallDialogPanel();

    var title = new createjs.Text("Corpse of " + jsonData.name, h1Font, textColor);
    title.x = Math.floor(smallDialogPanelBg.width / 2);
    title.y = 5;
    title.textAlign = "center";

    addChildSmallDialogPanel(title);

    var numItems = jsonData.items.length;

    for(var i = 0; i < numItems; i++) {
        var itemName = jsonData.items[i].name;            
        itemName = itemName.toLowerCase().replace(/ /g,'');

        var imagePath = "/static/art/" + itemName + ".png";
        var icon = new createjs.Container();
        
        icon.itemId = jsonData.items[i].id;


        var hitArea  = new createjs.Shape();
        hitArea.graphics.beginFill("#000").drawRect(0,0,48,48);
        icon.hitArea = hitArea;

        icon.on("mousedown", function(evt) {
            sendLoot(selectedPortrait, this.itemId);
        });

        icon.x = smallDialogPanelBg.width / 2 - (numItems * 24) + (i * 50);
        icon.y = smallDialogPanelBg.height / 2 + 5 - 24;

        addChildSmallDialogPanel(icon);
        addImage({id: itemName, path: imagePath, x: 0, y: 0, target: icon});
    }

}

function drawSurveyDialog(resources) {
    showSmallDialogPanel();

    var title = new createjs.Text("Resources", h1Font, textColor);
    title.x = Math.floor(smallDialogPanelBg.width / 2);
    title.y = 5;
    title.textAlign = "center";

    addChildSmallDialogPanel(title);

    for(var i = 0; i < resources.length; i++) {
        var resource = resources[i];
        var resourceImage = resource.name.toLowerCase().replace(/ /g, '');
        var imagePath = "/static/art/" + resourceImage + ".png";

        var icon = new createjs.Container();
        icon.resourceName = resource.name;

        icon.x = 25;
        icon.y = 40 + i * 60;

        icon.on("mousedown", function(evt) {
            sendHarvest(selectedPortrait, this.resourceName);
            smallDialogPanel.visible = false;
        });

        addChildSmallDialogPanel(icon);
        addImage({id: resourceImage, path: imagePath, x: 0, y: 0, target: icon});

        var name = new createjs.Text("Name: " + resource.name, h1Font, textColor);
        var quantity = new createjs.Text("Quantity: " + resource.quantity, h1Font, textColor);
        
        name.x = 85;
        name.y = 40 + i * 60;
        
        quantity.x = 85;
        quantity.y = 60 + i * 60;
        
        addChildSmallDialogPanel(name);
        addChildSmallDialogPanel(quantity);
    }
    
    var btnExplore = new createjs.Container();
    var btnExploreRest = new createjs.Bitmap(btnEquipRestImg);

    btnExplore.visible = true;
    btnExplore.addChild(btnExploreRest);
    
    var btnExploreBounds = btnExplore.getBounds();

    btnExplore.x = Math.floor(smallDialogPanelBg.width / 2) - (btnExploreBounds.width / 2);
    btnExplore.y = smallDialogPanelBg.height - btnExploreBounds.height - 5;

    addChildSmallDialogPanel(btnExplore); 

    btnExplore.on("mousedown", function(evt) {
        sendExplore(0);
    });
};

function drawResourceTypeDialog() {
    showSmallDialogPanel();

    var title = new createjs.Text("Resource Types", h1Font, textColor);
    title.x = Math.floor(smallDialogPanelBg.width / 2);
    title.y = 5;
    title.textAlign = "center";

    addChildSmallDialogPanel(title);

    var oreButton = new createjs.Container();
    var woodButton = new createjs.Container();
    var stoneButton = new createjs.Container();
    var waterButton = new createjs.Container();
    var gameButton = new createjs.Container();
    var plantButton = new createjs.Container();

    oreButton.x = 25;
    oreButton.y = 40;
    oreButton.addChild(new createjs.Bitmap(oreIcon));

    oreButton.on("mousedown", function(evt) {
        sendOrderGather(selectedPortrait, "Ore");
        smallDialogPanel.visible = false;
    });
 
    woodButton.x = 85;
    woodButton.y = 40;
    woodButton.addChild(new createjs.Bitmap(woodIcon));

    woodButton.on("mousedown", function(evt) {
        sendOrderGather(selectedPortrait, "Wood");
        smallDialogPanel.visible = false;
    });

    stoneButton.x = 145;
    stoneButton.y = 40;
    stoneButton.addChild(new createjs.Bitmap(stoneIcon));

    stoneButton.on("mousedown", function(evt) {
        sendOrderGather(selectedPortrait, "Stone");
        smallDialogPanel.visible = false;
    });
 
    waterButton.x = 205;
    waterButton.y = 40;
    waterButton.addChild(new createjs.Bitmap(waterIcon));

    waterButton.on("mousedown", function(evt) {
        sendOrderGather(selectedPortrait, "Water");
        smallDialogPanel.visible = false;
    });
 
    gameButton.x = 265;
    gameButton.y = 40;
    gameButton.addChild(new createjs.Bitmap(gameIcon));

    gameButton.on("mousedown", function(evt) {
        sendOrderGather(selectedPortrait, "Game");
        smallDialogPanel.visible = false;
    });

    plantButton.x = 325;
    plantButton.y = 40;
    plantButton.addChild(new createjs.Bitmap(plantIcon));

    plantButton.on("mousedown", function(evt) {
        sendOrderGather(selectedPortrait, "Plant");
        smallDialogPanel.visible = false;
    });

    addChildSmallDialogPanel(oreButton); 
    addChildSmallDialogPanel(woodButton); 
    addChildSmallDialogPanel(stoneButton); 
    addChildSmallDialogPanel(waterButton); 
    addChildSmallDialogPanel(gameButton); 
    addChildSmallDialogPanel(plantButton); 
}

function drawStructureListDialog(jsonData) {
    showDialogPanel();

    var title = new createjs.Text("Structure List", h1Font, textColor);
    title.x = Math.floor(dialogPanelBg.width / 2);
    title.y = 5;
    title.textAlign = "center";

    addChildDialogPanel(title);

    for(var i = 0; i < jsonData.result.length; i++) {
        var structure = jsonData.result[i];
        var structureImage = structure.name.toLowerCase().replace(/ /g, '');
        var imagePath = "/static/art/" + structureImage + ".png";

        var icon = new createjs.Container();
        icon.structureName = structure.name;

        icon.x = 25 + i * 75;
        icon.y = 50;

        icon.on("mousedown", function(evt) {
            sendBuild(this.structureName);
            dialogPanel.visible = false;
        });

        addChildDialogPanel(icon);
        addImage({id: structureImage, path: imagePath, x: 0, y: 0, target: icon});

        var name = new createjs.Text(structure.name, h1Font, textColor);
        
        name.x = 25 + i * 75;
        name.y = 130;
        
        addChildDialogPanel(name);

        for(var j = 0; j < structure.req.length; j++) {
            var req = structure.req[j];
            var reqText = new createjs.Text(req.type + " (" + req.quantity + ")", h1Font, textColor);

            reqText.x = 25 + i * 75;
            reqText.y = 145;

            addChildDialogPanel(reqText);
        }
   }
}

function drawAssignListDialog(jsonData) {
    showSmallDialogPanel();

    var title = new createjs.Text("Villagers", h1Font, textColor);
    title.x = Math.floor(smallDialogPanelBg.width / 2);
    title.y = 5;
    title.textAlign = "center";

    addChildSmallDialogPanel(title);

    for(var i = 0; i < jsonData.result.length; i++) {
        var villager = jsonData.result[i];

        var icon = new createjs.Container();
        icon.id = villager.id;

        icon.x = 17;
        icon.y = 25 + i * 60;

        var hitArea  = new createjs.Shape();
        hitArea.graphics.beginFill("#000").drawRect(0,0,72,72);
        icon.hitArea = hitArea;

        icon.on("mousedown", function(evt) {
            console.log("Id: " + this.id);

            sendAssign(this.id, selectedUnit);
            smallDialogPanel.visible = false;
        });

        addChildSmallDialogPanel(icon);

        addSprite({id: "humanvillager", x: 0, y: 0, target: icon, animation: "none", image: villager.image}); 

        var name = new createjs.Text("Name: " + villager.name, h1Font, textColor);
        var structure = new createjs.Text("Structure: " + villager.structure, h1Font, textColor);
        
        name.x = 85;
        name.y = 40 + i * 60;
        
        structure.x = 85;
        structure.y = 60 + i * 60;
        
        addChildSmallDialogPanel(name);
        addChildSmallDialogPanel(structure);
    } 
   
}

function drawCraftListDialog(jsonData) {
    showSmallDialogPanel();

    var title = new createjs.Text("Recipes", h1Font, textColor);
    title.x = Math.floor(smallDialogPanelBg.width / 2);
    title.y = 5;
    title.textAlign = "center";

    addChildSmallDialogPanel(title);

    for(var i = 0; i < 3; i++) {
        var recipe = jsonData.result[i];
        var recipeImage = recipe.item.toLowerCase().replace(/ /g, '');
        var imagePath = "/static/art/" + recipeImage + ".png";

        var icon = new createjs.Container();
        icon.name = recipe.item;
        icon.class = recipe.class;

        icon.x = 25;
        icon.y = 50 + i * 75;

        var hitArea  = new createjs.Shape();
        hitArea.graphics.beginFill("#000").drawRect(0,0,72,72);
        icon.hitArea = hitArea;

        icon.on("mousedown", function(evt) {
            console.log("Class: " + this.class);

            if(this.class == "Refine") {
                sendRefine(selectedUnit);
            }
            else {
                sendCraft(selectedUnit, this.name);
            }
            
            smallDialogPanel.visible = false;
        });

        addChildSmallDialogPanel(icon);
        addImage({id: recipeImage, path: imagePath, x: 0, y: 0, target: icon});

        var name = new createjs.Text(recipe.item, h1Font, textColor);
        
        name.x = 75;
        name.y = 50 + i * 75;
        
        addChildSmallDialogPanel(name);

        for(var j = 0; j < recipe.req.length; j++) {
            var req = recipe.req[j];
            var reqText = new createjs.Text(req.type + " (" + req.quantity + ")", h1Font, textColor);

            reqText.x = 250;
            reqText.y = 50 + j * 20 + i * 75;

            addChildSmallDialogPanel(reqText);
        }
    }
};

function drawNewItemsDialog(items) {
    showSmallDialogPanel();

    var title = new createjs.Text("Rewards", h1Font, textColor);
    title.x = Math.floor(smallDialogPanelBg.width / 2);
    title.y = 5;
    title.textAlign = "center";

    addChildSmallDialogPanel(title);

    for(var i = 0; i < items.length; i++) {
        var itemName = items[i].name;            
        itemName = itemName.toLowerCase().replace(/ /g,'');

        var imagePath = "/static/art/" + itemName + ".png";
        var icon = new createjs.Container();
       
        icon.itemId = items[i].id;
        icon.itemName = items[i].name;

        if(items[i].hasOwnProperty("id")) {
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
                if(!isInfoPanelOpened(this.itemId, "item")) {
                    sendInfoItem(this.itemId);
                }
            }
        });

        icon.x = smallDialogPanelBg.width / 2 - 24;
        icon.y = smallDialogPanelBg.height / 2 + 5 - 24;

        addChildSmallDialogPanel(icon);
        addImage({id: itemName, path: imagePath, x: 0, y: 0, target: icon});
    }

};

function drawInfoTile(jsonData) {
    var index = jsonData.x + "_" + jsonData.y;
    var infoPanel = initInfoPanel(index, "tile");
    var infoPanelContent = infoPanel.getChildByName('content');

    var tileName = jsonData.name + " (" + jsonData.x + ", " + jsonData.y + ")";   
    var sanctuary = jsonData.sanctuary ? "yes" : "no";
    var passable = jsonData.passable ? "yes" : "no";
   

    var nameText = new createjs.Text(tileName, h1Font, textColor);
    nameText.x = Math.floor(infoPanelBg.width / 2);
    nameText.y = 10;
    nameText.textAlign = "center";
  
    infoPanelContent.addChild(nameText);

    var stats = "Sanctuary: " + sanctuary + "\n" +
                "Wildness: " + jsonData.wildness + "\n" +
                "Movement Cost: " + jsonData.mc + "\n" + 
                "Defense Bonus: " + jsonData.def + "\n" + 
                "Passable: " + passable + "\n" +
                "Unrevealed Resources: " + jsonData.unrevealed;

    var statsText = new createjs.Text(stats, h1Font, textColor);

    statsText.lineHeight = 20;
    statsText.x = 10;
    statsText.y = 125;    
    
    infoPanelContent.addChild(statsText);

    for(var i = 0; i < jsonData.resources.length; i++) {
        var resource = jsonData.resources[i];
        var resourceImage = resource.name.toLowerCase().replace(/ /g, '');
        var imagePath = "/static/art/" + resourceImage + ".png";

        var icon = new createjs.Container();
        icon.resourceName = resource.name;

        icon.x = 25;
        icon.y = 250 + i * 60;

        infoPanelContent.addChild(icon);
        addImage({id: resourceImage, path: imagePath, x: 0, y: 0, target: icon});

        /*var name = new createjs.Text("Name: " + resource.name, h1Font, textColor);
        var quantity = new createjs.Text("Quantity: " + resource.quantity, h1Font, textColor);
        
        name.x = 85;
        name.y = 40 + i * 60;
        
        quantity.x = 85;
        quantity.y = 60 + i * 60;
        
        addChildSmallDialogPanel(name);
        addChildSmallDialogPanel(quantity);*/
    }
   
};

function drawInfoObj(jsonData) {
    var infoPanel = initInfoPanel(jsonData.id, "obj");
    var infoPanelContent = infoPanel.getChildByName('content');

    infoPanel.obj = jsonData;

    var nameText = new createjs.Text(jsonData.name, h1Font, textColor);
    infoPanelContent.addChild(nameText);

    var nameBounds = nameText.getBounds();
    nameText.x =  Math.floor(infoPanelBg.width / 2) - nameBounds.width / 2;
    nameText.y = 12;

    addSprite({id: jsonData.template, 
        x: Math.floor(infoPanelBg.width / 2) - 45, 
        y: 50, 
        target: infoPanelContent, 
        animation: "none", 
        image: jsonData.image}); 

    if(jsonData.class == "unit") {

        var stats = setStats(jsonData);

        var statsText = new createjs.Text(stats, h1Font, textColor);
        statsText.lineHeight = 20;
        statsText.x = 10;
        statsText.y = 125;
        statsText.name = "stats";

        infoPanelContent.addChild(statsText);

        var effects = "--- Effects ---\n";

        for(var i = 0; i < jsonData.effects.length; i++) {
            var effectName = jsonData.effects[i];

            var text = effectName + "\n";
            effects += text;
        }

        var effectsText = new createjs.Text(effects, h1Font, textColor);
        effectsText.lineHeight = 20;
        effectsText.x = 200;
        effectsText.y = 125;
        effectsText.name = "effects";

        infoPanelContent.addChild(effectsText);

        var skills = "--- Skills ---\n";

        for(var i = 0; i < jsonData.skills.length; i++) {
            var skillName = jsonData.skills[i].name;
            var skillValue = jsonData.skills[i].value;

            var text = skillName + ": " + skillValue + "\n";
            skills += text;
        }

        var skillsText = new createjs.Text(skills, h1Font, textColor);
        skillsText.lineHeight = 20;
        skillsText.x = 200;
        skillsText.y = 150;
    }
    else if(jsonData.class == "structure") {
        var stats = "--- Stats --- \n"
                  + "Hp: " + jsonData.hp + " / " + jsonData.base_hp + "\n"
                  + "State: " + jsonData.state + "\n"
                  + "Capacity: " + jsonData.total_weight + "/" + jsonData.capacity + "\n";

        var statsText = new createjs.Text(stats, h1Font, textColor);

        statsText.lineHeight = 20;
        statsText.x = 10;
        statsText.y = 125;
        
        infoPanelContent.addChild(statsText);

        if(jsonData.state != "none") {
            var req = "--- Requirements ---\n";

            for(var i = 0; i < jsonData.req.length; i++) {
                req += "  " + jsonData.req[i].quantity + " " + jsonData.req[i].type + "\n";
            }

            var reqText = new createjs.Text(req, h1Font, textColor);

            reqText.lineHeight = 20;
            reqText.x = 10;
            reqText.y = 225;

            infoPanelContent.addChild(reqText);
        }
    }

    var itemText = new createjs.Text("--- Items --- ", h1Font, textColor);
    itemText.x = 10;
    itemText.y = 350;
   
    infoPanelContent.addChild(itemText);

    //Draw items
    if(jsonData.hasOwnProperty("items")) {	

        for(var i = 0; i < jsonData.items.length; i++) {
            addItemImageObjPanel(infoPanel, jsonData.items[i], i);
        }
    }

    if(jsonData.class == "structure") {
        if(jsonData.state == "founded" || 
           jsonData.state == "under_construction") {
            var btnBuild = infoPanel.getChildByName("btnBuild");
            btnBuild.visible = true;
            
            console.log("Adding mousedown event handler");   
            btnBuild.on("mousedown", function(evt) {
                sendFinishBuild(evt.target.parent.parent.id);
            });
        }
        else if(jsonData.state == "none") {
            if(jsonData.subclass != "wall") {
                var btnCraft = infoPanel.getChildByName("btnCraft");
                btnCraft.visible = true;
    
                
                var btnAssign = infoPanel.getChildByName("btnAssign");
                btnAssign.visible = true;

                btnAssign.on("mousedown", function(evt) {
                    selectedUnit = jsonData.id;
                    sendAssignList();
                });
       
                btnCraft.on("mousedown", function(evt) {
                    selectedUnit = jsonData.id;
                    sendRecipeList(jsonData.id);                    
                }); 
            }
        }
    } 
};

function setStats(obj) {
    var itemDamage = getItemDamage(obj.items);
    var itemArmor = getItemArmor(obj.items);

    var base_dmg = Number(obj.base_dmg) + itemDamage;
    var dmg_range = Number(obj.dmg_range) + itemDamage;
    var armor = Number(obj.base_def) + itemArmor;
    var stats = ""

    if(obj.subclass == "villager") {
        stats = "--- Stats --- \n"
              + "Hp: " + obj.hp + " / " + obj.base_hp + "\n"
              + "Defense: " + armor + "\n"
              + "Speed: " + obj.base_speed + "\n"
              + "State: " + obj.state + "\n"
              + "Capacity: " + obj.total_weight + "/" + obj.capacity + "\n"
              + "Shelter: " + obj.shelter + "\n"
              + "Morale: " + obj.morale + "\n"
              + "Order: " + obj.order + "\n"
              + "Action: " + obj.action + "\n";
    } else {
        stats = "--- Stats --- \n"
              + "Hp: " + obj.hp + " / " + obj.base_hp + "\n"
              + "Damage: " + base_dmg + " - " + dmg_range + "\n" 
              + "Defense: " + armor + "\n"
              + "Speed: " + obj.base_speed + "\n"
              + "State: " + obj.state + "\n"
              + "Capacity: " + obj.total_weight + "/" + obj.capacity + "\n"
              + "Xp: " + obj.xp + "\n";
    }

    return stats;
}

function addItemImageObjPanel(panel, itemData, imageNum) {
    var panelContent = panel.getChildByName('content');

    var itemName = itemData.name;
    var itemSubclass = itemData.subclass;

    itemName = itemName.toLowerCase().replace(/ /g,'');

    var imagePath = "/static/art/" + itemName + ".png";
    var altImagePath = "/static/art/" + itemSubclass.toLowerCase() + ".png";

    var icon = new createjs.Container();

    icon.x = 10 + imageNum * 50;
    icon.y = 375;
    icon.name = 'item' + itemData.id
    icon.itemId = itemData.id;
    icon.owner = itemData.owner;
    icon.itemName = itemData.name;
    icon.quantity = itemData.quantity;

    var hitArea  = new createjs.Shape();
    hitArea.graphics.beginFill("#000").drawRect(0,0,72,72);
    icon.hitArea = hitArea;

    icon.on("click", function(evt) {
        if(!pressmove) {
            if(evt.nativeEvent.button == 2) {
                console.log("Right Click!");
                drawItemSplit(this.itemId, this.itemName, this.quantity);
            }
            else {
                sendInfoItem(this.itemId);
            }
        }
    });

    icon.on("pressmove", function(evt) {
        if(evt.nativeEvent.button != 2) {
            var cont = this.parent;

            if(!pressmove) {                            
                origX = evt.target.x;
                origY = evt.target.y;
            }

            pressmove = true;                    

            var p = cont.globalToLocal(evt.stageX, evt.stageY);

            evt.target.x = p.x - 25;
            evt.target.y = p.y - 25;
        
            stage.setChildIndex(cont.parent, stage.numChildren - 1);
        }
    });
    icon.on("pressup", function(evt) { 
        pressmove = false;
        
        var transfer = false;

        for(var i = 0; i < infoPanels.length; i++) {
            var pt = infoPanels[i].globalToLocal(evt.stageX, evt.stageY);
            if(infoPanels[i].hitTest(pt.x, pt.y)) {
                if(infoPanels[i].id != this.owner) {
                    if(infoPanels[i].id != undefined) {
                        console.log("Transfering item: " + infoPanels[i].id, this.itemId);
                        transfer = true;
                        sendItemTransfer(infoPanels[i].id, this.itemId);        
                    }
                }
            }
        }

        if(!transfer) {
            evt.target.x = origX;
            evt.target.y = origY;
        }

    });

    panelContent.addChild(icon);

    var path = imagePath;

    //Quick fix to be replaced by lookup table
    if(imageExists(imagePath)) {
        path = imagePath;
    } 
    else {
        path = altImagePath;    
    }

    addImage({id: itemName, path: path, x: 0, y: 0, target: icon});
};

function updateAttrObjPanel(objId, attr, value) {
    var objInfoPanel = getInfoPanel(objId, "obj");

    if(objInfoPanel != null) {
        var objInfoPanelContent = objInfoPanel.getChildByName('content');        

        if(attr == "state") {
            objInfoPanel.obj.state = value;            
            var stats = setStats(objInfoPanel.obj);

            var statsText = objInfoPanelContent.getChildByName('stats');
            statsText.text = stats;
        }
    }
};

function updateVillagerObjPanel(jsonData) {
    var objInfoPanel = getInfoPanel(jsonData.id, "obj");

    if(objInfoPanel != null) {
        var objInfoPanelContent = objInfoPanel.getChildByName('content');        

        objInfoPanel.obj.order = jsonData.order;
        objInfoPanel.obj.action = jsonData.action;
        objInfoPanel.obj.morale = jsonData.morale;
        objInfoPanel.obj.shelter = jsonData.shelter;
        objInfoPanel.obj.structure = jsonData.structure;

        var stats = setStats(objInfoPanel.obj);

        var statsText = objInfoPanelContent.getChildByName('stats');
        statsText.text = stats;
    }

};

function updateEffectsObjPanel(jsonData) {
    var objInfoPanel = getInfoPanel(jsonData.id, "obj");

    if(objInfoPanel != null) {            
        var objInfoPanelContent = objInfoPanel.getChildByName('content');
        
        if(objInfoPanel.obj.effects != null) {
            var effects = objInfoPanel.obj.effects;

            if(jsonData.op == "add") {
                effects.push(jsonData.effect);
            } else {
                var index = effects.indexOf(jsonData.effect);
                effects.splice(index, 1);
            }

            var effectStrList = "--- Effects ---\n";

            for(var i = 0; i < effects.length; i++) {
                var effectName = effects[i];

                effectStrList += effectName;
                effectStrList += "\n";
            }

            var effectsText = objInfoPanelContent.getChildByName('effects');
            effectsText.text = effectStrList;
        }
    }
};

function updateItemObjPanel(jsonData) {
    var objInfoPanel = getInfoPanel(jsonData.id, "obj");

    if(objInfoPanel == null)
        return;

    var objInfoPanelContent = objInfoPanel.getChildByName('content');

    // Check if item was merged
    if(!jsonData.merged) {
        var numItems = 0;

        if(objInfoPanel.obj.items != null) {
            numItems = objInfoPanel.obj.items.length;
        }

        addItemImageObjPanel(objInfoPanel, jsonData.item, numItems);    
        objInfoPanel.obj.items.push(jsonData.item);

    } else {
        //Find item which was updated, either deleted or quantity updated

        for(var i = 0; i < objInfoPanel.obj.items.length; i++) {
            if(objInfoPanel.obj.items[i] == jsonData.id) {
                if(jsonData.quantity > 0) {
                    objInfoPanel.obj.items[i] = jsonData.item;
                } else {
                    var itemIcon = objInfoPanelContent.getChildByName('item' + jsonData.item.id);
                    objInfoPanel.removeChild(itemIcon);
                    objInfoPanel.obj.items.splice(i, 1);
                }

                break;
            }
        }
    }
};

function transferItemObjPanel(jsonData) {
    var sourceObjPanel = getInfoPanel(jsonData.sourceid, "obj");
    var sourceObjPanelContent = sourceObjPanel.getChildByName('content');
    var destObjPanel = getInfoPanel(jsonData.destid, "obj");

    //Remove item from source obj panel items
    for(var i = 0; i < sourceObjPanel.obj.items.length; i++) {
        if(sourceObjPanel.obj.items[i].id == jsonData.sourceitemid) {
            
            var itemIcon = sourceObjPanelContent.getChildByName('item' + jsonData.sourceitemid);
            sourceObjPanelContent.removeChild(itemIcon);
            sourceObjPanel.obj.items.splice(i, 1);                    
            break;
        }
    }

    //Check if the item was not merged on the dest obj panel
    if(!jsonData.merged) {
        var numItems = 0;

        if(destObjPanel.obj.items != null) {
            numItems = destObjPanel.obj.items.length;        
        }

        addItemImageObjPanel(destObjPanel, jsonData.item, numItems);    
        destObjPanel.obj.items.push(jsonData.item);
    }
}

function drawInfoItem(jsonData) {
    var infoPanel = initInfoPanel(jsonData.id, "item");
    var infoPanelContent = infoPanel.getChildByName('content');

    var itemName = jsonData.name;
    var itemClass = jsonData.class;
    var itemSubclass = jsonData.subclass;

    var nameText = new createjs.Text(itemName, h1Font, textColor);
    nameText.x = Math.floor(infoPanelBg.width / 2);
    nameText.y = 10;
    nameText.textAlign = "center";

    infoPanelContent.addChild(nameText);

    itemName = itemName.toLowerCase().replace(/ /g, '');
    var imagePath =  "/static/art/" + itemName + ".png";

    imagesQueue.push({id: itemName, 
                      x: Math.floor(infoPanelBg.width / 2) - 24, 
                      y: 50, target: infoPanelContent});
    loaderQueue.loadFile({id: itemName, src: imagePath});

    var stats = "";

    for(attr in jsonData) {
        if(attr == "effects") {
            var effects = jsonData[attr];

            for(var i = 0; i < effects.length; i++) {
                var effect = effects[i];
                var effectType = effect["type"];
                var effectValue = effect["value"] * 100;

                var stat = effectType + ": " + effectValue + "\n";
                stats += stat;
            }
        } else if(attr != "id" && attr != "owner" && attr != "packet") {
            var stat = attr + ": " + jsonData[attr] + "\n";
            stats += stat;
        }
    }

    var statsText = new createjs.Text(stats, h1Font, textColor);

    statsText.lineHeight = 20;
    statsText.x = 10;
    statsText.y = 125;
    
    infoPanelContent.addChild(statsText);

    if(itemClass == "Weapon") {
        var statsHeight = statsText.getMeasuredHeight(); 
        var btnEquip = infoPanel.getChildByName("btnEquip");

        btnEquip.visible = true;
        btnEquip.x = 333 / 2 - 133 / 2;
        btnEquip.y = 125 + statsHeight; 

        btnEquip.on("mousedown", function(evt) {
            sendEquip(jsonData.id);
        });
    }
};

function drawItemSplit(itemId, itemName, quantity) {
    showSmallDialogPanel();

    var quantityLeft = quantity;
    var quantityRight = 0;

    var title = new createjs.Text("Split Item", h1Font, textColor);
    title.x = Math.floor(smallDialogPanelBg.width / 2);
    title.y = 5;
    title.textAlign = "center";

    addChildSmallDialogPanel(title);

    imageName = itemName.toLowerCase().replace(/ /g,'');

    var imagePath = "/static/art/" + imageName + ".png";
    var iconLeft = new createjs.Container();
    var iconRight = new createjs.Container();
    var btnLeft = new createjs.Bitmap(leftImage);
    var btnRight = new createjs.Bitmap(rightImage);
    var textLeft = new createjs.Text(quantityLeft, h1Font, textColor);
    var textRight = new createjs.Text(quantityRight, h1Font, textColor);
    var btnSplit = new createjs.Bitmap(btnSplitRestImg);

    iconLeft.x = 140;
    iconLeft.y = 100;

    iconRight.x = 240;
    iconRight.y = 100;

    btnLeft.x = 175;
    btnLeft.y = 150;

    btnLeft.on("mousedown", function(evt) {
        if(quantityRight > 1) {
            quantityLeft += 1;
            quantityRight -= 1;

            textLeft.text = quantityLeft;
            textRight.text = quantityRight;
        }
    });

    btnRight.x = 220;
    btnRight.y = 150;

    btnRight.on("mousedown", function(evt) {
        if(quantityLeft > 1) {
            quantityLeft -= 1;
            quantityRight += 1;

            textLeft.text = quantityLeft;
            textRight.text = quantityRight;
        }        
    });

    textLeft.x = 155;
    textLeft.y = 150;

    textRight.x = 250;
    textRight.y = 150;

    btnSplit.x = Math.floor(smallDialogPanelBg.width / 2) - 133 / 2; 
    btnSplit.y = 225;

    btnSplit.on("mousedown", function(evt) {
        sendItemSplit(itemId, quantityRight);
        smallDialogPanel.visible = false;
    });

    addChildSmallDialogPanel(iconLeft);
    addChildSmallDialogPanel(iconRight);
    addChildSmallDialogPanel(btnLeft);
    addChildSmallDialogPanel(btnRight);
    addChildSmallDialogPanel(textLeft);
    addChildSmallDialogPanel(textRight);
    addChildSmallDialogPanel(btnSplit);

    addImage({id: imageName, path: imagePath, x: 0, y: 0, target: iconLeft});
    addImage({id: imageName, path: imagePath, x: 0, y: 0, target: iconRight});
};

function drawReventPanel(jsonData, reventState) {
    reventPanel.visible = true;

    var content = reventPanel.getChildByName("content");
    content.removeAllChildren();

    var okButton = reventPanel.getChildByName("okButton");
    okButton.visible = false;

    var header = new createjs.Text(jsonData.title, "18px Book Antiqua Bold", textColor);
    
    header.x = Math.floor(464 / 2);
    header.y = 80;
    header.textAlign = "center";

    content.addChild(header);

    var text = new createjs.Text(jsonData.text, "12px Arial Regular", textColor);
    
    text.x = Math.floor(464 / 2);
    text.y = 120;
    text.textAlign = "center";
    text.lineWidth = 464 - 10;

    content.addChild(text);

    if(reventState == "responses") {
        for(var i = 0; i < jsonData.responses.length; i++) {
            var button = new reventButton(jsonData.responses[i]);

            button.x = 27;
            button.y = 255 + i * 30;
            button.responseNum = i + 1;

            button.on("click", function(evt) {
                console.log("Send Revent clicked");
                sendReventResponse(this.responseNum);
                reventPanel.visible = false;
            });

            content.addChild(button);
        }
    } else if(reventState == "resolution") {
        okButton.visible = true;

        for(var i = 0; i < jsonData.effects.length; i++) {
            var text = new createjs.Text(jsonData.effects[i], "12px Arial Regular", textColor);

            text.x = 25;
            text.y = 255 + i * 20;

            content.addChild(text);
        }
    }

};

function drawActionBar(objId) {
    var obj = getLocalObj(objId);

    detailsButton.visible = true;

    gatherButton.visible = false;
    buildButton.visible = false;
    moveButton.visible = false;
    hideButton.visible = false;
    followButton.visible = false;
    exploreButton.visible = false;

    if(obj.subclass == "hero") {
        gatherButton.visible = true;
        buildButton.visible = true;
        moveButton.visible = true;
        hideButton.visible = true;

    } else if(obj.subclass == "villager") {
        gatherButton.visible = true;
        followButton.visible = true;
        exploreButton.visible = true;
    }
};

function updateTextLog(newText) {    
    console.log(newText);
    var metrics = textLog.getMetrics();
    var num_lines = metrics.lines.length;

    while(num_lines > 12) {
        textLogLines.shift();
        num_lines--;
    }

    textLogLines.push(newText);

    var lines = "";

    for(var i = 0; i < textLogLines.length; i++) {
        lines += (textLogLines[i] + "\n");    
    }

    textLog.text = lines;
};

function drawProgressBar(jsonData) {
    var bar = new tine.ProgressBar('green', 'black', null, 100, 15);
    bar.value = 0;
    bar.x = 200;
    bar.y = 700;

    stage.addChild(bar); 

    updateBar(bar);

    function updateBar(bar) {
        createjs.Tween.get(bar)
            .to({value: 100}, (jsonData.build_time / 5) * 1000);
    }
};

function initUI() {
    localPanel = new createjs.Container();
    localPanel.visible = false;
    localPanel.x = 0;
    localPanel.y = 0;

    var bg = new createjs.Shape();
    var close = new createjs.Bitmap(close_rest);
    var localMapCont = new createjs.Container();
    var baseCont = new createjs.Container();
    var localObjsCont1 = new createjs.Container();
    var localObjsCont2 = new createjs.Container();
    var localShroudCont = new createjs.Container();
    var transCont = new createjs.Container();
    var extraCont = new createjs.Container();
    var voidCont = new createjs.Container();
    var textLayer = new createjs.Container();

    var closeHitArea  = new createjs.Shape();
    closeHitArea.graphics.beginFill("#000").drawRect(-17,-17,34,34);

    transCont.mouseEnabled = false;
    extraCont.mouseEnabled = false;
    voidCont.mouseEnabled = false;

    selectHex = new createjs.Bitmap(selectHexImage);

    localMapCont.width = stageWidth;
    localMapCont.height = stageHeight;

    bg.graphics.beginFill("#000000").drawRect(0,0,stageWidth, stageHeight);

    localMapCont.name = "localMap";
    baseCont.name = "base";
    transCont.name = "trans";
    extraCont.name = "extra";
    localShroudCont.name = "localShroud";
    localObjsCont1.name = "localObjs1";
    localObjsCont2.name = "localObjs2";
    voidCont.name = "void";
    textLayer.name = "textLayer";

    selectHex.name = "selectHex";
    selectHex.visible = false;

    localPanel.addChild(bg);
    localPanel.addChild(localMapCont);
    
    localMapCont.addChild(baseCont);
    localMapCont.addChild(transCont);
    localMapCont.addChild(extraCont);
    localMapCont.addChild(localShroudCont);
    localMapCont.addChild(localObjsCont1);
    localMapCont.addChild(localObjsCont2);
    localMapCont.addChild(voidCont);
    localMapCont.addChild(selectHex);
    localMapCont.addChild(textLayer);

    stage.addChild(localPanel);

    //Initialize actionBar
    var actionBar = new createjs.Container();
    var actionBarBg = new createjs.Bitmap(actionBarBgImage);

    actionBar.x = stageWidth / 2 - 492 / 2;
    actionBar.y = stageHeight - 231;

    detailsButton.x = 48;
    detailsButton.y = 96;
    detailsButton.mouseChildren = false;
    detailsButton.addChild(new createjs.Bitmap(detailsRest));
    
    gatherButton.x = 101;
    gatherButton.y = 96;
    gatherButton.mouseChildren = false;
    gatherButton.addChild(new createjs.Bitmap(gatherRest));
 
    buildButton.x = 155;
    buildButton.y = 96;
    buildButton.mouseChildren = false;
    buildButton.addChild(new createjs.Bitmap(buildRest));

    moveButton.x = 48;
    moveButton.y = 150;
    moveButton.mouseChildren = false;
    moveButton.addChild(new createjs.Bitmap(moveRest));

    hideButton.x = 101;
    hideButton.y = 150;
    hideButton.mouseChildren = false;
    hideButton.addChild(new createjs.Bitmap(hideRest));

    followButton.x = 101;
    followButton.y = 150;
    followButton.mouseChildren = false;
    followButton.addChild(new createjs.Bitmap(followRest));

    exploreButton.x = 48;
    exploreButton.y = 150;
    exploreButton.mouseChildren = false;
    exploreButton.addChild(new createjs.Bitmap(exploreRest));

    quickCooldown = new createjs.Shape();
    preciseCooldown = new createjs.Shape();
    fierceCooldown = new createjs.Shape();

    dodgeCooldown = new createjs.Shape();
    parryCooldown = new createjs.Shape();
    braceCooldown = new createjs.Shape();

    quickCooldown.y = 50;
    preciseCooldown.y = 50;
    fierceCooldown.y = 50;
    
    dodgeCooldown.y = 50;
    parryCooldown.y = 50;
    braceCooldown.y = 50;

    quickCooldown.regY = 50;
    preciseCooldown.regY = 50;
    fierceCooldown.regY = 50;

    dodgeCooldown.regY = 50;
    parryCooldown.regY = 50;
    braceCooldown.regY = 50;

    quickCooldown.visible = false;
    preciseCooldown.visible = false;
    fierceCooldown.visible = false;

    dodgeCooldown.visible = false;
    parryCooldown.visible = false;
    braceCooldown.visible = false;

    quickButton.x = 284;
    quickButton.y = 94;
    quickButton.mouseChildren = false;
    quickButton.addChild(new createjs.Bitmap(quick));
    quickButton.addChild(quickCooldown);

    preciseButton.x = 340;
    preciseButton.y = 94;
    preciseButton.mouseChildren = false;
    preciseButton.addChild(new createjs.Bitmap(precise));
    preciseButton.addChild(preciseCooldown);

    fierceButton.x = 396;
    fierceButton.y = 94;
    fierceButton.mouseChildren = false;
    fierceButton.addChild(new createjs.Bitmap(fierce));
    fierceButton.addChild(fierceCooldown);

    dodgeButton.x = 284;
    dodgeButton.y = 148;
    dodgeButton.mouseChildren = false;
    dodgeButton.addChild(new createjs.Bitmap(dodge));
    dodgeButton.addChild(dodgeCooldown);

    parryButton.x = 340; 
    parryButton.y = 148;
    parryButton.mouseChildren = false;
    parryButton.addChild(new createjs.Bitmap(parry));
    parryButton.addChild(parryCooldown);

    braceButton.x = 396; 
    braceButton.y = 148;
    braceButton.mouseChildren = false;
    braceButton.addChild(new createjs.Bitmap(brace));
    braceButton.addChild(braceCooldown);

    twoComboButton.x = 284;
    twoComboButton.y = 40;
    twoComboButton.mouseChildren = false;
    twoComboButton.addChild(new createjs.Bitmap(quick));

    threeComboButton.x = 340;
    threeComboButton.y = 40;
    threeComboButton.mouseChildren = false;
    threeComboButton.addChild(new createjs.Bitmap(precise));

    fourComboButton.x = 396;
    fourComboButton.y = 40;
    fourComboButton.mouseChildren = false;
    fourComboButton.addChild(new createjs.Bitmap(fierce));

    /*detailsButton.on("mouseover", function(evt) {
        this.removeAllChildren();
        this.addChild(new createjs.Bitmap(detailsRoll));
    });*/

    detailsButton.on("mousedown", function(evt) {
        if(selectedUnit != -1) {
            if(!isInfoPanelOpened(selectedUnit, "obj")) {
                sendInfoUnit(selectedUnit);
            }
        } 
        else if(selectedTile != -1) {
            var index = selectedTile['x'] + "_" + selectedTile['y'];

            if(!isInfoPanelOpened(index, "tile")) {
                sendInfoTile(selectedTile['x'], selectedTile['y']);
            }
        }
    
        updateTextLog("detailsButton");
    });

    gatherButton.on("mousedown", function(evt) {
        if(selectedPortrait != false) {
            drawResourceTypeDialog();                                    
        }
    });

    buildButton.on("mousedown", function(evt) {
        sendStructureList();
    });

    moveButton.on("mousedown", function(evt) {
        moveToggled = true;
    });

    hideButton.on("mousedown", function(evt) {
        sendHide(selectedPortrait);
    });

    followButton.on("mousedown", function(evt) {
        sendFollow();
    });

    exploreButton.on("mousedown", function(evt) {
        sendOrderExplore();
    });

    quickButton.on("mousedown", function(evt) {
        if(selectedPortrait != false) {
            sendAttack("quick");
        }
    });

    quickButton.on("rollover", function(evt) {        
        hover = new createjs.Bitmap(attack_hover);
        hover.y = -1;
        this.addChild(hover);
    });

    quickButton.on("rollout", function(evt) {
        this.removeChild(hover);
    });

    preciseButton.on("mousedown", function(evt) {
        if(selectedPortrait != false) {           
            sendAttack("precise");
        }
    });

    preciseButton.on("rollover", function(evt) {        
        hover = new createjs.Bitmap(attack_hover);
        hover.y = -1;
        this.addChild(hover);
    });

    preciseButton.on("rollout", function(evt) {
        this.removeChild(hover);
    });

    fierceButton.on("mousedown", function(evt) {
        if(selectedPortrait != false) {           
            sendAttack("fierce");
        }
    });

    fierceButton.on("rollover", function(evt) {        
        hover = new createjs.Bitmap(attack_hover);
        hover.y = -1;
        this.addChild(hover);
    });

    fierceButton.on("rollout", function(evt) {
        this.removeChild(hover);
    });

    dodgeButton.on("mousedown", function(evt) {
        if(selectedPortrait != false) {           
            sendDefend("dodge");
        }
    });

    parryButton.on("mousedown", function(evt) {
        if(selectedPortrait != false) {           
            sendDefend("parry");
        }
    });

    braceButton.on("mousedown", function(evt) {
        if(selectedPortrait != false) {           
            sendDefend("brace");
        }
    });

    twoComboButton.on("mousedown", function(evt) {
        if(selectedPortrait != false) {           
            sendCombo("quick");
        }
    });

    threeComboButton.on("mousedown", function(evt) {
        if(selectedPortrait != false) {           
            sendCombo("precise");
        }
    });

    fourComboButton.on("mousedown", function(evt) {
        if(selectedPortrait != false) {           
            sendCombo("fierce");
        }
    });

    selectPanel = new createjs.Container();
    var bgPanel = new createjs.Bitmap(selectPanelBg);
    var content = new createjs.Container();

    content.name = "content";

    selectPanel.addChild(bgPanel);
    selectPanel.addChild(content);
    
    selectPanel.x = 0;
    selectPanel.y = 0;
    
    actionBar.addChild(selectPanel);

    actionBar.addChild(actionBarBg);
    actionBar.addChild(detailsButton);
    actionBar.addChild(gatherButton);
    actionBar.addChild(buildButton);
    actionBar.addChild(moveButton);
    //actionBar.addChild(hideButton);
    actionBar.addChild(followButton);
    actionBar.addChild(exploreButton);
    actionBar.addChild(quickButton);
    actionBar.addChild(preciseButton);
    actionBar.addChild(fierceButton);
    actionBar.addChild(dodgeButton);
    actionBar.addChild(parryButton);
    actionBar.addChild(braceButton);
    //actionBar.addChild(twoComboButton);
    //actionBar.addChild(threeComboButton);
    //actionBar.addChild(fourComboButton);

    stage.addChild(actionBar);

    portraitPanel = new createjs.Container();
    var bgPanel = new createjs.Bitmap(portraitBg);
    var content = new createjs.Container();
   
    content.name = "content";

    portraitPanel.addChild(bgPanel);
    portraitPanel.addChild(content);
 
    portraitPanel.x = 15;
    portraitPanel.y = 5;

    stage.addChild(portraitPanel);

    //HpBar and StaminaBar
    hpBar = new createjs.Shape();
    staminaBar = new createjs.Shape();

    stage.addChild(hpBar);
    stage.addChild(staminaBar);

    //Initialize infoPanels
    for(var i = 0; i < 4; i++) {
        var panel = new createjs.Container();
        var bg = new createjs.Bitmap(infoPanelBg);
        var close = new createjs.Bitmap(close_rest);
        var content = new createjs.Container();
        var btnBuild = new createjs.Container();
        var btnCraft = new createjs.Container();
        var btnAssign = new createjs.Container();
        var btnEquip = new createjs.Container();

        var btnBuildRest = new createjs.Bitmap(btnBuildRestImg);
        var btnBuildClicked = new createjs.Bitmap(btnBuildClickedImg);

        var btnCraftRest = new createjs.Bitmap(btnCraftRestImg);
        var btnAssignRest = new createjs.Bitmap(btnAssignRestImg);
        var btnEquipRest = new createjs.Bitmap(btnEquipRestImg);

        panel.visible = false;

        close.x = 290;
        close.y = 20;

        close.scaleX = 2;
        close.scaleY = 2;

        btnBuild.visible = false;
        btnBuild.x = 500 / 2 - 133 / 2;
        btnBuild.y = 240;

        btnCraft.visible = false;
        btnCraft.x = 500 / 2 - 133 / 2;
        btnCraft.y = 240;

        btnAssign.visible = false;
        btnAssign.x = 500 / 2 - 133 / 2;
        btnAssign.y = 165;

        btnEquip.visible = false;
        btnEquip.x = 500 / 2 - 133 / 2;
        btnEquip.y = 165;


        btnBuild.name = "btnBuild";
        btnBuildRest.name = "rest";
        btnBuildClicked.name = "clicked";

        btnCraft.name = "btnCraft";
        btnCraftRest.name = "rest";

        btnAssign.name = "btnAssign";
        btnAssignRest.name = "rest";

        btnEquip.name = "btnEquip";
        btnEquipRest.name = "rest";

        btnBuildRest.visible = true;
        btnBuildClicked.visible = false;

        btnCraftRest.visible = true;        
        btnAssignRest.visible = true;
        btnEquipRest.visible = true;

        btnBuild.addChild(btnBuildRest);

        btnBuild.addChild(btnBuildClicked);

        btnCraft.addChild(btnCraftRest);

        btnAssign.addChild(btnAssignRest);

        btnEquip.addChild(btnEquipRest);

        close.hitArea = closeHitArea;

        close.on("mousedown", function(evt) {
            console.log('Close mousedown')
            this.parent.visible = false;
            this.parent.id = null;

            updateTextLog("closeButton");
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

        panel.id = -1;
        panel.panel_type = "none";

        panel.addChild(bg);
        panel.addChild(close);
        panel.addChild(content);
        panel.addChild(btnBuild);
        panel.addChild(btnCraft);
        panel.addChild(btnAssign);
        panel.addChild(btnEquip);

        stage.addChild(panel);

        infoPanels.push(panel);
    }

    //Initialize dialogPanel
    dialogPanel = new createjs.Container();
    dialogPanel.x = 140;
    dialogPanel.y = stageHeight / 2 - 275; 
    dialogPanel.visible = false;

    var bg = new createjs.Bitmap(dialogPanelBg);
    var close = new createjs.Bitmap(close_rest);
    var content = new createjs.Container();

    close.scaleX = 2;
    close.scaleY = 2;

    close.x = 373;
    close.y = 20;

    close.hitArea = closeHitArea;

    close.on("mousedown", function(evt) {
        this.parent.visible = false;
    });

    content.name = 'content';

    dialogPanel.addChild(bg); 
    dialogPanel.addChild(close);
    dialogPanel.addChild(content);

    stage.addChild(dialogPanel);

    //Initialize smallDialogPanel
    smallDialogPanel = new createjs.Container();
    smallDialogPanel.x = stageWidth / 2 - 200;
    smallDialogPanel.y = stageHeight / 2 - 150; 
    smallDialogPanel.visible = false;

    var bg = new createjs.Bitmap(smallDialogPanelBg);
    var close = new createjs.Bitmap(close_rest);
    var content = new createjs.Container();

    close.scaleX = 2;
    close.scaleY = 2;

    close.x = 365;
    close.y = 20;

    close.hitArea = closeHitArea;

    close.on("mousedown", function(evt) {
        this.parent.visible = false;
    });

    content.name = 'content';

    smallDialogPanel.addChild(bg); 
    smallDialogPanel.addChild(close);
    smallDialogPanel.addChild(content);

    stage.addChild(smallDialogPanel);

    textLog = new createjs.Text("", h1Font, textColor);
    
    textLog.x = 10;
    textLog.y = stageHeight - 200;
    textLog.lineWidth = 310;
    
    //Initialize textLog with 10 blank lines 
    initTextLog();

    stage.addChild(textLog); 

    //Button click box
    clicked = new createjs.Bitmap(attack_clicked);

    reventPanel = new createjs.Container();
    
    reventPanel.x = stageWidth / 2 - 464 / 2;
    reventPanel.y = stageHeight / 2 - 265;
    reventPanel.visible = false;

    var content = new createjs.Container();
    content.name = "content";

    reventPanel.addChild(new createjs.Bitmap(reventBg));
    reventPanel.addChild(content);

    var okButton = new createjs.Container();
    okButton.name = "okButton";

    okButton.addChild(new createjs.Bitmap(reventOkButton));

    okButton.x = 464 / 2 - 103 / 2;
    okButton.y = 335;

    okButton.on('click', function(evt) {
        reventPanel.visible = false;
    });

    reventPanel.addChild(okButton);

    stage.addChild(reventPanel);
};

function showLocalPanel() {
    localPanel.visible = true;
};

function initInfoPanel(id, panel_type) {    
    var xCoords = [0, infoPanelBg.width, infoPanelBg.width * 2];
    var infoPanel;
    var alreadyOpen = false;

    //Find free info panel
    for(var i = 0; i < infoPanels.length; i++) {
        if(infoPanels[i].visible == false) {            
            infoPanel = infoPanels[i]; 
        }
        else {
            if(infoPanel.id == id && infoPanel == panel_type) {
                alreadyOpen = true;
            } else {                
                var index = xCoords.indexOf(infoPanels[i].x);
                xCoords.splice(index, 1);    
            }
        }
    }

    if(infoPanel != null && alreadyOpen == false) {        
        //Set id and type of info panel
        infoPanel.id = id;
        infoPanel.panel_type = panel_type

        var content = infoPanel.getChildByName('content');
        content.removeAllChildren();

        if(xCoords.length > 0) {
            infoPanel.x = xCoords[0];
        } 
        else {
            infoPanel.x = 0;
        }

        infoPanel.visible = true;    
        hideButtons(infoPanel);
    }

    return infoPanel;
};

function showDialogPanel() {
    var content = dialogPanel.getChildByName('content');
    content.removeAllChildren();

    dialogPanel.visible = true;
};

function showSmallDialogPanel() {
    var content = smallDialogPanel.getChildByName('content');
    content.removeAllChildren();

    smallDialogPanel.visible = true;
}

function getInfoPanel(id, type) {
    for(var i = 0; i < infoPanels.length; i++) {
        if(infoPanels[i].id == id && infoPanels[i].panel_type == type) {
            return infoPanels[i];
        }
    }

    return null
}

function isInfoPanelOpened(id, type) {
    for(var i = 0; i < infoPanels.length; i++) {
        if(infoPanels[i].visible == true) {
            if(infoPanels[i].id == id && infoPanels[i].panel_type == type) {
                return true;
            }
        }
    }

    return false;
}

function addChildLocalMap(item, childName) {
    var localMapCont = localPanel.getChildByName('localMap');
    var child = localMapCont.getChildByName(childName);
    child.addChild(item);
};

function removeChildLocalMap(item, childName) {
    var localMapCont = localPanel.getChildByName('localMap');
    var child = localMapCont.getChildByName(childName);
    child.removeChild(item);
};

function addChildDialogPanel(item) {
    var content = dialogPanel.getChildByName('content');
    content.addChild(item);
};

function addChildSmallDialogPanel(item) {
    var content = smallDialogPanel.getChildByName('content');
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

      if(i == 0) 
          odd_q["d"] = "se";
      else if(i == 1)
          odd_q["d"] = "ne";
      else if(i == 2)
          odd_q["d"] = "n";
      else if(i == 3)
          odd_q["d"] = "nw";
      else if(i == 4)
          odd_q["d"] = "sw";
      else if(i == 5)
          odd_q["d"] = "s";

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
    var xy = x + "_" + y;

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

function is_hero(subclass) {
    if(subclass == "hero") {
        return true;
    } 

    return false;
};

function in_array(array, index) {
    if(array.indexOf(index) >= 0) {
        return true;
    } else {
        return false;
    }
};

function imageExists(image_url) {
    var http = new XMLHttpRequest();

    http.open('HEAD', image_url, false);
    http.send();

    var result = http.status != 404;

    return result;
};

function hideButtons(infoPanel) {
    var btnBuild = infoPanel.getChildByName("btnBuild");
    var btnCraft = infoPanel.getChildByName("btnCraft");
    var btnAssign = infoPanel.getChildByName("btnAssign");
    var btnEquip = infoPanel.getChildByName("btnEquip");

    btnBuild.visible = false;
    btnCraft.visible = false;
    btnAssign.visible = false; 
    btnEquip.visible = false; 
};

function getItemDamage(items) {
    var damage = 0;

    for(var i = 0; i < items.length; i++) {
        if(items[i].hasOwnProperty('damage') && 
           items[i].hasOwnProperty('equip')) {
            
            if(items[i].equip == 'true') {
                damage += Number(items[i].damage);
            }
        }    
    }

    return damage;
};

function getItemArmor(items) {
    var armor = 0;

    for(var i = 0; i < items.length; i++) {
        if(items[i].hasOwnProperty('armor') && 
           items[i].hasOwnProperty('equip')) {
            
            if(items[i].equip == 'true') {
                armor += Number(items[i].armor);
            }
        }    
    }
    return armor;
};

function initTextLog() {
    var lines = "";

    for(var i = 0; i < 10; i++) {
        textLogLines.push("");
        lines += "";
    }

    textLog.text = lines;
};

function removeClicked()
{
    if(clicked.parent != null) {
        var button = clicked.parent;
        button.removeChild(clicked);
    }
};

function reventButton(response)
{
    var button = new createjs.Container();
    
    var response = new createjs.Text(response, "12px Arial Regular", "#ECECEC");
    
    response.x = Math.floor(410 / 2);
    response.y = 5;

    response.textAlign = "center";
    response.visible = true;

    button.addChild(new createjs.Bitmap(reventButtonBg));
    button.addChild(response);

    return button;
};

