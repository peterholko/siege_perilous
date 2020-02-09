
import { Network } from './network';
import { Util } from './util';
import { Global } from './global';
import { ObjectState } from './objectState';
import { GameEvent } from './gameEvent';
import { Tile } from './objects/tile';

import * as React from "react";
import styles from "./ui.css";

import SingleInventoryPanel from './ui/singleInventoryPanel';
import ItemPanel from './ui/itemPanel';

import movecompass from "ui/movecompass.png";

import inventorybutton from "ui/inventorybutton.png";
import attrsbutton from "ui/attrsbutton.png";
import buildbutton from "ui/buildbutton.png";
import explorebutton from "ui/explorebutton.png";
import gatherbutton from "ui/gatherbutton.png";
import sleepbutton from "ui/sleepbutton.png";

import bracebutton from "ui/bracebutton.png";
import parrybutton from "ui/parrybutton.png";
import dodgebutton from "ui/dodgebutton.png";

import { NetworkEvent } from './networkEvent';
import { HERO, VILLAGER, STRUCTURE, PROGRESSING, 
  TRIGGER_INVENTORY,
  QUICK,
  PRECISE,
  FIERCE,
  OBJ} from './config';
import TargetActionPanel from './ui/targetActionPanel';
import ItemTransferPanel from './ui/itemTransferPanel';
import HeroPanel from './ui/heroPanel';
import VillagerPanel from './ui/villagerPanel';
import AttrsPanel from './ui/attrsPanel';
import SkillsPanel from './ui/skillsPanel';
import TilePanel from './ui/tilePanel';
import GatherPanel from './ui/gatherPanel';
import BuildPanel from './ui/buildPanel';
import StructurePanel from './ui/structurePanel';
import ErrorPanel from './ui/errorPanel';
import SelectPanel from './ui/selectPanel';
import AssignPanel from './ui/assignPanel';
import CraftPanel from './ui/craftPanel';
import ItemDividePanel from './ui/itemDividePanel';
import MerchantPanel from './ui/merchantPanel';
import MerchantQuantityPanel from './ui/merchantQuantityPanel';
import ResourcePanel from './ui/resourcePanel';
import HeroFrame from './ui/heroFrame';
import MerchantHirePanel from './ui/merchantHirePanel';
import ExperimentPanel from './ui/experimentPanel';
import ActionButton from './ui/actionButton';
import NPCPanel from './ui/npcPanel';
import HeroAdvancePanel from './ui/heroAdvancePanel';

interface UIState {
  selectBoxes : [],
  inventoryPanels : [],
  hideTargetActionPanel: boolean,
  hideGatherPanel : boolean,
  hideInventoryPanel : boolean,
  hideItemTransferPanel : boolean,
  hideItemDividePanel : boolean,
  hideItemPanel : boolean,
  hideHeroPanel : boolean,
  hideVillagerPanel : boolean,
  hideNPCPanel : boolean,
  hideAttrsPanel : boolean,
  hideSkillsPanel : boolean,
  hideAdvancePanel : boolean,
  hideTilePanel : boolean,
  hideBuildPanel : boolean,
  hideStructurePanel : boolean,
  hideErrorPanel : boolean,
  hideAssignPanel : boolean,
  hideCraftPanel : boolean,
  hideMerchantPanel : boolean,
  hideMerchantQuantityPanel : boolean,
  hideMerchantHirePanel : boolean,
  hideResourcePanel : boolean,
  hideExperimentPanel : boolean,
  leftInventoryId : integer,
  leftInventoryData: any,
  rightInventoryId : integer,
  rightInventoryData: any,
  inventoryReqs: [], //Currently used for structure inventory reqs
  itemData : any,
  heroData : any,
  npcData : any,
  villagerData : any,
  assignData : any,
  attrsData : any,
  skillsData : any,
  advanceData : any,
  tileData : any,
  structuresData : any, //Structures list
  structureData : any, //Structure data
  recipesData : any,
  itemDivideData : any,
  itemMerchantQuantityData : any,
  resourceData : any,
  hireData : any,
  expData : any,
  merchantAction : any,
  selectedTile : Tile,
  selectedBoxPos : integer,
  selectedKey: any,
  infoItemAction : string,
  errmsg : string
}

export default class UI extends React.Component<any, UIState>{
  private compassRef = React.createRef<HTMLImageElement>();

  constructor(props) {
    super(props);

    this.state = {
      selectBoxes: [],
      inventoryPanels: [],
      hideTargetActionPanel: true,
      hideGatherPanel: true,
      hideInventoryPanel: true,
      hideItemTransferPanel: true,
      hideItemDividePanel : true,
      hideItemPanel: true,
      hideHeroPanel: true,
      hideVillagerPanel: true,
      hideNPCPanel : true,
      hideAttrsPanel : true,
      hideSkillsPanel : true,
      hideAdvancePanel : true,
      hideTilePanel : true,
      hideBuildPanel : true,
      hideStructurePanel : true,
      hideAssignPanel : true,
      hideCraftPanel : true,
      hideErrorPanel : true,
      hideMerchantPanel : true,
      hideMerchantQuantityPanel : true,
      hideMerchantHirePanel : true,
      hideResourcePanel : true,
      hideExperimentPanel : true,
      leftInventoryId: -1,
      leftInventoryData: [],
      rightInventoryId: -1,
      rightInventoryData: [],
      inventoryReqs : [],
      itemData : {},
      heroData : {},
      npcData : {},
      villagerData : {},
      assignData : {},
      attrsData : {},
      skillsData : {},
      advanceData : {},
      tileData : {},
      structuresData : {},
      structureData : {},
      recipesData : {},
      itemDivideData : {},
      itemMerchantQuantityData : {},
      resourceData : {},
      hireData : {},
      expData: {},
      merchantAction : 'buy',
      selectedTile: null,
      selectedBoxPos: 0,
      selectedKey: {type: '', id: -1},
      infoItemAction: TRIGGER_INVENTORY,
      errmsg : ''
    }

    this.handleMoveClick = this.handleMoveClick.bind(this);
    this.handleTileClick = this.handleTileClick.bind(this);

    this.handleHeroAttrsClick = this.handleHeroAttrsClick.bind(this);
    this.handleHeroInventoryClick = this.handleHeroInventoryClick.bind(this);
    this.handleHeroExploreClick = this.handleHeroExploreClick.bind(this);
    this.handleHeroBuildClick = this.handleHeroBuildClick.bind(this);
    this.handleHeroGatherClick = this.handleHeroGatherClick.bind(this);
    this.handleHeroSleepClick = this.handleHeroSleepClick.bind(this);

    this.handleQuickAttack = this.handleQuickAttack.bind(this);
    this.handlePreciseAttack = this.handlePreciseAttack.bind(this);
    this.handleFierceAttack = this.handleFierceAttack.bind(this);

    this.handleTransitionEnd = this.handleTransitionEnd.bind(this);

    Global.gameEmitter.on(GameEvent.TILE_CLICK, this.handleTileClick, this);
    Global.gameEmitter.on(GameEvent.SELECTBOX_CLICK, this.handleSelectBoxClick, this);
    Global.gameEmitter.on(GameEvent.SELECT_PANEL_CLICK, this.handleSelectPanelClick, this);
    Global.gameEmitter.on(GameEvent.EXIT_HALFPANEL_CLICK, this.handleExitHalfPanelClick, this);
    Global.gameEmitter.on(GameEvent.TAP_CLICK, this.handleTargetActionPanelClick, this);
    Global.gameEmitter.on(GameEvent.VILLAGER_GATHER_CLICK, this.handleVillagerGatherClick, this);
    Global.gameEmitter.on(GameEvent.RESOURCE_GATHER_CLICK, this.handleResourceGatherClick, this);
    Global.gameEmitter.on(GameEvent.START_BUILD_CLICK, this.handleStartBuildClick, this);
    Global.gameEmitter.on(GameEvent.ASSIGN_CLICK, this.handleAssignClick, this);
    Global.gameEmitter.on(GameEvent.ERROR_OK_CLICK, this.handleErrorOkClick, this);
    Global.gameEmitter.on(GameEvent.ITEM_DIVIDE_CLICK, this.handleItemDivideClick, this);
    Global.gameEmitter.on(GameEvent.ITEM_DIVIDE_OK_CLICK, this.handleItemDivideOkClick, this);
    Global.gameEmitter.on(GameEvent.MERCHANT_BUYSELL_CLICK, this.handleMerchantBuySellClick, this);
    Global.gameEmitter.on(GameEvent.MERCHANT_QUANTITY_CANCEL, this.handleMerchantQuantityCancel, this);
    Global.gameEmitter.on(GameEvent.MERCHANT_HIRE_CLICK, this.handleMerchantHireClick, this);
    Global.gameEmitter.on(GameEvent.RESOURCE_CLICK, this.handleResourceClick, this);

    Global.gameEmitter.on(NetworkEvent.SERVER_OFFLINE, this.handleServerOffline, this);
    Global.gameEmitter.on(NetworkEvent.NETWORK_ERROR, this.handleNetworkError, this);

    Global.gameEmitter.on(NetworkEvent.ERROR, this.handleError, this);
    Global.gameEmitter.on(NetworkEvent.HERO_INIT, this.handleHeroInit, this);
    Global.gameEmitter.on(NetworkEvent.HERO_DEAD, this.handleHeroDead, this);
    Global.gameEmitter.on(NetworkEvent.INFO_OBJ, this.handleInfoObj, this);
    Global.gameEmitter.on(NetworkEvent.INFO_TILE, this.handleInfoTile, this);
    Global.gameEmitter.on(NetworkEvent.INFO_ITEM, this.handleInfoItem, this);
    Global.gameEmitter.on(NetworkEvent.INFO_INVENTORY, this.handleInfoInventory, this);
    Global.gameEmitter.on(NetworkEvent.INFO_ITEM_TRANSFER, this.handleInfoItemTransfer, this);
    Global.gameEmitter.on(NetworkEvent.INFO_ITEM_UPDATE, this.handleInfoItemUpdate, this);
    Global.gameEmitter.on(NetworkEvent.INFO_ATTRS, this.handleInfoAttrs, this);
    Global.gameEmitter.on(NetworkEvent.INFO_SKILLS, this.handleInfoSkills, this);
    Global.gameEmitter.on(NetworkEvent.INFO_ADVANCE, this.handleInfoAdvance, this);
    Global.gameEmitter.on(NetworkEvent.INFO_HAULING, this.handleInfoHauling, this);
    Global.gameEmitter.on(NetworkEvent.INFO_EXPERIMENT, this.handleInfoExperiment, this);
    Global.gameEmitter.on(NetworkEvent.ITEM_TRANSFER, this.handleItemTransfer, this);
    Global.gameEmitter.on(NetworkEvent.BUYSELL_ITEM, this.handleBuySellItem, this);
    Global.gameEmitter.on(NetworkEvent.STRUCTURE_LIST, this.handleStructureList, this);
    Global.gameEmitter.on(NetworkEvent.ASSIGN_LIST, this.handleAssignList, this);
    Global.gameEmitter.on(NetworkEvent.RECIPE_LIST, this.handleRecipeList, this);
    Global.gameEmitter.on(NetworkEvent.ATTACK, this.handleAttack, this);
    Global.gameEmitter.on(NetworkEvent.ADVANCE, this.handleAdvance, this);
  }

  handleMoveClick(event : React.MouseEvent) {
    const compass = this.compassRef.current!;

      var pocX = event.nativeEvent.offsetX - compass.naturalWidth / 2;
      var pocY = event.nativeEvent.offsetY - compass.naturalHeight / 2;

      var angleRads = Math.atan2(pocX, pocY);
      var angleDegrees = ((angleRads * 180) / Math.PI) + 180;

      var heroObj = Global.objectStates[Global.heroId] as ObjectState;

      if(angleDegrees < 30 || angleDegrees >= 330) {
        console.log('N');
        var nextPos = Util.nextPosByDirection(heroObj.x, heroObj.y, 'N');
        Network.sendMove(nextPos.q, nextPos.r);
      } else if(angleDegrees < 90 && angleDegrees >= 30) {
        console.log('NW');
        var nextPos = Util.nextPosByDirection(heroObj.x, heroObj.y, 'NW');
        Network.sendMove(nextPos.q, nextPos.r);
      } else if(angleDegrees < 150 && angleDegrees >= 90) {
        console.log('SW');
        var nextPos = Util.nextPosByDirection(heroObj.x, heroObj.y, 'SW');
        Network.sendMove(nextPos.q, nextPos.r);
      } else if(angleDegrees < 210 && angleDegrees >= 150) {
        console.log('S');
        var nextPos = Util.nextPosByDirection(heroObj.x, heroObj.y, 'S');
        Network.sendMove(nextPos.q, nextPos.r);
      } else if(angleDegrees < 270 && angleDegrees >= 210) {
        console.log('SE');
        var nextPos = Util.nextPosByDirection(heroObj.x, heroObj.y, 'SE');
        Network.sendMove(nextPos.q, nextPos.r);
      } else if(angleDegrees < 330 && angleDegrees >= 270) {
        console.log('NE');
        var nextPos = Util.nextPosByDirection(heroObj.x, heroObj.y, 'NE');
        Network.sendMove(nextPos.q, nextPos.r);
      }
  }

  handleTileClick(gameObject) {
    console.log("gameObject: " + gameObject);
    this.setState({selectedTile: gameObject,
                   hideTargetActionPanel: true});
  }

  handleSelectBoxClick(eventData) {
    console.log('SelectBoxClick');

    this.setState({hideTargetActionPanel: false,
                   selectedBoxPos: eventData.pos,
                   selectedKey: eventData.selectedKey})
  }

  handleSelectPanelClick() {
    this.setState({hideTargetActionPanel: true});
  }

  handleExitHalfPanelClick(event) {
    console.log('ExitHalfPanel');

    if(event.panelType == 'inventory') {
      this.setState({hideInventoryPanel: true});
    } else if(event.panelType == 'itemTransfer') {
      this.setState({hideItemTransferPanel: true})
    } else if(event.panelType == 'merchant') {
      this.setState({hideMerchantPanel: true})
    } else if(event.panelType == 'item') {
      this.setState({hideItemPanel: true});
    } else if(event.panelType == 'hero') {
      this.setState({hideHeroPanel: true});
    } else if(event.panelType == 'villager') {
      this.setState({hideVillagerPanel: true});
    } else if(event.panelType == 'npc') {
      this.setState({hideNPCPanel: true});
    } else if(event.panelType == 'attrs') {
      this.setState({hideAttrsPanel: true});
    } else if(event.panelType == 'skills') {
      this.setState({hideSkillsPanel: true});
    } else if(event.panelType == 'advance') {
      this.setState({hideAdvancePanel: true});
    } else if(event.panelType == 'tile') {
      this.setState({hideTilePanel: true});
    } else if(event.panelType == 'build') {
      this.setState({hideBuildPanel: true});
    } else if(event.panelType == 'structure') {
      this.setState({hideStructurePanel: true,
                     hideAssignPanel: true});
    } else if(event.panelType == 'assign') {
      this.setState({hideAssignPanel: true});
    } else if(event.panelType == 'craft') {
      this.setState({hideCraftPanel: true});
    } else if(event.panelType == 'resource') {
      this.setState({hideResourcePanel: true});
    } else if(event.panelType == 'hire') {
      this.setState({hideMerchantHirePanel: true});
    } else if(event.panelType == 'experiment') {
      Network.sendInfoExit(this.state.expData.id, "experiment");
      this.setState({hideExperimentPanel: true});
    } 
  }

  handleErrorOkClick() {
    if(Global.heroDead || Global.networkError || Global.serverOffline) {
      location.reload();
    }

    this.setState({hideErrorPanel: true});
  }

  handleAssignClick() {
    this.setState({hideAssignPanel: true});
  }
  
  handleItemDivideClick(itemData) {
    this.setState({hideItemDividePanel: false, 
                   itemDivideData: itemData});
  }

  handleItemDivideOkClick() {
    this.setState({hideItemDividePanel: true});
  }

  handleMerchantBuySellClick(eventData) {
    this.setState({hideMerchantQuantityPanel: false,
                   itemMerchantQuantityData: eventData.itemData,
                   merchantAction: eventData.action});
  }

  handleMerchantHireClick() {
    this.setState({hideMerchantHirePanel: true,
                   hideMerchantPanel: true});
  }

  handleMerchantQuantityCancel() {
    this.setState({hideMerchantQuantityPanel: true});
  }

  handleTargetActionPanelClick(event : React.MouseEvent) {
    this.setState({hideTargetActionPanel: true});
  }

  handleVillagerGatherClick(event : React.MouseEvent) {
    this.setState({hideGatherPanel: false});
  }

  handleResourceGatherClick(event : React.MouseEvent) {
    this.setState({hideGatherPanel: true});
  }

  handleStartBuildClick(event : React.MouseEvent) {
    this.setState({hideBuildPanel: true});
  }

  handleHeroAttrsClick(event: React.MouseEvent) {
    Network.sendInfoObj(Global.heroId);
  }

  handleHeroInventoryClick(event: React.MouseEvent) {
    Network.sendInfoInventory(Global.heroId);
  }

  handleHeroExploreClick(event: React.MouseEvent) {
    Network.sendExpore(Global.heroId);
  }
 
  handleHeroBuildClick(event: React.MouseEvent) {
    Network.sendGetStructureList()
  }

  handleHeroGatherClick(event: React.MouseEvent) {
    this.setState({selectedKey: {type: OBJ, id: Global.heroId},
                   hideGatherPanel: false});
  }

  handleHeroSleepClick(event: React.MouseEvent) {
    Network.sendRest(Global.heroId);
  }

  handleQuickAttack(event: React.MouseEvent) {
    Network.sendAttack('quick', Global.heroId, this.state.selectedKey.id);
  }

  handlePreciseAttack(event: React.MouseEvent) {
    Network.sendAttack('precise', Global.heroId, this.state.selectedKey.id);
  }

  handleFierceAttack(event: React.MouseEvent) {
    Network.sendAttack('fierce', Global.heroId, this.state.selectedKey.id);
  }
  
  handleAttack(message) {

  }

  handleAdvance(message) {
    console.log('handleAdvance');
    this.setState({advanceData: message})
  }

  handleError(message) {
    this.setState({hideErrorPanel : false,
                   errmsg : message.errmsg});
  }
  handleServerOffline() {
    Global.serverOffline = true;

    this.setState({hideErrorPanel : false,
                   errmsg : "The server is offline..."})
  }

  handleNetworkError() {
    Global.networkError = true;

    this.setState({hideErrorPanel : false,
                   errmsg : "The network connection encountered an error, click to reconnect."})
  }

  handleHeroInit() {
    this.setState({selectedKey: {type: OBJ, id: Global.heroId}});
  }

  handleHeroDead() {
    Global.heroDead = true;

    this.setState({hideErrorPanel : false,
                   errmsg : "Your hero has perished... "})
  }

  handleResourceClick(eventData) {
    this.setState({hideResourcePanel: false,
                   resourceData: eventData});
  }

  handleInfoObj(message) {
    console.log('UI handleInfoObj');
    if(Util.isPlayerObj(message.id)) {
      if(Util.isSubclass(message.id, HERO)) {
        this.setState({hideHeroPanel: false, heroData: message});  
      } else if(Util.isSubclass(message.id, VILLAGER)) {
        this.setState({hideVillagerPanel: false, villagerData: message});
      } else if(Util.isClass(message.id, STRUCTURE)) {
        this.setState({hideStructurePanel: false, structureData: message});
      } 
    } else {
        this.setState({hideNPCPanel: false, npcData: message});  
    }
  }

  handleInfoTile(message) {
    console.log('UI handleInfoTile');
    this.setState({hideTilePanel: false, tileData: message});
  }

  handleInfoItem(message) {
    console.log('UI handleInfoItem');
    this.setState({hideItemPanel: false, 
                   itemData: message,
                   infoItemAction: Global.infoItemAction});
  }

  handleInfoInventory(message) {
    console.log('UI handleInfoInventory');
    this.setState({hideInventoryPanel: false, leftInventoryData: message});
  }

  handleInfoItemTransfer(message) {
    console.log('UI handleInfoItemTransfer');
    console.log(message);
    console.log('leftInventoryData.id: ' + message.sourceitems.id);
    console.log('rightInventoryData.id: ' + message.targetitems.id);

    if(Global.infoItemTransferAction == 'transfer') {
      this.setState({hideItemTransferPanel: false,
                    leftInventoryId: message.sourceid, 
                    leftInventoryData: message.sourceitems,
                    rightInventoryId: message.targetid,
                    rightInventoryData: message.targetitems,
                    inventoryReqs: message.reqitems});
    } else if(Global.infoItemTransferAction == 'merchant') {
      this.setState({hideMerchantPanel: false,
                    leftInventoryId: message.sourceid, 
                    leftInventoryData: message.sourceitems,
                    rightInventoryId: message.targetid,
                    rightInventoryData: message.targetitems});
    }
  }

  handleInfoItemUpdate(message) {
    console.log('UI handleInfoItemUpdate');
    this.setState({itemData: message.item});

    //TODO improve this one day
    if(message.id == this.state.leftInventoryData.id) {
      var newLeftInventoryData : any = {...this.state.leftInventoryData};

      for(var i = 0; i < newLeftInventoryData.items.length; i++) {
        if(newLeftInventoryData.items[i].id == message.item.id) {
          newLeftInventoryData.items[i] = message.item;
        }
      }

      this.setState({leftInventoryData: newLeftInventoryData});
    }

    if(message.id == this.state.rightInventoryData.id) {
      var newRightInventoryData : any = {...this.state.rightInventoryData};

      for(var i = 0; i < newRightInventoryData.items.length; i++) {
        if(newRightInventoryData.items[i].id == message.item.id) {
          newRightInventoryData.items[i] = message.item;
        }
      }

      this.setState({rightInventoryData: newRightInventoryData});
    }
  }

  handleInfoAttrs(message) {
    console.log('UI handleInfoAttrs');
    this.setState({hideAttrsPanel: false, attrsData: message});
  }

  handleInfoSkills(message) {
    console.log('UI handleInfoSkills');
    this.setState({hideSkillsPanel: false, skillsData: message});
  }

  handleInfoAdvance(message) {
    console.log('UI handleInfoAdvance');
    this.setState({hideAdvancePanel: false, advanceData: message});
  }

  handleInfoHauling(message) {
    console.log('UI handleInfoHauling');
    if(message.data.length > 0) {
      this.setState({hideMerchantHirePanel: false, hireData : message.data})
    } else {
      this.setState({hideErrorPanel : false,
                     errmsg : "No hires availables"});
    }
  }

  handleInfoExperiment(message) {
    console.log("UI handleInfoExperiment");
    this.setState({hideExperimentPanel: false, expData: message});
  }

  handleItemTransfer(message) {
    console.log('UI handleItemTransfer leftId: ' + this.state.leftInventoryId + ' rightId: ' + 
    this.state.rightInventoryId + ' sourceId: ' + message.sourceid + ' targetId: ' + message.targetid );

    var leftInventoryData; 
    var rightInventoryData;

    if(this.state.leftInventoryId == message.sourceid) {
      leftInventoryData = message.sourceitems;
    } else if(this.state.leftInventoryId == message.targetid) {
      leftInventoryData = message.targetitems;
    }

    if(this.state.rightInventoryId == message.sourceid) {
      rightInventoryData = message.sourceitems;
    } else if(this.state.rightInventoryId == message.targetid) {
      rightInventoryData = message.targetitems;
    }

    this.setState({hideItemTransferPanel: false,
                   leftInventoryData: leftInventoryData,
                   rightInventoryData: rightInventoryData,
                   inventoryReqs: message.reqitems})
  }

  handleBuySellItem(message) {
    var leftInventoryData; 
    var rightInventoryData;

    if(this.state.leftInventoryId == message.sourceid) {
      leftInventoryData = message.sourceitems;
    } else if(this.state.leftInventoryId == message.targetid) {
      leftInventoryData = message.targetitems;
    }

    if(this.state.rightInventoryId == message.sourceid) {
      rightInventoryData = message.sourceitems;
    } else if(this.state.rightInventoryId == message.targetid) {
      rightInventoryData = message.targetitems;
    }

    this.setState({hideMerchantPanel: false,
                   hideItemPanel: true,
                   leftInventoryData: leftInventoryData,
                   rightInventoryData: rightInventoryData})
    
  }

  handleStructureList(message) {
    //TODO look to fix the structures list packet
    this.setState({hideBuildPanel: false, structuresData: message.result});
  }

  handleAssignList(message) {
    this.setState({hideAssignPanel: false, assignData: message.result});
  }

  handleRecipeList(message) {
    this.setState({hideCraftPanel: false, recipesData: message.result});
  }

  handleBuild(message) {
    let newData = {...this.state.structureData};
    newData.state = PROGRESSING;
    this.setState({structureData: newData})
  }

  handleTransitionEnd() {
    console.log("TransitionEnd");
  }

  render() {
    return(
      <div id="ui" className={styles.ui}>
         <img src={attrsbutton} 
              id="heroattrsbutton" 
              className={styles.heroattrsbutton} 
              onClick={this.handleHeroAttrsClick} />

          <img src={inventorybutton} 
              id="heroinventorybutton" 
              className={styles.heroinventorybutton} 
              onClick={this.handleHeroInventoryClick} />

          <img src={explorebutton} 
              id="heroexplorebutton" 
              className={styles.heroexplorebutton} 
              onClick={this.handleHeroExploreClick} />

          <img src={buildbutton} 
              id="herobuildbutton" 
              className={styles.herobuildbutton} 
              onClick={this.handleHeroBuildClick} />

          <img src={gatherbutton} 
              id="herogatherbutton" 
              className={styles.herogatherbutton} 
              onClick={this.handleHeroGatherClick} />
        
          <img src={sleepbutton} 
              id="herosleepbutton" 
              className={styles.herosleepbutton} 
              onClick={this.handleHeroSleepClick} />

          <ActionButton type={QUICK}
                        handler={this.handleQuickAttack}/>

          <ActionButton type={PRECISE}
                        handler={this.handlePreciseAttack}/>

          <ActionButton type={FIERCE}
                        handler={this.handleFierceAttack}/>

          <img src={bracebutton} 
              id="bracebutton" 
              className={styles.bracebutton} />

          <img src={parrybutton} 
              id="parrybutton" 
              className={styles.parrybutton} />
          
          <img src={dodgebutton} 
              id="dodgebutton" 
              className={styles.dodgebutton} />

          <img src={movecompass} 
               id="movecompass" 
               ref={this.compassRef}
               className={styles.movecompass} 
               onClick={this.handleMoveClick}/>
          
          <HeroFrame></HeroFrame>

          <SelectPanel selectedTile={this.state.selectedTile} />

          {!this.state.hideTargetActionPanel && 
            <TargetActionPanel selectedBoxPos={this.state.selectedBoxPos}
                               selectedKey={this.state.selectedKey}/> }

          {!this.state.hideInventoryPanel && 
            <SingleInventoryPanel left={true} 
                            inventoryData={this.state.leftInventoryData}
                            hideExitButton={false} /> }

          {!this.state.hideItemTransferPanel && 
            <ItemTransferPanel leftInventoryData={this.state.leftInventoryData} 
                               rightInventoryData={this.state.rightInventoryData}
                               reqs={this.state.inventoryReqs} /> }

          {!this.state.hideMerchantPanel && 
            <MerchantPanel leftInventoryData={this.state.leftInventoryData} 
                           rightInventoryData={this.state.rightInventoryData} /> }
                              
          {!this.state.hideItemPanel &&
            <ItemPanel itemData={this.state.itemData}
                       triggerAction={this.state.infoItemAction}/> }

          {!this.state.hideHeroPanel &&
            <HeroPanel heroData={this.state.heroData}/> }

          {!this.state.hideVillagerPanel &&
            <VillagerPanel villagerData={this.state.villagerData}/> }
          
          {!this.state.hideNPCPanel &&
            <NPCPanel npcData={this.state.npcData}/> }

          {!this.state.hideAttrsPanel &&
            <AttrsPanel attrsData={this.state.attrsData}/> }

          {!this.state.hideSkillsPanel &&
            <SkillsPanel skillsData={this.state.skillsData}/> }
          
          {!this.state.hideAdvancePanel &&
            <HeroAdvancePanel advanceData={this.state.advanceData}/> }

          {!this.state.hideTilePanel &&
            <TilePanel tileData={this.state.tileData}/> }

          {!this.state.hideGatherPanel &&
            <GatherPanel selectedKey={this.state.selectedKey}/> }

          {!this.state.hideBuildPanel &&
            <BuildPanel structuresData={this.state.structuresData}/> }

          {!this.state.hideStructurePanel &&
            <StructurePanel structureData={this.state.structureData} />}

          {!this.state.hideAssignPanel && 
            <AssignPanel structuredId={this.state.structureData.id}
                         assignData={this.state.assignData} />}

          {!this.state.hideCraftPanel && 
            <CraftPanel structureId={this.state.structureData.id}
                        recipesData={this.state.recipesData} />}

          {!this.state.hideItemDividePanel && 
            <ItemDividePanel itemData={this.state.itemDivideData} />}

          {!this.state.hideMerchantQuantityPanel && 
            <MerchantQuantityPanel itemData={this.state.itemMerchantQuantityData}
                                   action={this.state.merchantAction} />}

          {!this.state.hideResourcePanel && 
            <ResourcePanel resourceData={this.state.resourceData} />}

          {!this.state.hideMerchantHirePanel && 
            <MerchantHirePanel hireData={this.state.hireData} />}

          {!this.state.hideExperimentPanel && 
            <ExperimentPanel expData={this.state.expData} />}

          {!this.state.hideErrorPanel && 
            <ErrorPanel errmsg={this.state.errmsg} />}
      </div>
    );
  }
}


/*
          <div
            onTransitionEnd={this.handleTransitionEnd} 
            style={{display: this.state.fierceButtonDisplay,
                    position: 'fixed',
                    bottom: '10px',
                    left: "50%",
                    marginLeft: '-50px',
                    backgroundColor: 'black',
                    opacity: 0.66,
                    width: '50px',
                    height: this.state.fierceButtonHeight + 'px',
                    transition: 'height 5s'}} />

          <img src={fierceattackbutton} 
              id="fierceattackbutton"
              className={styles.fierceattackbutton}
              onClick={this.handleFierceAttack}/>



*/