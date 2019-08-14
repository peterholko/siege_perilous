
import { Network } from './network';
import { Util } from './util';
import { Global } from './global';
import { ObjectState } from './objectState';
import { GameEvent } from './gameEvent';
import { Tile } from './objects/tile';
import { Obj } from './obj';

import * as React from "react";
import styles from "./ui.css";

import SelectBox from './ui/selectBox';
import InventoryPanel from './ui/inventoryPanel';
import ItemPanel from './ui/itemPanel';

import heroring from "ui/heroring.png";
import hpframe from "ui/hpframe.png";
import movecompass from "ui/movecompass.png";
import statbg from "ui/statbg.png";
import hpbar from "ui/hpbar.png";
import stabar from "ui/stabar.png";
import manabar from "ui/manabar.png";

import hero from "art/heromage_single.png";
import inventorybutton from "ui/inventorybutton.png";
import attrsbutton from "ui/attrsbutton.png";
import buildbutton from "ui/buildbutton.png";
import explorebutton from "ui/explorebutton.png";
import gatherbutton from "ui/gatherbutton.png";

import { NetworkEvent } from './networkEvent';
import { STAT_BAR_WIDTH, STAT_BAR_HEIGHT, TILE, OBJ, HERO, VILLAGER } from './config';
import TargetActionPanel from './ui/targetActionPanel';
import ItemTransferPanel from './ui/itemTransferPanel';
import HeroPanel from './ui/heroPanel';
import VillagerPanel from './ui/villagerPanel';
import AttrsPanel from './ui/attrsPanel';
import SkillsPanel from './ui/skillsPanel';

interface UIState {
  selectBoxes : [],
  inventoryPanels : [],
  hideTargetActionPanel: boolean,
  hideInventoryPanel: boolean,
  hideItemTransferPanel: boolean,
  hideItemPanel: boolean,
  hideHeroPanel: boolean,
  hideVillagerPanel: boolean,
  hideAttrsPanel : boolean,
  hideSkillsPanel : boolean,
  leftInventoryId : integer,
  leftInventoryData: [],
  rightInventoryId : integer,
  rightInventoryData: [],
  itemData : any,
  heroData : any,
  villagerData : any,
  attrsData : any,
  skillsData : any,
  selectedKey: {},
  hpBarWidth : integer,
  staBarWidth : integer,
  manaBarWidth: integer
}

export default class UI extends React.Component<any, UIState>{
  private compassRef = React.createRef<HTMLImageElement>();

  constructor(props) {
    super(props);

    this.state = {
      selectBoxes: [],
      inventoryPanels: [],
      hideTargetActionPanel: true,
      hideInventoryPanel: true,
      hideItemTransferPanel: true,
      hideItemPanel: true,
      hideHeroPanel: true,
      hideVillagerPanel: true,
      hideAttrsPanel : true,
      hideSkillsPanel : true,
      leftInventoryId: -1,
      leftInventoryData: [],
      rightInventoryId: -1,
      rightInventoryData: [],
      itemData : {},
      heroData : {},
      villagerData : {},
      attrsData : {},
      skillsData : {},
      selectedKey: {},
      hpBarWidth: STAT_BAR_WIDTH,
      staBarWidth: STAT_BAR_WIDTH,
      manaBarWidth: STAT_BAR_WIDTH
    }

    this.handleMoveClick = this.handleMoveClick.bind(this);
    this.handleTileClick = this.handleTileClick.bind(this);

    this.handleHeroAttrsClick = this.handleHeroAttrsClick.bind(this);
    this.handleHeroInventoryClick = this.handleHeroInventoryClick.bind(this);
    this.handleHeroExploreClick = this.handleHeroExploreClick.bind(this);
    this.handleHeroBuildClick = this.handleHeroBuildClick.bind(this);
    this.handleHeroGatherClick = this.handleHeroGatherClick.bind(this);

    Global.gameEmitter.on(GameEvent.TILE_CLICK, this.handleTileClick, this);
    Global.gameEmitter.on(GameEvent.SELECTBOX_CLICK, this.handleSelectBoxClick, this);
    Global.gameEmitter.on(GameEvent.EXIT_HALFPANEL_CLICK, this.handleExitHalfPanelClick, this);
    Global.gameEmitter.on(GameEvent.TAP_CLICK, this.handleTargetActionPanelClick, this);

    Global.gameEmitter.on(NetworkEvent.STATS, this.handleStats, this);
    Global.gameEmitter.on(NetworkEvent.INFO_OBJ, this.handleInfoObj, this);
    Global.gameEmitter.on(NetworkEvent.INFO_ITEM, this.handleInfoItem, this);
    Global.gameEmitter.on(NetworkEvent.INFO_INVENTORY, this.handleInfoInventory, this);
    Global.gameEmitter.on(NetworkEvent.INFO_ITEM_TRANSFER, this.handleInfoItemTransfer, this);
    Global.gameEmitter.on(NetworkEvent.INFO_ATTRS, this.handleInfoAttrs, this);
    Global.gameEmitter.on(NetworkEvent.INFO_SKILLS, this.handleInfoSkills, this);
    Global.gameEmitter.on(NetworkEvent.ITEM_TRANSFER, this.handleItemTransfer, this);
  }

  handleMoveClick(event : React.MouseEvent) {
    const compass = this.compassRef.current!;

      console.log('compass click');
      console.log('offset: ' + event.nativeEvent.offsetX + ', ' + event.nativeEvent.offsetY);
      console.log('compass: ' + this.compassRef);

      var pocX = event.nativeEvent.offsetX - compass.naturalWidth / 2;
      var pocY = event.nativeEvent.offsetY - compass.naturalHeight / 2;

      console.log('poc: ' + pocX + ', ' + pocY);

      var angleRads = Math.atan2(pocX, pocY);
      var angleDegrees = ((angleRads * 180) / Math.PI) + 180;

      console.log('Angle: ' + angleDegrees);

      var heroObj = Global.objectStates[Global.heroId] as ObjectState;

      if(angleDegrees < 30 || angleDegrees >= 330) {
        console.log('N');
        var nextPos = Util.nextPosByDirection(heroObj.hexX, heroObj.hexY, 'N');
        Network.sendMove(nextPos.q, nextPos.r);
      } else if(angleDegrees < 90 && angleDegrees >= 30) {
        console.log('NW');
        var nextPos = Util.nextPosByDirection(heroObj.hexX, heroObj.hexY, 'NW');
        Network.sendMove(nextPos.q, nextPos.r);
      } else if(angleDegrees < 150 && angleDegrees >= 90) {
        console.log('SW');
        var nextPos = Util.nextPosByDirection(heroObj.hexX, heroObj.hexY, 'SW');
        Network.sendMove(nextPos.q, nextPos.r);
      } else if(angleDegrees < 210 && angleDegrees >= 150) {
        console.log('S');
        var nextPos = Util.nextPosByDirection(heroObj.hexX, heroObj.hexY, 'S');
        Network.sendMove(nextPos.q, nextPos.r);
      } else if(angleDegrees < 270 && angleDegrees >= 210) {
        console.log('SE');
        var nextPos = Util.nextPosByDirection(heroObj.hexX, heroObj.hexY, 'SE');
        Network.sendMove(nextPos.q, nextPos.r);
      } else if(angleDegrees < 330 && angleDegrees >= 270) {
        console.log('NE');
        var nextPos = Util.nextPosByDirection(heroObj.hexX, heroObj.hexY, 'NE');
        Network.sendMove(nextPos.q, nextPos.r);
      }
  }

  handleTileClick(gameObject) {
    console.log("gameObject: " + gameObject);
    const boxes : any = []  // Strange react bug where type wasn't working

    const tile = gameObject as Tile;
    const objIdsOnTile = Obj.getObjsAt(tile.hexX, tile.hexY);

    //Add terain tile first
    var tileIndex = tile.hexX + '_' + tile.hexY;
    var tileState = Global.tileStates[tileIndex];
    var tileId = tileState.tiles.reverse()[0];
    var imageName = Global.tileset[tileId].image; 

    var style = {
      top: '10px',
      right: '10px',
      position: 'fixed'
    } as React.CSSProperties

    var imageStyle = {
      top: '24px',
      right: '24px',
      width: '45px',
      height: '45px',
      position: 'fixed'
    } as React.CSSProperties

    boxes.push(<SelectBox key={-1} 
                          selectedKey={{type: TILE, id: tileIndex}}
                          imageName={imageName} 
                          style={style}
                          imageStyle={imageStyle} />)

    for(var i = 0; i < objIdsOnTile.length; i++) {
      if(i > 5)
        break;

      const objId : integer = Number(objIdsOnTile[i]);

      if(Util.isSprite(Global.objectStates[objId].image)) {
        var imageName = Global.objectStates[objId].image + '_single.png';
      } else {
        var imageName = Global.objectStates[objId].image + '.png';
      }

      var style = {
        top: '10px',
        right: 85 + (75 * i) + 'px',
        position: 'fixed'
      } as React.CSSProperties

      boxes.push(<SelectBox key={i} 
                            selectedKey={{type: OBJ, id: objId}}
                            imageName={imageName}
                            style={style} />);
    }

    this.setState({selectBoxes: boxes,
                   hideTargetActionPanel: true});
  }

  handleSelectBoxClick(eventData) {
    console.log('SelectBoxClick');
    this.setState({hideTargetActionPanel: false,
                   selectedKey: eventData})
  }

  handleExitHalfPanelClick(event) {
    console.log('ExitHalfPanel');

    if(event.panelType == 'inventory') {
      this.setState({hideInventoryPanel: true});
    } else if(event.panelType == 'itemTransfer') {
      this.setState({hideItemTransferPanel: true})
    } else if(event.panelType == 'item') {
      this.setState({hideItemPanel: true});
    } else if(event.panelType == 'hero') {
      this.setState({hideHeroPanel: true});
    } else if(event.panelType == 'villager') {
      this.setState({hideVillagerPanel: true});
    } else if(event.panelType == 'attrs') {
      this.setState({hideAttrsPanel: true});
    } else if(event.panelType == 'skills') {
      this.setState({hideSkillsPanel: true});
    }
  }

  handleTargetActionPanelClick(event : React.MouseEvent) {
    this.setState({hideTargetActionPanel: true});
  }

  handleHeroAttrsClick(event: React.MouseEvent) {
    Network.sendInfoObj(Global.heroId);
  }

  handleHeroInventoryClick(event: React.MouseEvent) {
    Network.sendInfoInventory(Global.heroId);
  }

  handleHeroExploreClick(event: React.MouseEvent) {
  }
 
  handleHeroBuildClick(event: React.MouseEvent) {
  }

  handleHeroGatherClick(event: React.MouseEvent) {
  }

  handleStats(message) {
    console.log('UI handleStats');
    const hpRatio = message.hp / message.maxHp;
    const hpBarWidth = hpRatio * STAT_BAR_WIDTH;

    this.setState({hpBarWidth: hpBarWidth});
  }

  handleInfoObj(message) {
    console.log('UI handleInfoObj');
    if(Util.isPlayerObj(message.id)) {
      if(Util.isSubclass(message.id, HERO)) {
        this.setState({hideHeroPanel: false, heroData: message});  
      } else if(Util.isSubclass(message.id, VILLAGER)) {
        this.setState({hideVillagerPanel: false, villagerData: message});
      }
    }
    //this.setState({hideHeroPanel: false, heroData: message});
  }

  handleInfoItem(message) {
    console.log('UI handleInfoItem');
    this.setState({hideItemPanel: false, itemData: message});
  }

  handleInfoInventory(message) {
    console.log('UI handleInfoInventory');
    this.setState({hideInventoryPanel: false, leftInventoryData: message});
  }

  handleInfoItemTransfer(message) {
    console.log('UI handleInfoItemTransfer');

    this.setState({hideItemTransferPanel: false,
                   leftInventoryId: message.sourceid, 
                   leftInventoryData: message.sourceitems,
                   rightInventoryId: message.targetid,
                   rightInventoryData: message.targetitems});
  }

  handleInfoAttrs(message) {
    console.log('UI handleInfoAttrs');
    this.setState({hideAttrsPanel: false, attrsData: message});
  }

  handleInfoSkills(message) {
    console.log('UI handleInfoSkills');
    this.setState({hideSkillsPanel: false, skillsData: message});
  }

  handleItemTransfer(message) {
    console.log('UI handleItemTransfer');
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
                   rightInventoryData: rightInventoryData})
  }

  render() {

    const hpBarStyle  = {
      transform: 'translate(97px, 19px)',
      width: this.state.hpBarWidth + 'px',
      height: STAT_BAR_HEIGHT + 'px',
      zIndex: 4,
      position: 'fixed' 
    } as React.CSSProperties
 
    const staBarStyle  = {
      transform: 'translate(97px, 37px)',
      width: this.state.staBarWidth + 'px',
      height: STAT_BAR_HEIGHT + 'px',
      zIndex: 4,
      position: 'fixed' 
    } as React.CSSProperties
  
    const manaBarStyle  = {
      transform: 'translate(97px, 55px)',
      width: this.state.hpBarWidth + 'px',
      height: STAT_BAR_HEIGHT + 'px',
      zIndex: 4,
      position: 'fixed' 
    } as React.CSSProperties
  
    return(
      <div id="ui" className={styles.ui}>
          <img src={heroring} id="heroring" className={styles.heroring}/>
          <img src={hpframe} id="hpframe" className={styles.hpframe}/>

          <img src={statbg} id="hpbg" className={styles.hpbg}/>
          <img src={hpbar} id="hpbar" style={hpBarStyle}/>
          <img src={statbg} id="stabg" className={styles.stabg}/>
          <img src={stabar} id="stabar" style={staBarStyle}/>
          <img src={statbg} id="manabg" className={styles.manabg}/>
          <img src={manabar} id="manabar" style={manaBarStyle}/>

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

          <img src={hero} id="hero" className={styles.hero}/>

          <img src={movecompass} 
               id="movecompass" 
               ref={this.compassRef}
               className={styles.movecompass} 
               onClick={this.handleMoveClick}/>

          {this.state.selectBoxes}

          {!this.state.hideTargetActionPanel && 
            <TargetActionPanel selectedKey={this.state.selectedKey}/> }

          {!this.state.hideInventoryPanel && 
            <InventoryPanel left={true} 
                            inventoryData={this.state.leftInventoryData}
                            hideExitButton={false} /> }

          {!this.state.hideItemTransferPanel && 
            <ItemTransferPanel leftInventoryData={this.state.leftInventoryData} 
                               rightInventoryData={this.state.rightInventoryData} /> }
                               
          {!this.state.hideItemPanel &&
            <ItemPanel itemData={this.state.itemData}/> }

          {!this.state.hideHeroPanel &&
            <HeroPanel heroData={this.state.heroData}/> }

          {!this.state.hideVillagerPanel &&
            <VillagerPanel villagerData={this.state.villagerData}/> }
          
          {!this.state.hideAttrsPanel &&
            <AttrsPanel attrsData={this.state.attrsData}/> }

          {!this.state.hideSkillsPanel &&
            <SkillsPanel skillsData={this.state.skillsData}/> }
      </div>
    );
  }
}
