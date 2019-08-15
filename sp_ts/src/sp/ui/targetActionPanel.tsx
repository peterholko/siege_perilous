
import * as React from "react";
import targetactionpanel from "ui_comp/buttonsframe.png"
import { Network } from "../network";
import { Global } from "../global";

import inventorybutton from "ui_comp/inventorybutton.png";
import transferbutton from "ui_comp/transferbutton.png";
import statsbutton from "ui_comp/statsbutton.png";
import explorebutton from "ui_comp/explorebutton.png";
import gatherbutton from "ui_comp/gatherbutton.png";
import followbutton from "ui_comp/followbutton.png"; 
import infobutton from "ui_comp/infobutton.png";

import { Util } from "../util";
import { VILLAGER, DEAD, OBJ, TILE} from "../config";
import { Game } from "phaser";
import { GameEvent } from "../gameEvent";

interface TAProps {
  selectedKey : any
}

export default class TargetActionPanel extends React.Component<TAProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };

    this.handleInventoryClick = this.handleInventoryClick.bind(this)
    this.handleTransferClick = this.handleTransferClick.bind(this)
    this.handleExploreClick = this.handleExploreClick.bind(this)
    this.handleGatherClick = this.handleGatherClick.bind(this)
    this.handleFollowClick = this.handleFollowClick.bind(this)
    this.handleInfoClick = this.handleInfoClick.bind(this)
  }

  handleInventoryClick(event : React.MouseEvent) {
    console.log('Inventory Click');
    Network.sendInfoInventory(this.props.selectedKey.id);
    Global.gameEmitter.emit(GameEvent.TAP_CLICK, {});
  }

  handleTransferClick(event : React.MouseEvent) {
    console.log('Transfer Click');
    Network.sendInfoItemTransfer(Global.heroId, this.props.selectedKey.id);
    Global.gameEmitter.emit(GameEvent.TAP_CLICK, {});
  }

  handleStatsClick(event : React.MouseEvent) {
    Global.gameEmitter.emit(GameEvent.TAP_CLICK, {});
  }

  handleExploreClick(event : React.MouseEvent) {
    Global.gameEmitter.emit(GameEvent.TAP_CLICK, {});
  }

  handleGatherClick(event : React.MouseEvent) {
    Global.gameEmitter.emit(GameEvent.VILLAGER_GATHER_CLICK, this.props.selectedKey);
    Global.gameEmitter.emit(GameEvent.TAP_CLICK, {});
  }

  handleFollowClick(event : React.MouseEvent) {
    Network.sendFollow(this.props.selectedKey.id);
    Global.gameEmitter.emit(GameEvent.TAP_CLICK, {});
  }
  
  handleInfoClick(event : React.MouseEvent) {
    if(this.props.selectedKey.type == OBJ) {
      Network.sendInfoObj(this.props.selectedKey.id);
    } else if(this.props.selectedKey.type == TILE) {
      Network.sendInfoTile(this.props.selectedKey.x, 
                           this.props.selectedKey.y); 
    }

    Global.gameEmitter.emit(GameEvent.TAP_CLICK, {});
  }

  render() {
    var hideInfoButton = true;
    var hideInventoryButton = true;
    var hideTranferButton = true;
    var hideStatsButton = true;
    var hideExploreButton = true;
    var hideGatherButton = true;
    var hideFollowButton = true;

    var buttonOrder = {
      info: 0,
      inventory: 1,
      transfer: 2,
      explore: 3,
      gather: 4,
      follow: 5
    };

    if(this.props.selectedKey.type == OBJ) {
      if(Util.isPlayerObj(this.props.selectedKey.id)) {
        if(Util.isSubclass(this.props.selectedKey.id, VILLAGER)) {
          hideInfoButton = false;
          hideInventoryButton = false;
          hideTranferButton = false;
          hideExploreButton = false;
          hideGatherButton = false;
          hideFollowButton = false;

        } else {
          hideInfoButton = false;
          hideInventoryButton = false;
          hideTranferButton = false;
        }
      } else {
        if(Util.isState(this.props.selectedKey.id, DEAD)) {
          hideInfoButton = false;
          hideInventoryButton = false;
          hideTranferButton = false;
        } else {
          hideInfoButton = false;
        }
      }
    } else if(this.props.selectedKey.type == TILE) {
      hideInfoButton = false;
    }

    const targetActionPanelStyle = {
      top: '50%',
      left: '50%',
      width: '320px',
      height: '67px',
      marginTop: '-33px',
      marginLeft: '-160px',
      position: 'fixed',
      zIndex: 6
    } as React.CSSProperties

    const tapStyle = {
      position: 'fixed'
    } as React.CSSProperties

    const infoStyle = {
      transform: 'translate(8px, 8px)',
      position: 'fixed'
    } as React.CSSProperties

    const inventoryStyle = {
      transform: 'translate(58px, 8px)',
      position: 'fixed'
    } as React.CSSProperties

    const transferStyle = {
      transform: 'translate(108px, 8px)',
      position: 'fixed'  
    } as React.CSSProperties

    const statsStyle = {
      transform: 'translate(158px, 8px)',
      position: 'fixed'  
    } as React.CSSProperties

    const exploreStyle = {
      transform: 'translate(158px, 8px)',
      position: 'fixed'  
    } as React.CSSProperties

    const gatherStyle = {
      transform: 'translate(208px, 8px)',
      position: 'fixed'  
    } as React.CSSProperties

    const followStyle = {
      transform: 'translate(258px, 8px)',
      position: 'fixed'  
    } as React.CSSProperties

    return (
      <div style={targetActionPanelStyle} >
          <img src={targetactionpanel} style={tapStyle}/>

          {!hideInfoButton && 
              <img src={infobutton} 
                   style={infoStyle} 
                   onClick={this.handleInfoClick}/>}
                 
          {!hideInventoryButton &&
            <img src={inventorybutton} 
                 style={inventoryStyle} 
                 onClick={this.handleInventoryClick}/>}

          {!hideTranferButton &&  
            <img src={transferbutton} 
                 style={transferStyle} 
                 onClick={this.handleTransferClick}/>}

          {!hideExploreButton && 
            <img src={explorebutton} 
                 style={exploreStyle} 
                 onClick={this.handleExploreClick}/>}

          {!hideGatherButton && 
            <img src={gatherbutton} 
                 style={gatherStyle} 
                 onClick={this.handleGatherClick}/>}

          {!hideFollowButton && 
            <img src={followbutton} 
                 style={followStyle} 
                 onClick={this.handleFollowClick}/>}

      </div>
    );
  }
}