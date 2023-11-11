
import * as React from "react";
import targetactionpanel from "ui_comp/buttonsframe.png"
import { Network } from "../network";
import { Global } from "../global";

import inventorybutton from "ui_comp/inventorybutton.png";
import transferbutton from "ui_comp/transferbutton.png";
import explorebutton from "ui_comp/explorebutton.png";
import gatherbutton from "ui_comp/gatherbutton.png";
import followbutton from "ui_comp/followbutton.png"; 
import infobutton from "ui_comp/infobutton.png";
import merchantbutton from "ui_comp/merchantbutton.png";

import { Util } from "../util";
import { VILLAGER, DEAD, OBJ, TILE, FOUNDED, BUTTON_WIDTH} from "../config";
import { GameEvent } from "../gameEvent";

interface TAProps {
  selectedBoxPos : integer,
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
    this.handleMerchantClick = this.handleMerchantClick.bind(this)
  }

  handleInventoryClick(event : React.MouseEvent) {
    console.log('Inventory Click');
    Network.sendInfoInventory(this.props.selectedKey.id);
    Global.gameEmitter.emit(GameEvent.TAP_CLICK, {});
  }

  handleTransferClick(event : React.MouseEvent) {
    console.log('Transfer Click');
    Global.infoItemTransferAction = 'transfer';
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

  handleMerchantClick() {
    Global.infoItemTransferAction = 'merchant';
    Network.sendInfoItemTransfer(Global.heroId, this.props.selectedKey.id);
    Global.gameEmitter.emit(GameEvent.MERCHANT_CLICK, this.props.selectedKey.id);
  }

  render() {
    var hideInfoButton = true;
    var hideInventoryButton = true;
    var hideTranferButton = true;
    var hideExploreButton = true;
    var hideGatherButton = true;
    var hideFollowButton = true;
    var hideMerchantButton = true;

    var buttonOrder = {
      info: 0,
      inventory: 1,
      transfer: 2,
      explore: 3,
      gather: 4,
      follow: 5
    };

    var numButtons = 1;

    if(this.props.selectedKey.type == OBJ) {
      if(Util.isPlayerObj(this.props.selectedKey.id)) {
        if(Util.isSubclass(this.props.selectedKey.id, VILLAGER)) {
          hideInfoButton = false;
          hideInventoryButton = false;
          hideTranferButton = false;
          hideExploreButton = false;
          hideGatherButton = false;
          hideFollowButton = false;
          numButtons = 3; //Shortcut because the explore, gather, follow are stacked below

        } else if(Util.isState(this.props.selectedKey.id, FOUNDED)) {
          hideInfoButton = false;
          hideTranferButton = false;
          numButtons = 2;
        } else {
          hideInfoButton = false;
          hideInventoryButton = false;
          hideTranferButton = false;
          numButtons = 3;
        }
      } else {
        if(Util.isState(this.props.selectedKey.id, DEAD)) {
          hideInfoButton = false;
          hideTranferButton = false;
          numButtons = 2;
        }
        else if(Util.isSubclass(this.props.selectedKey.id, "merchant")) {
          hideMerchantButton = false;
          hideInfoButton = false;
        }
        else if(Util.hasGroup(this.props.selectedKey.id, "Tax Collector")) {
          hideTranferButton = false;
          hideInfoButton = false;
        } else {
          hideInfoButton = false;
        }
      }
    } else if(this.props.selectedKey.type == TILE) {
      hideInfoButton = false;
    }

    var panelWidth = numButtons * BUTTON_WIDTH;
    var panelPos = ((this.props.selectedBoxPos + 1) * 74) + 35 - 37 + panelWidth / 2;

    const targetActionPanelStyle = {
      top: '82px',
      right:  panelPos + 'px',
      position: 'fixed',
      zIndex: 6
    } as React.CSSProperties

    const tapStyle = {
      position: 'fixed',
      width: '67px',
      height: '67px'
    } as React.CSSProperties

    const infoStyle = {
      transform: 'translate(0px, 0px)',
      position: 'fixed'
    } as React.CSSProperties

    const inventoryStyle = {
      transform: 'translate(100px, 0px)',
      position: 'fixed'
    } as React.CSSProperties

    const transferStyle = {
      transform: 'translate(50px, 0px)',
      position: 'fixed'  
    } as React.CSSProperties

    const merchantStyle = {
      transform: 'translate(50px, 0px)',
      position: 'fixed'  
    } as React.CSSProperties

    const exploreStyle = {
      transform: 'translate(0px, 50px)',
      position: 'fixed'  
    } as React.CSSProperties

    const gatherStyle = {
      transform: 'translate(50px, 50px)',
      position: 'fixed'  
    } as React.CSSProperties

    const followStyle = {
      transform: 'translate(100px, 50px)',
      position: 'fixed'  
    } as React.CSSProperties

    return (
      <div style={targetActionPanelStyle} >

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

          {!hideMerchantButton && 
            <img src={merchantbutton} 
                 style={merchantStyle} 
                 onClick={this.handleMerchantClick}/>}

      </div>
    );
  }
}