import * as React from "react";
import HalfPanel from "./halfPanel";
import InventoryItem from "./inventoryItem";
import { Global } from "../global";
import { GameEvent } from "../gameEvent";

import itemframe from "ui_comp/itemframe.png";
import selectitemborder from "ui_comp/selectitemborder.png";
import { Util } from "../util";
import { Network } from "../network";

interface InventoryProps {
  left : boolean,
  inventoryData,
  hideExitButton : boolean,
  panelType? : string
}

export default class InventoryPanel extends React.Component<InventoryProps, any> {
  constructor(props) {
    super(props);

    const selectItemStyle = {
      position: "fixed"
    } as React.CSSProperties

    this.state = {
      panelType: this.props.panelType || 'inventory',
      hideSelectItem: true,
      selectItemStyle : selectItemStyle
    };
    
    this.handleSelect = this.handleSelect.bind(this)
  }

  handleSelect(eventData) {
    console.log('handleSelect ' + eventData);

    const selectItemStyle = {
      transform: 'translate(' + eventData.xPos + 'px, ' + eventData.yPos + 'px)',
      position: 'fixed'
    }

    Global.selectedItemOwnerId = eventData.ownerId;
    Global.selectedItemId = eventData.itemId;

    this.setState({hideSelectItem: false,
                   selectItemStyle: selectItemStyle});

    if(this.state.panelType == 'inventory') {
      //Global.gameEmitter.emit(GameEvent.ITEM_INV_CLICK, eventData.itemId);
      Network.sendInfoItem(eventData.itemId);
    }
  }

  render() {
    const objId = this.props.inventoryData.id;
    const itemFrames = []
    const items = []

    const spriteStyle = {
      transform: 'translate(-200px, 5px)',
      position: 'fixed'
    } as React.CSSProperties

    if(Util.isSprite(Global.objectStates[objId].image)) {
      var imageName = Global.objectStates[objId].image + '_single.png';
    } else {
      var imageName = Global.objectStates[objId].image + '.png';
    } 

    for(var i = 0; i < 20; i++) {
      var xPos = -293 + ((i % 5) * 53);
      var yPos = 73 + (Math.floor(i / 5) * 53);

      var itemFrameStyle = {
        transform: 'translate(' + xPos + 'px, ' + yPos + 'px)',
        position: 'fixed'
      } as React.CSSProperties

      itemFrames.push(<img src={itemframe} key={i} style={itemFrameStyle}/> )
    }

    for(var i = 0; i < this.props.inventoryData.items.length; i++) {
      console.log('Item: ' + this.props.inventoryData.items[i]);
      var itemId = this.props.inventoryData.items[i].id;
      var itemName = this.props.inventoryData.items[i].name;

      var xPos = -292 + ((i % 5) * 53);
      var yPos = 74 + (Math.floor(i / 5) * 53);

      items.push(<InventoryItem key={i}
                                ownerId={objId}
                                itemId={itemId} 
                                itemName={itemName} 
                                xPos={xPos} 
                                yPos={yPos}
                                handleSelect={this.handleSelect}/>);
    }

    return (
      <HalfPanel left={this.props.left} 
                 panelType={this.state.panelType} 
                 hideExitButton={this.props.hideExitButton}>
        <img src={'/static/art/' + imageName} style={spriteStyle} />
        {itemFrames}
        {items}
        {!this.state.hideSelectItem && 
          <img src={selectitemborder} style={this.state.selectItemStyle} />
        }
      </HalfPanel>
    );
  }
}

