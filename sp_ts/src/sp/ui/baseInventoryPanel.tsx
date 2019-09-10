import * as React from "react";
import HalfPanel from "./halfPanel";
import InventoryItem from "./inventoryItem";
import { Global } from "../global";

import itemframe from "ui_comp/itemframe.png";
import selectitemborder from "ui_comp/selectitemborder.png";
import { Util } from "../util";

interface BaseInventoryProps {
  left : boolean,
  inventoryData,
  panelType : string,
  hideExitButton : boolean,
  hideSelect : boolean,
  handleSelect : Function,
}

export default class BaseInventoryPanel extends React.Component<BaseInventoryProps, any> {
  constructor(props) {
    super(props);

    const selectItemStyle = {
      position: "fixed"
    } as React.CSSProperties

    this.state = {
      selectItemStyle : selectItemStyle
    };
    
    this.handleSelect = this.handleSelect.bind(this)
  }

  handleSelect(eventData) {
    console.log('handleSelect ' + eventData);
    var xPos = -293 + ((eventData.index % 5) * 53);
    var yPos = 73 + (Math.floor(eventData.index / 5) * 53);

    const selectItemStyle = {
      transform: 'translate(' + xPos + 'px, ' + yPos + 'px)',
      position: 'fixed'
    }

    Global.selectedItemOwnerId = eventData.ownerId;
    Global.selectedItemId = eventData.itemId;

    this.setState({selectItemStyle: selectItemStyle});

    this.props.handleSelect(eventData);
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
      var quantity = this.props.inventoryData.items[i].quantity;

      var xPos = -292 + ((i % 5) * 53);
      var yPos = 74 + (Math.floor(i / 5) * 53);

      items.push(<InventoryItem key={i}
                                ownerId={objId}
                                itemId={itemId} 
                                itemName={itemName} 
                                quantity={quantity}
                                index={i}
                                handleSelect={this.handleSelect}/>);
    }

    return (
      <HalfPanel left={this.props.left} 
                 panelType={this.props.panelType} 
                 hideExitButton={this.props.hideExitButton}>
        <img src={'/static/art/' + imageName} style={spriteStyle} />
        {itemFrames}
        {items}
        {!this.props.hideSelect && 
          <img src={selectitemborder} style={this.state.selectItemStyle} />
        }
      </HalfPanel>
    );
  }
}

