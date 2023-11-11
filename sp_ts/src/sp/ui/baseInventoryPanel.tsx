import * as React from "react";
import HalfPanel from "./halfPanel";
import InventoryItem from "./inventoryItem";
import { Global } from "../global";

import itemframe from "ui_comp/itemframe.png";
import selectitemborder from "ui_comp/selectitemborder.png";
import { Util } from "../util";
import ResourceItem from "./resourceItem";
import { STRUCTURE, FOUNDED } from "../config";

interface BaseInventoryProps {
  left : boolean,
  id: integer,
  items: any,
  capacity?: integer,
  totalWeight?: integer,
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
    Global.selectedItemName = eventData.itemName;

    this.setState({selectItemStyle: selectItemStyle});

    this.props.handleSelect(eventData);
  }

  render() {
    const objId = this.props.id;
    const itemFrames = []
    const items = []
    const reqs = []

    var imageName;
    var capacityText;
    var selectItemStyle = this.state.selectItemStyle;

    if(Util.isSprite(Global.objectStates[objId].image)) {
      imageName = Global.objectStates[objId].image + '_single.png';
    } else {
      imageName = Global.objectStates[objId].image + '.png';
    } 

    if(this.props.capacity != null && this.props.totalWeight != null) {
      capacityText = this.props.totalWeight + '/' + this.props.capacity + ' lbs';      
    } else {
      capacityText = '';
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

    var anyItemSelected = false;

    for(var i = 0; i < this.props.items.length; i++) {
      console.log('Item: ' + this.props.items[i]);
      var itemId = this.props.items[i].id;
      var itemName = this.props.items[i].name;
      var image = this.props.items[i].image;
      var quantity = this.props.items[i].quantity;

      var xPos = 31 + ((i % 5) * 53);
      var yPos = -286 + (Math.floor(i / 5) * 53);

      items.push(<InventoryItem key={i}
                                ownerId={objId}
                                itemId={itemId} 
                                itemName={itemName}
                                image={image} 
                                quantity={quantity}
                                index={i}
                                xPos={xPos}
                                yPos={yPos}
                                handleSelect={this.handleSelect} />);

      if(Global.selectedItemId == itemId) {
        var xPos = -293 + ((i % 5) * 53);
        var yPos = 73 + (Math.floor(i / 5) * 53);
    
        const style = {
          transform: 'translate(' + xPos + 'px, ' + yPos + 'px)',
          position: 'fixed'
        };

        selectItemStyle = style;
        anyItemSelected = true;
      }
    }

    const spriteStyle = {
      transform: 'translate(-290px, 5px)',
      position: 'fixed'
    } as React.CSSProperties

    const spanNameStyle = {
      transform: 'translate(-225px, 20px)',
      position: 'fixed',
      textAlign: 'left',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
    } as React.CSSProperties

    const capacityTextStyle = {
      transform: 'translate(-225px, 40px)',
      position: 'fixed',
      textAlign: 'left',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
    } as React.CSSProperties


    return (
      <HalfPanel left={this.props.left} 
                 panelType={this.props.panelType} 
                 hideExitButton={this.props.hideExitButton}>
        <img src={'/static/art/' + imageName} style={spriteStyle} />
        <span style={spanNameStyle}>
          {Global.objectStates[objId].name}
        </span>

        <span style={capacityTextStyle}>
          {capacityText}
        </span>

        {itemFrames}
        {items}
        {reqs}
        {anyItemSelected && 
          <img src={selectitemborder} style={selectItemStyle} />
        }
      </HalfPanel>
    );
  }
}

