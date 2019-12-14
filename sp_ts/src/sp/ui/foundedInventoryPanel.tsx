import * as React from "react";
import HalfPanel from "./halfPanel";
import InventoryItem from "./inventoryItem";
import { Global } from "../global";

import itemframe from "ui_comp/itemframe.png";
import selectitemborder from "ui_comp/selectitemborder.png";
import { Util } from "../util";
import ResourceItem from "./resourceItem";
import { STRUCTURE, FOUNDED } from "../config";

interface FoundedInventoryProps {
  id: integer,
  items: any,
  reqs: any,
  panelType : string,
  hideExitButton : boolean,
  hideSelect : boolean,
  handleSelect : Function,
}

export default class FoundedInventoryPanel extends React.Component<FoundedInventoryProps, any> {
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
    var yPos = 232 + (Math.floor(eventData.index / 5) * 53);

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
    const objId = this.props.id;
    const itemFrames = []
    
    const items = []
    const reqs = []

    const spriteStyle = {
      transform: 'translate(-200px, 5px)',
      position: 'fixed'
    } as React.CSSProperties

    const reqStyle = {
      transform: 'translate(-295px, 75px)',
      position: 'fixed',
      textAlign: 'left',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '295px'
    } as React.CSSProperties

    const materialStyle = {
      transform: 'translate(-295px, 210px)',
      position: 'fixed',
      textAlign: 'left',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '295px'
    } as React.CSSProperties


    if(Util.isSprite(Global.objectStates[objId].image)) {
      var imageName = Global.objectStates[objId].image + '_single.png';
    } else {
      var imageName = Global.objectStates[objId].image + '.png';
    }
    
    for(var i = 0; i < this.props.reqs.length; i++) {
      var xPos = 25 + ((i % 5) * 53);
      var yPos = -265 + (Math.floor(i / 5) * 53);

      reqs.push(
        <ResourceItem key={i}
                      index={i}
                      resourceName={this.props.reqs[i].type}
                      quantity={this.props.reqs[i].quantity}
                      currentQuantity={this.props.reqs[i].cquantity}
                      showQuantity={true} 
                      xPos={xPos}
                      yPos={yPos}/>
      )
    }

    for(var i = 0; i < 10; i++) {
      var xPos = -293 + ((i % 5) * 53);
      var yPos = 232 + (Math.floor(i / 5) * 53);

      var itemFrameStyle = {
        transform: 'translate(' + xPos + 'px, ' + yPos + 'px)',
        position: 'fixed'
      } as React.CSSProperties

      itemFrames.push(<img src={itemframe} key={i} style={itemFrameStyle}/> )
    }

    for(var i = 0; i < this.props.items.length; i++) {
      console.log('Item: ' + this.props.items[i]);
      var itemId = this.props.items[i].id;
      var itemName = this.props.items[i].name;
      var quantity = this.props.items[i].quantity;

      var xPos = 31 + ((i % 5) * 53);
      var yPos = -127 + (Math.floor(i / 5) * 53);

      items.push(<InventoryItem key={i}
                                ownerId={objId}
                                itemId={itemId} 
                                itemName={itemName} 
                                quantity={quantity}
                                index={i}
                                xPos={xPos}
                                yPos={yPos}
                                handleSelect={this.handleSelect} />);
  }

    return (
      <HalfPanel left={false} 
                 panelType={this.props.panelType} 
                 hideExitButton={this.props.hideExitButton}>
        <img src={'/static/art/' + imageName} style={spriteStyle} />
        <span style={reqStyle}>Requirements:</span>
        {reqs}
        <span style={materialStyle}>Materials:</span>
        {itemFrames}
        {items}
        {!this.props.hideSelect && 
          <img src={selectitemborder} style={this.state.selectItemStyle} />
        }
      </HalfPanel>
    );
  }
}

