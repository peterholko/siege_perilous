import * as React from "react";
import HalfPanel from "./halfPanel";
import InventoryItem from "./inventoryItem";
import { Global } from "../global";

import itemframe from "ui_comp/itemframe.png";
import selectitemborder from "ui_comp/selectitemborder.png";
import leftbutton from "ui_comp/leftbutton.png";
import rightbutton from "ui_comp/rightbutton.png";
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
      selectItemStyle : selectItemStyle,
      page: 0
    };
    
    this.handleSelect = this.handleSelect.bind(this)
    this.handleLeftClick = this.handleLeftClick.bind(this);
    this.handleRightClick = this.handleRightClick.bind(this);
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

  handleLeftClick(event) {
    console.log("Left Click - page: " + this.state.page);
    if(this.state.page != 0) {
      const newPage = this.state.page - 1;
      this.setState({page: newPage})
    } 
  }

  handleRightClick(event) {
    console.log("Right Click - page: " + this.state.page);
    if(this.state.page != (Math.ceil(this.props.items.length / 20) - 1)) {
      const newPage = this.state.page + 1;
      this.setState({page: newPage})
    } 
  }

  render() {
    const objId = this.props.id;
    const itemFrames = []
    const items = []
    const reqs = []

    var imageName;
    var capacityText;
    var selectItemStyle = this.state.selectItemStyle;
    var hideLeftButton = false;
    var hideRightButton = false;

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
    var maxItemIndex = (this.state.page + 1) * 20;

    if(maxItemIndex > (this.props.items.length - 1)) {
      maxItemIndex = this.props.items.length;
    }

    console.log("maxItemIndex: " + maxItemIndex);
    console.log("state page: " + this.state)

    var itemPageIndex = 0;

    for(var itemIndex = this.state.page * 20; itemIndex < maxItemIndex; itemIndex++) {
      console.log('Item: ' + JSON.stringify(this.props.items[itemIndex]));
      var itemId = this.props.items[itemIndex].id;
      var itemName = this.props.items[itemIndex].name;
      var image = this.props.items[itemIndex].image;
      var quantity = this.props.items[itemIndex].quantity;

      var xPos = 31 + ((itemPageIndex % 5) * 53);
      var yPos = -286 + (Math.floor(itemPageIndex / 5) * 53);

      items.push(<InventoryItem key={itemPageIndex}
                                ownerId={objId}
                                itemId={itemId} 
                                itemName={itemName}
                                image={image} 
                                quantity={quantity}
                                index={itemPageIndex}
                                xPos={xPos}
                                yPos={yPos}
                                handleSelect={this.handleSelect} />);

      if(Global.selectedItemId == itemId) {
        var xPos = -293 + ((itemPageIndex % 5) * 53);
        var yPos = 73 + (Math.floor(itemPageIndex / 5) * 53);
    
        const style = {
          transform: 'translate(' + xPos + 'px, ' + yPos + 'px)',
          position: 'fixed'
        };

        selectItemStyle = style;
        anyItemSelected = true;
      }

      itemPageIndex++;
    }

    if(this.state.page == 0) {
      hideLeftButton = true;
    }

    if(this.props.items.length == 0) {
      hideRightButton = true;
    } else if((Math.ceil(this.props.items.length / 20) - 1) == this.state.page) {
      hideRightButton = true;
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

    const leftStyle = {
      transform: 'translate(-305px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const rightStyle = {
      transform: 'translate(-65px, 295px)',
      position: 'fixed'
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
        {!hideLeftButton && <img src={leftbutton} style={leftStyle} onClick={this.handleLeftClick} />}
        {!hideRightButton && <img src={rightbutton} style={rightStyle} onClick={this.handleRightClick} />}
      </HalfPanel>
    );
  }
}

