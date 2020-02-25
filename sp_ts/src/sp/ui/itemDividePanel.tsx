
import * as React from "react";
import dividepanel from "ui_comp/errorframe.png";
import okbutton from "ui_comp/okbutton.png";
import InventoryItem from "./inventoryItem";
import leftbutton from "ui_comp/leftbutton.png";
import rightbutton from "ui_comp/rightbutton.png";
import { Network } from "../network";
import { Global } from "../global";
import { GameEvent } from "../gameEvent";

interface ItemDivideProps {
  itemData,
}

export default class ItemDividePanel extends React.Component<ItemDivideProps, any> {
  constructor(props) {
    super(props);

    const leftItem = Object.assign({}, this.props.itemData);
    var rightItem = Object.assign({}, this.props.itemData);
    rightItem.quantity = 0;

    this.state = {
      leftItem : leftItem,
      rightItem : rightItem
    };
   
    this.handleOkClick = this.handleOkClick.bind(this);
    this.handleLeftClick = this.handleLeftClick.bind(this);
    this.handleRightClick = this.handleRightClick.bind(this);
  }

  handleLeftClick(event) { 
    this.state.leftItem.quantity = this.state.leftItem.quantity + 1;
    this.state.rightItem.quantity = this.state.rightItem.quantity - 1;

    this.setState({leftItem: this.state.leftItem,
                   rightItem: this.state.rightItem});
  }

  handleRightClick(event) {
    this.state.leftItem.quantity = this.state.leftItem.quantity - 1;
    this.state.rightItem.quantity = this.state.rightItem.quantity + 1;

    this.setState({leftItem: this.state.leftItem,
                   rightItem: this.state.rightItem});
  }

  handleOkClick() {
    Network.sendItemSplit(this.state.rightItem.id, 
                          this.state.rightItem.quantity);
    Global.gameEmitter.emit(GameEvent.ITEM_DIVIDE_OK_CLICK, {});
  }

  render() {
    const divideStyle = {
      top: '50%',
      left: '50%',
      width: '333px',
      height: '119px',
      marginTop: '-59px',
      marginLeft: '-166px',
      position: 'fixed',
      zIndex: 20
    } as React.CSSProperties

    const dividePanelStyle = {
      position: 'fixed'
    } as React.CSSProperties

    const spanNameStyle = {
      transform: 'translate(15px, 20px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '14px',
      width: '300px'
    } as React.CSSProperties

    const leftStyle = {
      transform: 'translate(50px, 30px)',
      position: 'fixed'
    } as React.CSSProperties

    const rightStyle = {
      transform: 'translate(235px, 30px)',
      position: 'fixed'
    } as React.CSSProperties

    const okButtonStyle = {
      transform: 'translate(141px, 90px)',
      position: 'fixed'
    } as React.CSSProperties

    return (
      <div style={divideStyle}>
        <img src={dividepanel} style={dividePanelStyle}/>
        <InventoryItem key={'left'}
                       ownerId={this.state.leftItem.owner}
                       itemId={this.state.leftItem.id} 
                       itemName={this.state.leftItem.name}
                       image={this.state.leftItem.image} 
                       quantity={this.state.leftItem.quantity}
                       xPos={110}
                       yPos={30} />

        <InventoryItem key={'right'}
                       ownerId={this.state.rightItem.owner}
                       itemId={this.state.rightItem.id} 
                       itemName={this.state.rightItem.name} 
                       image={this.state.rightItem.image} 
                       quantity={this.state.rightItem.quantity}
                       xPos={175}
                       yPos={30} />  

        <img src={leftbutton} style={leftStyle} onClick={this.handleLeftClick} />
        <img src={rightbutton} style={rightStyle} onClick={this.handleRightClick} />
        <img src={okbutton} style={okButtonStyle} onClick={this.handleOkClick}/>
      </div>
    );
  }
}

