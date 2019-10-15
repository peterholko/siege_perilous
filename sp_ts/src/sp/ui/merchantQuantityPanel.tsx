
import * as React from "react";
import merchantquantitypanel from "ui_comp/errorframe.png";
import buybutton from "ui_comp/buybutton.png";
import sellbutton from "ui_comp/sellbutton.png";
import InventoryItem from "./inventoryItem";
import leftbutton from "ui_comp/leftbutton.png";
import rightbutton from "ui_comp/rightbutton.png";
import cancelbutton from "ui_comp/exitbutton.png";
import { Network } from "../network";
import { Global } from "../global";
import { GameEvent } from "../gameEvent";

interface MQPProps {
  itemData,
  action,
  targetId?
}

export default class MerchantQuantityPanel extends React.Component<MQPProps, any> {
  constructor(props) {
    super(props);

    let item = Object.assign({}, this.props.itemData);

    this.state = {
      item : item,
    };
   
    this.handleBuyClick = this.handleBuyClick.bind(this);
    this.handleSellClick = this.handleSellClick.bind(this);
    this.handleLeftClick = this.handleLeftClick.bind(this);
    this.handleRightClick = this.handleRightClick.bind(this);
    this.handleCancelClick = this.handleCancelClick.bind(this);
  }

  handleLeftClick() {
    this.state.item.quantity = this.state.item.quantity - 1;
    this.setState({item: this.state.item});
  }

  handleRightClick() {
    this.state.item.quantity = this.state.item.quantity + 1;
    this.setState({item: this.state.item});
  }

  handleBuyClick() {
    Network.sendBuyItem(this.props.itemData.id, this.state.item.quantity);
    Global.gameEmitter.emit(GameEvent.MERCHANT_QUANTITY_CANCEL, {});
  }

  handleSellClick() {
    Network.sendSellItem(this.props.itemData.id, Global.merchantSellTarget, this.state.item.quantity);
    Global.gameEmitter.emit(GameEvent.MERCHANT_QUANTITY_CANCEL, {});
  }

  handleCancelClick() {
    Global.gameEmitter.emit(GameEvent.MERCHANT_QUANTITY_CANCEL, {});
  }

  render() {
    const hideLeft = this.state.item.quantity == 1;
    const hideRight = this.state.item.quantity == this.props.itemData.quantity;

    const merchantStyle = {
      top: '50%',
      left: '50%',
      width: '333px',
      height: '119px',
      marginTop: '-59px',
      marginLeft: '-166px',
      position: 'fixed',
      zIndex: 20
    } as React.CSSProperties

    const merchantPanelStyle = {
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

    const buySellButtonStyle = {
      transform: 'translate(116px, 90px)',
      position: 'fixed'
    } as React.CSSProperties

    const cancelButtonStyle = {
      transform: 'translate(166px, 90px)',
      position: 'fixed'
    } as React.CSSProperties

    return (
      <div style={merchantStyle}>
        <img src={merchantquantitypanel} style={merchantPanelStyle}/>
        <InventoryItem key={'item'}
                       ownerId={this.state.item.owner}
                       itemId={this.state.item.id} 
                       itemName={this.state.item.name} 
                       quantity={this.state.item.quantity}
                       xPos={140}
                       yPos={30} />

        {!hideLeft && <img src={leftbutton} style={leftStyle} onClick={this.handleLeftClick} />}
        {!hideRight && <img src={rightbutton} style={rightStyle} onClick={this.handleRightClick} />}

        <img src={cancelbutton} style={cancelButtonStyle} onClick={this.handleCancelClick} />

        {(this.props.action == 'buy') &&
        <img src={buybutton} style={buySellButtonStyle} onClick={this.handleBuyClick}/> }

        {(this.props.action == 'sell') &&
        <img src={sellbutton} style={buySellButtonStyle} onClick={this.handleSellClick}/> }
      </div>
    );
  }
}

