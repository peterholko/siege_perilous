
import * as React from "react";
import HalfPanel from "./halfPanel";
import dividebutton from "ui_comp/dividebutton.png";
import buybutton from "ui_comp/buybutton.png";
import sellbutton from "ui_comp/sellbutton.png";
import equipbutton from "ui_comp/equipbutton.png";
import usebutton from "ui_comp/usebutton.png";
import { GameEvent } from "../gameEvent";
import { Global } from "../global";
import { TRIGGER_MERCHANT_BUY, TRIGGER_INVENTORY, TRIGGER_MERCHANT_SELL, FALSE } from "../config";
import { Network } from "../network";

interface ItemPanelProps {
  triggerAction,
  itemData,
}

export default class ItemPanel extends React.Component<ItemPanelProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };
    
    this.handleDivideClick = this.handleDivideClick.bind(this);
    this.handleBuyClick = this.handleBuyClick.bind(this);
    this.handleSellClick = this.handleSellClick.bind(this);
    this.handleEquipClick = this.handleEquipClick.bind(this);
    this.handleUseClick = this.handleUseClick.bind(this);
  }

  handleDivideClick() {
    Global.gameEmitter.emit(GameEvent.ITEM_DIVIDE_CLICK, this.props.itemData);
  }

  handleBuyClick() {
    const eventData = {'itemData': this.props.itemData,
                       'action': 'buy'};

    Global.gameEmitter.emit(GameEvent.MERCHANT_BUYSELL_CLICK, eventData);
  }

  handleSellClick() {
    //Network.sendSellItem(this.props.itemData.id, this.props.itemData.quantity);
    const eventData = {'itemData': this.props.itemData,
                       'action': 'sell'};

    Global.gameEmitter.emit(GameEvent.MERCHANT_BUYSELL_CLICK, eventData);
  }

  handleEquipClick() {
    if(this.props.itemData.equipped == false) {
      Network.sendEquip(this.props.itemData.id, true);
    } else {
      Network.sendEquip(this.props.itemData.id, false);
    }
  }
  
  handleUseClick() {
    Network.sendUse(this.props.itemData.id);
  }

  render() {
    const itemName = this.props.itemData.name;
    const imageName = this.props.itemData.image + '.png'
    const effects = [];
    var produces = '';

    const showDivideButton = (this.props.itemData.quantity > 1) && 
                             (this.props.triggerAction == TRIGGER_INVENTORY)

    const showBuyButton = (this.props.triggerAction == TRIGGER_MERCHANT_BUY);
    const showSellButton = (this.props.triggerAction == TRIGGER_MERCHANT_SELL);

    const isLeftPanel = (this.props.triggerAction == TRIGGER_MERCHANT_BUY);

    const showEquipButton = (this.props.itemData.class == "Weapon") || 
                            (this.props.itemData.class == "Armor");

    const showUseButton = (this.props.itemData.class == "Potion") ||
                          (this.props.itemData.class == "Deed");

    const showPrice = this.props.itemData.hasOwnProperty('price');

    if(this.props.itemData.hasOwnProperty('effects')) {

      for(var i = 0; i < this.props.itemData.effects.length; i++) {
        var effect = this.props.itemData.effects[i];
        var type = ''
        var value = ''

        if(effect.type.indexOf('%') != -1) {
          type = effect.type.replace('%', '');

          if(effect.value > 0) {
            value = type + '+' + (effect.value * 100)+ '%';
          } else {
            value = type + (effect.value * 100)+ '%';
          }

        } else {
          value = type + effect.value;     
        }

        effects.push(<tr key={i}>
          <td>{value}</td>
        </tr>)
      }
    }

    if(this.props.itemData.hasOwnProperty('produces')) {
      produces = this.props.itemData.produces.join();
    }

    const itemStyle = {
      transform: 'translate(-185px, 25px)',
      position: 'fixed'
    } as React.CSSProperties

    const spanNameStyle = {
      transform: 'translate(-323px, 85px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '323px'
    } as React.CSSProperties

    const tableStyle = {
      transform: 'translate(20px, -250px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '300px'
    } as React.CSSProperties

    const tableStyle2 = {
      transform: 'translate(-50px, 15px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px'
    } as React.CSSProperties

    const divideStyle = {
      transform: 'translate(-187px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const buyStyle = {
      transform: 'translate(-187px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const sellStyle = {
      transform: 'translate(-187px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const equipStyle = {
      transform: 'translate(-137px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const useStyle = {
      transform: 'translate(-137px, 295px)',
      position: 'fixed'
    } as React.CSSProperties


    return (
      <HalfPanel left={isLeftPanel} 
                 panelType={'item'} 
                 hideExitButton={false}>
        <img src={'/static/art/items/' + imageName} style={itemStyle} />
        <span style={spanNameStyle}>
          {itemName} x {this.props.itemData.quantity}
        </span>
        <table style={tableStyle}>
          <tbody>
          <tr>
            <td>Equipped: </td>
            <td>{String(this.props.itemData.equipped)}</td>
          </tr>
          <tr>
            <td>Class: </td>
            <td>{this.props.itemData.class}</td>
          </tr>
          <tr>
            <td>Subclass: </td>
            <td>{this.props.itemData.subclass}</td>
          </tr>
          <tr>
            <td>Weight: </td>
            <td>
              {this.props.itemData.weight} per unit 
              ({this.props.itemData.quantity * this.props.itemData.weight})
            </td>
          </tr>
          <tr>
            <td>Produces: </td>
            <td >{produces}</td>
          </tr>
          <tr>
            <td>Effects: </td>
            <td>
              <table style={tableStyle2}>
                <tbody>
                  {effects}
                </tbody>
              </table>
            </td>
          </tr>
          {showPrice &&
            <tr>
              <td>Price: </td>
              <td>{this.props.itemData.price}</td>
            </tr> 
          }
          </tbody>
        </table>

        {showDivideButton && 
          <img src={dividebutton}
               style={divideStyle}
               onClick={this.handleDivideClick} />}

        {showBuyButton && 
          <img src={buybutton}
               style={buyStyle}
               onClick={this.handleBuyClick} />}

        {showSellButton && 
          <img src={sellbutton}
               style={sellStyle}
               onClick={this.handleSellClick} />}
 
        {showEquipButton && 
          <img src={equipbutton}
               style={equipStyle}
               onClick={this.handleEquipClick} />}
 
         {showUseButton && 
          <img src={usebutton}
               style={useStyle}
               onClick={this.handleUseClick} />}
 
      </HalfPanel>
    );
  }
}

