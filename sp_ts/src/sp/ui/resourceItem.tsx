
import * as React from "react";
import styles from "./../ui.css";
import { Global } from "../global";
import { GameEvent } from "../gameEvent";

interface ResItemProps {
  resourceName,
  quantity,
  currentQuantity?,
  index,
  showQuantity,
  fixedPos?,
  xPos?,
  yPos?
}

export default class ResourceItem extends React.Component<ResItemProps, any> {
  constructor(props) {
    super(props);

    this.handleClick = this.handleClick.bind(this)
  }

  handleClick = () => {
    const eventData = {
      name: this.props.resourceName,
      quantity: this.props.quantity,
      index: this.props.index,
    }

    Global.gameEmitter.emit(GameEvent.RESOURCE_CLICK, eventData);
  }

  formatQuantity(quantity) {
    if(quantity > 1000000) {
      return (quantity / 1000000).toFixed(2) + 'M';
    } else if(quantity > 1000) {
      return (quantity / 1000).toFixed(2) + 'K';
    } else {
      return quantity;
    }
  }

  render() {    
    var xPos = this.props.index * 55;
    var yPos = 0;
    var formattedQuantity = this.formatQuantity(this.props.quantity);
    var quantityStr = formattedQuantity;
    let fixedPos = this.props.fixedPos != null ? 'static' : 'fixed';

    if(this.props.currentQuantity != null) {
      var currentQuantity = this.formatQuantity(this.props.currentQuantity)
      quantityStr = currentQuantity + '/' + formattedQuantity;
    }

    if(this.props.xPos != null) {
      xPos = this.props.xPos;
      yPos = this.props.yPos;
    }
    
    //31px -286px
    const divStyle = {
      transform: 'translate(' + xPos + 'px,  ' + yPos + 'px)',
      position: fixedPos
    } as React.CSSProperties

    const itemStyle = {
      transform: 'translate(0px, 0px)',
      position: 'fixed'
    } as React.CSSProperties

    const imageName = this.props.resourceName.replace(/\s/g, '').toLowerCase();

    return (
      <div style={divStyle} onClick={this.handleClick}>
        <img src={'/static/art/items/' + imageName + '.png'}
            style={itemStyle} />
        {this.props.showQuantity &&
          <span id="itemquantity" className={styles.itemquantity}>{quantityStr}</span>}
      </div>
    );
  }
}

