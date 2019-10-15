
import * as React from "react";
import styles from "./../ui.css";
import { Global } from "../global";
import { GameEvent } from "../gameEvent";

interface ResItemProps {
  resourceName,
  quantity,
  index
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
    //var quantityStr = this.formatQuantity(this.props.quantity);

    let xPos = this.props.index * 50 + 15;

    //31px -286px
    const divStyle = {
      transform: 'translate(' + xPos +'px,  0px)',
      position: 'fixed'
    } as React.CSSProperties

    const itemStyle = {
      transform: 'translate(0px, 0px)',
      position: 'fixed'
    } as React.CSSProperties

    const imageName = this.props.resourceName.replace(/\s/g, '').toLowerCase();

    return (
      <div style={divStyle}>
        <img src={'/static/art/items/' + imageName + '.png'}
            style={itemStyle}
            onClick={this.handleClick}/>
      </div>
    );
  }
}


        //<span id="itemquantity" className={styles.itemquantity}>{quantityStr}</span>