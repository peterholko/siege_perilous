
import * as React from "react";
import styles from "./../ui.css";

interface InvItemProps {
  ownerId,
  itemName,
  itemId,
  image,
  quantity,
  xPos,
  yPos,
  index?,
  handleSelect?
}

export default class InventoryItem extends React.Component<InvItemProps, any> {
  constructor(props) {
    super(props);

    this.handleClick = this.handleClick.bind(this)
  }

  handleClick = () => {
    const eventData = {
      ownerId: this.props.ownerId,
      itemId: this.props.itemId,
      itemName: this.props.itemName,
      index: this.props.index,
    }
    this.props.handleSelect(eventData)
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
    var quantityStr = this.formatQuantity(this.props.quantity);

    //31px -286px
    const divStyle = {
      transform: 'translate(' + this.props.xPos + 'px, ' + this.props.yPos + 'px)',
      position: 'fixed'
    } as React.CSSProperties

    const itemStyle = {
      transform: 'translate(0px, 0px)',
      position: 'fixed'
    } as React.CSSProperties


    return (
      <div style={divStyle} onClick={this.props.handleSelect != null ? this.handleClick : null}>
        <img src={'/static/art/items/' + this.props.image + '.png'}
            style={itemStyle}/>
        <span id="itemquantity" className={styles.itemquantity}>{quantityStr}</span>
      </div>
    );
  }
}

