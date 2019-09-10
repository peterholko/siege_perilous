
import * as React from "react";
import selectbox from "ui_comp/selectbox.png";
import { Global } from "../global";
import { GameEvent } from "../gameEvent";

interface InvItemProps {
  ownerId,
  itemName,
  itemId,
  index,
  quantity,
  handleSelect
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

  render() {
    var xPos = 31 + ((this.props.index % 5) * 53);
    var yPos = -286 + (Math.floor(this.props.index / 5) * 53);

    //31px -286px
    const divStyle = {
      transform: 'translate(' + xPos + 'px, ' + yPos + 'px)',
      position: 'fixed'
    } as React.CSSProperties

    const itemStyle = {
      transform: 'translate(0px, 0px)',
      position: 'fixed'
    } as React.CSSProperties

    const spanStyle = {
      transform: 'translate(-3px, 35px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '50px',
      textAlign: 'right'
    } as React.CSSProperties

    const imageName = this.props.itemName.replace(/\s/g, '').toLowerCase();

    return (
      <div style={divStyle}>
        <img src={'/static/art/' + imageName + '.png'}
            style={itemStyle}
            onClick={this.handleClick}/>
        <span style={spanStyle}>{this.props.quantity}</span>
      </div>
    );
  }
}

