
import * as React from "react";
import selectbox from "ui_comp/selectbox.png";
import { Global } from "../global";
import { GameEvent } from "../gameEvent";

interface InvItemProps {
  ownerId,
  itemName,
  itemId,
  xPos,
  yPos,
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
      xPos: this.props.xPos,
      yPos: this.props.yPos
    }
    this.props.handleSelect(eventData)
  }

  render() {
    const itemStyle = {
      transform: 'translate(' + this.props.xPos + 'px, ' + this.props.yPos + 'px)',
      position: 'fixed'
    } as React.CSSProperties

    const imageName = this.props.itemName.replace(/\s/g, '').toLowerCase();

    return (
      <img src={'/static/art/' + imageName + '.png'}
           style={itemStyle}
           onClick={this.handleClick}/>
    );
  }
}

