
/*import * as React from "react";
import styles from "./../ui.css";
import { Global } from "../global";
import { GameEvent } from "../gameEvent";

interface TileResProps {
  resourceName,
  yieldLabel,
  quantityLabel,
  currentQuantity?,
  index,
  showQuantity,
  fixedPos?,
  xPos?,
  yPos?
}

export default class TileResource extends React.Component<TileResProps, any> {
  constructor(props) {
    super(props);

  }


  render() {    
    var xPos = this.props.index * 55;
    var yPos = 0;
    let fixedPos = this.props.fixedPos != null ? 'static' : 'fixed';

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
      <div style={divStyle}>
        <img src={'/static/art/items/' + imageName + '.png'}
            style={itemStyle} />
          <span>Yield: {this.props.yieldLabel}</span>
          <span>Quantity: {this.props.quantityLabel}</span>
      </div>
    );
  }
}*/

