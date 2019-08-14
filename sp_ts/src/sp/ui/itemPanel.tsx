
import * as React from "react";
import HalfPanel from "./halfPanel";
import InventoryItem from "./inventoryItem";
import { Global } from "../global";
import { GameEvent } from "../gameEvent";

import itemframe from "ui_comp/itemframe.png";
import selectitemborder from "ui_comp/selectitemborder.png";
import { Util } from "../util";

interface ItemPanelProps {
  itemData,
}

export default class ItemPanel extends React.Component<ItemPanelProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };
    
  }

  render() {
    const itemName = this.props.itemData.name;
    const imageName = itemName.toLowerCase().replace(/ /g, '') + '.png'

    const itemStyle = {
      transform: 'translate(-185px, 25px)',
      position: 'fixed'
    } as React.CSSProperties

    const tableStyle = {
      transform: 'translate(20px, -250px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px'
    } as React.CSSProperties

    return (
      <HalfPanel left={false} 
                 panelType={'item'} 
                 hideExitButton={false}>
        <img src={'/static/art/' + imageName} style={itemStyle} />
        <table style={tableStyle}>
          <tbody>
          <tr>
            <td>Name: </td>
            <td>{this.props.itemData.name}</td>
          </tr>
          <tr>
            <td>Class: </td>
            <td>{this.props.itemData.class}</td>
          </tr>
          <tr>
            <td>Subclass: </td>
            <td>{this.props.itemData.subclass}</td>
          </tr>
          </tbody>
        </table>
      </HalfPanel>
    );
  }
}

