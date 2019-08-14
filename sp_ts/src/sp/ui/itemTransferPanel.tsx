
import * as React from "react";
import transferbutton from "ui_comp/transferbutton.png";
import InventoryPanel from "./inventoryPanel";
import { Global } from "../global";
import { GameEvent } from "../gameEvent";
import { Network } from "../network";

interface ITPProps {
  leftInventoryData,
  rightInventoryData
}

export default class ItemTransferPanel extends React.Component<ITPProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };
    
    this.handleItemTransferClick = this.handleItemTransferClick.bind(this);
  }

  handleItemTransferClick(event : React.MouseEvent) {
    console.log('Item Transfer Click');
    var targetId;

    if(Global.selectedItemOwnerId == this.props.leftInventoryData.id) {
      targetId = this.props.rightInventoryData.id;
    } else {
      targetId = this.props.leftInventoryData.id;
    }

    Network.sendItemTransfer(targetId, Global.selectedItemId);
  }

  render() {
    const transferStyle = {
      top: '50%',
      left: '50%',
      marginTop: '-25px',
      marginLeft: '-25px',
      position: 'fixed',
      zIndex: 7
    } as React.CSSProperties

    return (
      <div>
        <InventoryPanel left={true} 
                        inventoryData={this.props.leftInventoryData} 
                        hideExitButton={true}
                        panelType={'itemTransfer'}/>

        <InventoryPanel left={false} 
                        inventoryData={this.props.rightInventoryData} 
                        hideExitButton={false}
                        panelType={'itemTransfer'}/>

        {<img src={transferbutton} 
              style={transferStyle} 
              onClick={this.handleItemTransferClick}/> }
      </div>
    );
  }
}

