
import * as React from "react";
import transferbutton from "ui_comp/transferbutton.png";
import BaseInventoryPanel from "./baseInventoryPanel";
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

    Global.selectedItemId = -1;
    Global.selectedItemOwnerId = -1;

    this.state = {
      hideLeftSelect: true,
      hideRightSelect: true
    };
  
    this.handleSelect = this.handleSelect.bind(this);
    this.handleItemTransferClick = this.handleItemTransferClick.bind(this);
  }

  handleSelect() {
    if(Global.selectedItemOwnerId == this.props.leftInventoryData.id) {
      this.setState({hideLeftSelect: false,
                     hideRightSelect: true});
    } else {
      this.setState({hideLeftSelect: true,
                     hideRightSelect: false});
    }
  }

  handleItemTransferClick(event : React.MouseEvent) {
    console.log('Item Transfer Click');
    if(Global.selectedItemId != -1) {
      var targetId;

      if(Global.selectedItemOwnerId == this.props.leftInventoryData.id) {
        targetId = this.props.rightInventoryData.id;
      } else {
        targetId = this.props.leftInventoryData.id;
      }
      this.setState({hideLeftSelect: true,
                    hideRightSelect: true});

      Network.sendItemTransfer(targetId, Global.selectedItemId);
      
      //Reset Global selected item / owner
      Global.selectedItemId = -1;
      Global.selectedItemOwnerId = -1;
    }
  }

  render() {

    console.log('this.props.leftInventoryData.id: ' + this.props.leftInventoryData.id);
    console.log('this.props.rightInventoryData.id: ' + this.props.rightInventoryData.id);

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
        <BaseInventoryPanel left={true} 
                            inventoryData={this.props.leftInventoryData} 
                            panelType={'itemTransfer'}
                            hideExitButton={true}
                            hideSelect={this.state.hideLeftSelect}
                            handleSelect={this.handleSelect} />

        <BaseInventoryPanel left={false} 
                            inventoryData={this.props.rightInventoryData} 
                            panelType={'itemTransfer'}
                            hideExitButton={false}
                            hideSelect={this.state.hideRightSelect}
                            handleSelect={this.handleSelect} />

        {<img src={transferbutton} 
              style={transferStyle} 
              onClick={this.handleItemTransferClick}/> }
      </div>
    );
  }
}

