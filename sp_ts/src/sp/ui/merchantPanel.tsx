
import * as React from "react";
import BaseInventoryPanel from "./baseInventoryPanel";
import { Global } from "../global";
import { GameEvent } from "../gameEvent";
import { Network } from "../network";

interface MPProps {
  leftInventoryData,
  rightInventoryData
}

export default class MerchantPanel extends React.Component<MPProps, any> {
  constructor(props) {
    super(props);

    Global.selectedItemId = -1;
    Global.selectedItemOwnerId = -1;
    Global.merchantSellTarget = this.props.rightInventoryData.id;

    this.state = {
      hideLeftSelect: true,
      hideRightSelect: true
    };
  
    this.handleSelect = this.handleSelect.bind(this);
  }

  handleSelect() {
    if(Global.selectedItemOwnerId == this.props.leftInventoryData.id) {
      this.setState({hideLeftSelect: false,
                     hideRightSelect: true});
    
      Global.infoItemAction = 'merchantsell';
    } else {
      this.setState({hideLeftSelect: true,
                     hideRightSelect: false});
      Global.infoItemAction = 'merchantbuy';
    }

    Network.sendInfoItem(Global.selectedItemId);
  }

  /*handleItemTransferClick(event : React.MouseEvent) {
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
  }*/

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
                            panelType={'merchant'}
                            hideExitButton={true}
                            hideSelect={this.state.hideLeftSelect}
                            handleSelect={this.handleSelect} />

        <BaseInventoryPanel left={false} 
                            inventoryData={this.props.rightInventoryData} 
                            panelType={'merchant'}
                            hideExitButton={false}
                            hideSelect={this.state.hideRightSelect}
                            handleSelect={this.handleSelect} />

      </div>
    );
  }
}

