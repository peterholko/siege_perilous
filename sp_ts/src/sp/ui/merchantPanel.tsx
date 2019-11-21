
import * as React from "react";
import BaseInventoryPanel from "./baseInventoryPanel";
import { Global } from "../global";
import { GameEvent } from "../gameEvent";
import { Network } from "../network";
import hirebutton from "ui_comp/assignbutton.png";

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
    this.handleInfoHireClick = this.handleInfoHireClick.bind(this);
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

  handleInfoHireClick() {
    Network.sendInfoHauling(Global.merchantSellTarget);
  }

  render() {
    console.log('this.props.leftInventoryData.id: ' + this.props.leftInventoryData.id);
    console.log('this.props.rightInventoryData.id: ' + this.props.rightInventoryData.id);

    const hireStyle = {
      top: '50%',
      left: '50%',
      marginTop: '-25px',
      marginLeft: '-25px',
      position: 'fixed',
      transform: 'translate(161px, 135px)',
      zIndex: 6
    } as React.CSSProperties

    return (
      <div>
        <BaseInventoryPanel left={true}
                            id={this.props.leftInventoryData.id} 
                            items={this.props.leftInventoryData.items} 
                            panelType={'merchant'}
                            hideExitButton={true}
                            hideSelect={this.state.hideLeftSelect}
                            handleSelect={this.handleSelect} />

        <BaseInventoryPanel left={false} 
                            id={this.props.rightInventoryData.id}
                            items={this.props.rightInventoryData.items} 
                            panelType={'merchant'}
                            hideExitButton={false}
                            hideSelect={this.state.hideRightSelect}
                            handleSelect={this.handleSelect} />

      <img src={hirebutton}
               style={hireStyle}
               onClick={this.handleInfoHireClick} />}
      </div>
    );
  }
}

