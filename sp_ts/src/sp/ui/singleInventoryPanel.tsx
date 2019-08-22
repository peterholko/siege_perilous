

import * as React from "react";
import InventoryPanel from "./baseInventoryPanel";
import { Network } from "../network";


export default class SingleInventoryPanel extends React.Component<any, any> {
  constructor(props) {
    super(props);

    this.state = {
      hideSelect: true
    };
  
    this.handleSelect = this.handleSelect.bind(this);
  }

  handleSelect(eventData) {
    this.setState({hideSelect: false});

    Network.sendInfoItem(eventData.itemId);
  }

  render() {

    return (
      <div>
        <InventoryPanel left={true} 
                        inventoryData={this.props.inventoryData}
                        panelType={'inventory'} 
                        hideExitButton={false}
                        hideSelect={this.state.hideSelect}
                        handleSelect={this.handleSelect} />

      </div>
    );
  }
}

