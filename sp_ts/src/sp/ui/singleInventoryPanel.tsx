

import * as React from "react";
import BaseInventoryPanel from "./baseInventoryPanel";
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
        <BaseInventoryPanel left={true} 
                        inventoryData={this.props.inventoryData}
                        panelType={'inventory'} 
                        hideExitButton={false}
                        hideSelect={this.state.hideSelect}
                        handleSelect={this.handleSelect} />

      </div>
    );
  }
}

