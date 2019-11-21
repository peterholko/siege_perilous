

import * as React from "react";
import BaseInventoryPanel from "./baseInventoryPanel";
import { Network } from "../network";
import { Global } from "../global";


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

    Global.infoItemAction = 'inventory';
    Network.sendInfoItem(eventData.itemId);
  }

  render() {

    return (
      <div>
        <BaseInventoryPanel left={true}
                        id={this.props.inventoryData.id} 
                        items={this.props.inventoryData.items}
                        panelType={'inventory'} 
                        hideExitButton={false}
                        hideSelect={this.state.hideSelect}
                        handleSelect={this.handleSelect} />

      </div>
    );
  }
}

