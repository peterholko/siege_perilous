
import * as React from "react";
import transferbutton from "ui_comp/transferbutton.png";
import BaseInventoryPanel from "./baseInventoryPanel";
import { Global } from "../global";
import { GameEvent } from "../gameEvent";
import { Network } from "../network";
import { STRUCTURE, FOUNDED } from "../config";
import FoundedInventoryPanel from "./foundedInventoryPanel";
import SmallButton from "./smallButton";

interface ITPProps {
  leftInventoryData,
  rightInventoryData,
  reqs
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
    if (Global.selectedItemOwnerId == this.props.leftInventoryData.id) {
      this.setState({
        hideLeftSelect: false,
        hideRightSelect: true
      });
    } else {
      this.setState({
        hideLeftSelect: true,
        hideRightSelect: false
      });
    }
  }

  handleItemTransferClick(event: React.MouseEvent) {
    console.log('Item Transfer Click');
    if (Global.selectedItemId != -1) {
      var targetId;

      if (Global.selectedItemOwnerId == this.props.leftInventoryData.id) {
        targetId = this.props.rightInventoryData.id;
      } else {
        targetId = this.props.leftInventoryData.id;
      }
      this.setState({
        hideLeftSelect: true,
        hideRightSelect: true
      });

      Network.sendItemTransfer(targetId, Global.selectedItemId);

      //Reset Global selected item / owner
      Global.selectedItemId = -1;
      Global.selectedItemOwnerId = -1;
      Global.selectedItemName = '';
    }
  }

  render() {
    console.log("Left: ");
    console.log(this.props.leftInventoryData);
    console.log("Right: ");
    console.log(this.props.rightInventoryData);
    var objState = Global.objectStates[this.props.rightInventoryData.id];
    var isFounded = objState.class == STRUCTURE && objState.state == FOUNDED;

    const transferStyle = {
      top: '50%',
      left: '50%',
      marginTop: '-25px',
      marginLeft: '-25px',
      position: 'fixed',
      zIndex: 7
    } as React.CSSProperties

    const leftItemNameStyle = {
      transform: 'translate(-323px, 0px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '323px',
      zIndex: 6,
      top: '50%',
      left: '50%',
      marginTop: '125px'
    } as React.CSSProperties

    const rightItemNameStyle = {
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '323px',
      zIndex: 6,
      top: '50%',
      left: '50%',
      marginTop: '125px'
    } as React.CSSProperties

    return (
      <div>
        <BaseInventoryPanel left={true}
          id={this.props.leftInventoryData.id}
          items={this.props.leftInventoryData.items}
          capacity={this.props.leftInventoryData.cap}
          totalWeight={this.props.leftInventoryData.tw}
          panelType={'itemTransfer'}
          hideExitButton={true}
          hideSelect={this.state.hideLeftSelect}
          handleSelect={this.handleSelect} />
        {!isFounded &&
          <BaseInventoryPanel left={false}
            id={this.props.rightInventoryData.id}
            items={this.props.rightInventoryData.items}
            capacity={this.props.rightInventoryData.cap}
            totalWeight={this.props.rightInventoryData.tw}
            panelType={'itemTransfer'}
            hideExitButton={false}
            hideSelect={this.state.hideRightSelect}
            handleSelect={this.handleSelect} />}
        {isFounded &&
          <FoundedInventoryPanel id={this.props.rightInventoryData.id}
            items={this.props.rightInventoryData.items}
            reqs={this.props.reqs}
            panelType={'itemTransfer'}
            hideExitButton={false}
            hideSelect={this.state.hideRightSelect}
            handleSelect={this.handleSelect} />}

        {!this.state.hideLeftSelect &&
          <span style={leftItemNameStyle}>{Global.selectedItemName}</span>}

        {!this.state.hideRightSelect &&
          <span style={rightItemNameStyle}>{Global.selectedItemName}</span>}

        <SmallButton handler={this.handleItemTransferClick}
          imageName="transferbutton"
          style={transferStyle} />

      </div>
    );
  }
}

