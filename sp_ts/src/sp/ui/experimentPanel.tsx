
import * as React from "react";
import transferbutton from "ui_comp/transferbutton.png";
import BaseInventoryPanel from "./baseInventoryPanel";
import { Global } from "../global";
import HalfPanel from "./halfPanel";
import itemframe from "ui_comp/itemframe.png";
import experimentbutton from "ui_comp/experimentbutton.png";
import { exportAllDeclaration } from "@babel/types";
import { Network } from "../network";
import InventoryItem from "./inventoryItem";

interface ETPProps {
  expData
}

export default class ExperimentTransferPanel extends React.Component<ETPProps, any> {
  constructor(props) {
    super(props);

    Global.selectedItemId = -1;
    Global.selectedItemOwnerId = -1;

    this.state = {
      hideLeftSelect: true,
      hideRightSelect: true
    };
  
    this.handleSelect = this.handleSelect.bind(this);
    this.handleSetExpItemClick = this.handleSetExpItemClick.bind(this);
    this.handleSetExpResourceClick = this.handleSetExpResourceClick.bind(this);
    this.handleExperimentClick = this.handleExperimentClick.bind(this);
  }

  handleSelect() {
    this.setState({hideLeftSelect: false});
  }

  handleSetExpItemClick() {
    console.log('Set Experiment Item Click');
    Network.sendSetExpItem(Global.selectedItemId);
  }

  handleSetExpResourceClick() {
    console.log('Set Experiment Item Click');
    Network.sendSetExpResource(Global.selectedItemId);
  }

  handleExperimentClick() {
    Network.sendOrderExperiment(this.props.expData.id);
  }

  render() {
    var itemExperiment = []; //Only one item possible 
    var itemFrameResources = [];
    var itemExpResources = [];

    const sourceTransferStyle = {
      transform: 'translate(-250px, 75px)',
      position: 'fixed'
    } as React.CSSProperties

    const reagentsTransferStyle = {
      transform: 'translate(-280px, 175px)',
      position: 'fixed'
    } as React.CSSProperties 

    const sourceStyle = {
      transform: 'translate(-323px, 50px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '323px'
    } as React.CSSProperties

    const reagentsStyle = {
      transform: 'translate(-323px, 150px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '323px'
    } as React.CSSProperties

    const expItemStyle = {
      transform: 'translate(-185px, 75px)',
      position: 'fixed'
    } as React.CSSProperties

    const expButtonStyle = {
      transform: 'translate(-185px, 290px)',
      position: 'fixed'
    } as React.CSSProperties

    if(this.props.expData.expitem.length > 0) {
      var xPos = 138;
      var yPos = -285;

      var itemId = this.props.expData.expitem[0].id;
      var itemName = this.props.expData.expitem[0].name;
      var quantity = this.props.expData.expitem[0].quantity;

      itemExperiment.push(
         <InventoryItem key={i}
                       ownerId={this.props.expData.id}
                       itemId={itemId} 
                       itemName={itemName} 
                       quantity={quantity}
                       index={i}
                       xPos={xPos}
                       yPos={yPos}
                       handleSelect={this.handleSelect} />
      )
    }

    for(var i = 0; i < 2; i++) {
      var xPos = i * 60 - 215;
      var yPos = 175;

      var itemFrameResource = {
        transform: 'translate(' + xPos + 'px, ' + yPos + 'px',
        position: 'fixed'
      } as React.CSSProperties

      itemFrameResources.push(
        <img src={itemframe} key={i} style={itemFrameResource} /> 
      )
    }

    for(var i = 0; i < this.props.expData.expresources.length; i++) {
      var xPos = i * 60 + 109;
      var yPos = -187;

      var itemId = this.props.expData.expresources[i].id;
      var itemName = this.props.expData.expresources[i].name;
      var quantity = this.props.expData.expresources[i].quantity;

      itemExpResources.push(
        <InventoryItem key={i}
                       ownerId={this.props.expData.id}
                       itemId={itemId} 
                       itemName={itemName} 
                       quantity={quantity}
                       index={i}
                       xPos={xPos}
                       yPos={yPos}
                       handleSelect={this.handleSelect} />
      );
    }

    return (
      <div>
        <BaseInventoryPanel left={true} 
                            id={this.props.expData.id}
                            items={this.props.expData.validresources}
                            panelType={'experiment'}
                            hideExitButton={true}
                            hideSelect={this.state.hideLeftSelect}
                            handleSelect={this.handleSelect} />

        <HalfPanel left={false} 
                 panelType={'experiment'} 
                 hideExitButton={false}>
          
          <span style={sourceStyle}>Source</span>
          <img src={itemframe} style={expItemStyle}/>

          {itemExperiment}

          <img src={transferbutton} 
              style={sourceTransferStyle} 
              onClick={this.handleSetExpItemClick}/> 
 
          <img src={transferbutton} 
              style={reagentsTransferStyle} 
              onClick={this.handleSetExpResourceClick}/> 

          <span style={reagentsStyle}>Reagents</span>
          {itemFrameResources}
          {itemExpResources}

          <img src={experimentbutton} 
               style={expButtonStyle}
               onClick={this.handleExperimentClick}  /> 
        </HalfPanel>

     </div>
    );
  }
}
