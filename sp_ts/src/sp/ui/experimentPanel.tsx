
import * as React from "react";
import transferbutton from "ui_comp/transferbutton.png";
import BaseInventoryPanel from "./baseInventoryPanel";
import { Global } from "../global";
import HalfPanel from "./halfPanel";
import itemframe from "ui_comp/itemframe.png";
import experimentbutton from "ui_comp/experimentbutton.png";
import okbutton from "ui_comp/okbutton.png";
import recipe from "art_comp/items/recipe.png";
import { Network } from "../network";
import InventoryItem from "./inventoryItem";
import { EXP_RECIPE_NONE } from "../config";
import selectitemborder from "ui_comp/selectitemborder.png";

interface ETPProps {
  expData
}

export default class ExperimentTransferPanel extends React.Component<ETPProps, any> {
  constructor(props) {
    super(props);

    Global.selectedItemId = -1;
    Global.selectedItemOwnerId = -1;

    const selectExpResStyle = {
      position: "fixed"
    } as React.CSSProperties

    this.state = {
      hideLeftSelect: true,
      hideRightSelect: true,
      hideSelectExpRes: true,
      selectExpResStyle: selectExpResStyle
    };
  
    this.handleSelect = this.handleSelect.bind(this);
    this.handleExpItemSelect = this.handleExpItemSelect.bind(this);
    this.handleExpResSelect = this.handleExpResSelect.bind(this);

    this.handleSetExpItemClick = this.handleSetExpItemClick.bind(this);
    this.handleSetExpResourceClick = this.handleSetExpResourceClick.bind(this);
    this.handleExperimentClick = this.handleExperimentClick.bind(this);
    this.handleOkClick = this.handleOkClick.bind(this);
  }

  handleSelect() {
    this.setState({hideLeftSelect: false,
                   hideSelectExpRes: true});
  }

  handleExpItemSelect(eventData) {
    console.log('handleExpItemSelect ' + eventData);

    const selectStyle = {
      transform: 'translate(-185px, 50px)',
      position: 'fixed'
    } as React.CSSProperties

    Global.selectedItemOwnerId = eventData.ownerId;
    Global.selectedItemId = eventData.itemId;

    this.setState({hideSelectExpRes: false,
                   hideLeftSelect: true,
                   selectExpResStyle: selectStyle})
  }

  handleExpResSelect(eventData) {
    console.log('handleExpResSelect ' + eventData);
    var xPos = -215 + ((eventData.index % 5) * 60);
    var yPos = 140 + (Math.floor(eventData.index / 5) * 53);

    const selectStyle = {
      transform: 'translate(' + xPos + 'px, ' + yPos + 'px)',
      position: 'fixed'
    } as React.CSSProperties

    Global.selectedItemOwnerId = eventData.ownerId;
    Global.selectedItemId = eventData.itemId;

    this.setState({hideSelectExpRes: false,
                   hideLeftSelect: true,
                   selectExpResStyle: selectStyle});
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

  handleOkClick() {
    Network.sendResetExperiment(this.props.expData.id);
  }

  render() {
    var itemExperiment = []; //Only one item possible 
    var itemFrameResources = [];
    var itemExpResources = [];

    var showNewRecipe = this.props.expData.hasOwnProperty("recipe");

    const sourceTransferStyle = {
      transform: 'translate(-250px, 50px)',
      position: 'fixed'
    } as React.CSSProperties

    const reagentsTransferStyle = {
      transform: 'translate(-280px, 140px)',
      position: 'fixed'
    } as React.CSSProperties 

    const sourceStyle = {
      transform: 'translate(-323px, 25px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '323px'
    } as React.CSSProperties

    const reagentsStyle = {
      transform: 'translate(-323px, 115px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '323px'
    } as React.CSSProperties

    const expItemStyle = {
      transform: 'translate(-185px, 50px)',
      position: 'fixed'
    } as React.CSSProperties

    const expButtonStyle = {
      transform: 'translate(-185px, 290px)',
      position: 'fixed'
    } as React.CSSProperties
 
    const okButtonStyle = {
      transform: 'translate(-185px, 290px)',
      position: 'fixed'
    } as React.CSSProperties
 
    const eurakaStyle = {
      transform: 'translate(-323px, 40px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '323px'
    } as React.CSSProperties
  
    const recipeStyle = {
      transform: 'translate(-185px, 75px)',
      position: 'fixed'
    } as React.CSSProperties

    const recipeNameStyle = {
      transform: 'translate(-323px, 140px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '323px'
    } as React.CSSProperties

    const expStateStyle = {
      transform: 'translate(-323px, 230px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '323px'
    } as React.CSSProperties

    if(this.props.expData.expitem.length > 0) {
      var xPos = 138;
      var yPos = -310;

      var itemId = this.props.expData.expitem[0].id;
      var itemName = this.props.expData.expitem[0].name;
      var image = this.props.expData.expitem[0].image;
      var quantity = this.props.expData.expitem[0].quantity;

      itemExperiment.push(
         <InventoryItem key={1}
                       ownerId={this.props.expData.id}
                       itemId={itemId} 
                       itemName={itemName} 
                       image={image}
                       quantity={quantity}
                       index={i}
                       xPos={xPos}
                       yPos={yPos}
                       handleSelect={this.handleExpItemSelect} />
      )
    }

    for(var i = 0; i < 2; i++) {
      var xPos = i * 60 - 215;
      var yPos = 140;

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
      var yPos = -220;

      var itemId = this.props.expData.expresources[i].id;
      var itemName = this.props.expData.expresources[i].name;
      var image = this.props.expData.expresources[i].image;
      var quantity = this.props.expData.expresources[i].quantity;

      itemExpResources.push(
        <InventoryItem key={i}
                       ownerId={this.props.expData.id}
                       itemId={itemId} 
                       itemName={itemName}
                       image={image} 
                       quantity={quantity}
                       index={i}
                       xPos={xPos}
                       yPos={yPos}
                       handleSelect={this.handleExpResSelect} />
      );
    }

    if(showNewRecipe) {
      return(
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
          
            <span style={eurakaStyle}>Euraka!</span>
            <img src={recipe} style={recipeStyle} />
            <span style={recipeNameStyle}>{this.props.expData.recipe.name}</span>

            <img src={okbutton}
                 style={okButtonStyle}
                 onClick={this.handleOkClick}  /> 

          </HalfPanel>
        </div>
      )
    } else {
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
              
              <span style={sourceStyle}>Source Item</span>
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

              <span style={expStateStyle}>{this.props.expData.expstate}</span>

              <img src={experimentbutton}
                    style={expButtonStyle}
                    onClick={this.handleExperimentClick}  /> 

              {!this.state.hideSelectExpRes && 
                <img src={selectitemborder} style={this.state.selectExpResStyle} /> 
              }
              
            </HalfPanel>
          </div>
        );
      }
    }
}
