
import * as React from "react";
import gatherpanel from "ui_comp/buttonsframe.png"
import { Network } from "../network";
import { Global } from "../global";

import orebutton from "ui_comp/ore.png";
import woodbutton from "ui_comp/wood.png";
import plantbutton from "ui_comp/plant.png";
import stonebutton from "ui_comp/stone.png";
import waterbutton from "ui_comp/water.png"; 
import gamebutton from "ui_comp/game.png";
import { GameEvent } from "../gameEvent";

interface GatherProps {
  selectedKey : any
}

export default class GatherPanel extends React.Component<GatherProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };

    this.handleOreClick = this.handleOreClick.bind(this)
    this.handleWoodClick = this.handleWoodClick.bind(this)
    this.handlePlantClick = this.handlePlantClick.bind(this)
    this.handleStoneClick = this.handleStoneClick.bind(this)
    this.handleWaterClick = this.handleWaterClick.bind(this)
    this.handleGameClick = this.handleGameClick.bind(this)
  }

  handleOreClick(event : React.MouseEvent) {
    console.log('Ore Click');
    Network.sendGather(this.props.selectedKey.id, 'Ore');
    Global.gameEmitter.emit(GameEvent.RESOURCE_GATHER_CLICK, {});
  }

  handleWoodClick(event : React.MouseEvent) {
    console.log('Transfer Click');
    Network.sendGather(this.props.selectedKey.id, 'Wood');
    Global.gameEmitter.emit(GameEvent.RESOURCE_GATHER_CLICK, {});
  }

  handlePlantClick(event : React.MouseEvent) {
    console.log('Plant Click');
    Network.sendGather(this.props.selectedKey.id, 'Plant');
    Global.gameEmitter.emit(GameEvent.RESOURCE_GATHER_CLICK, {});
  }

  handleStoneClick(event : React.MouseEvent) {
    console.log('Stone Click');
    Network.sendGather(this.props.selectedKey.id, 'Stone');
    Global.gameEmitter.emit(GameEvent.RESOURCE_GATHER_CLICK, {});
  }

  handleWaterClick(event : React.MouseEvent) {
    console.log('Water Click');
    Network.sendGather(this.props.selectedKey.id, 'Water');
    Global.gameEmitter.emit(GameEvent.RESOURCE_GATHER_CLICK, {});
  }

  handleGameClick(event : React.MouseEvent) {
    console.log('Game Click');
    Network.sendGather(this.props.selectedKey.id, 'Game');
    Global.gameEmitter.emit(GameEvent.RESOURCE_GATHER_CLICK, {});
  }
  
  render() {
    var hideOreButton = false;
    var hideWoodButton = false;
    var hidePlantButton = false;
    var hideStoneButton = false;
    var hideWaterButton = false;
    var hideGameButton = false;

    var buttonOrder = {
      ore: 0,
      wood: 1,
      plant: 2,
      stone: 3,
      water: 4,
      game: 5
    };

    const gatherStyle = {
      top: '50%',
      left: '50%',
      width: '320px',
      height: '67px',
      marginTop: '-33px',
      marginLeft: '-160px',
      position: 'fixed',
      zIndex: 6
    } as React.CSSProperties

    const panelStyle = {
      position: 'fixed'
    } as React.CSSProperties

    const oreStyle = {
      transform: 'translate(8px, 8px)',
      position: 'fixed'
    } as React.CSSProperties

    const woodStyle = {
      transform: 'translate(58px, 8px)',
      position: 'fixed'
    } as React.CSSProperties

    const plantStyle = {
      transform: 'translate(108px, 8px)',
      position: 'fixed'  
    } as React.CSSProperties

    const stoneStyle = {
      transform: 'translate(158px, 8px)',
      position: 'fixed'  
    } as React.CSSProperties

    const waterStyle = {
      transform: 'translate(208px, 8px)',
      position: 'fixed'  
    } as React.CSSProperties

    const gameStyle = {
      transform: 'translate(258px, 8px)',
      position: 'fixed'  
    } as React.CSSProperties

    return (
      <div style={gatherStyle} >
          <img src={gatherpanel} style={panelStyle}/>

          {!hideOreButton && 
              <img src={orebutton} 
                   style={oreStyle} 
                   onClick={this.handleOreClick}/>}
                 
          {!hideWoodButton &&
            <img src={woodbutton} 
                 style={woodStyle} 
                 onClick={this.handleWoodClick}/>}

          {!hidePlantButton &&  
            <img src={plantbutton} 
                 style={plantStyle} 
                 onClick={this.handlePlantClick}/>}

          {!hideStoneButton && 
            <img src={stonebutton} 
                 style={stoneStyle} 
                 onClick={this.handleStoneClick}/>}

          {!hideWaterButton && 
            <img src={waterbutton} 
                 style={waterStyle} 
                 onClick={this.handleWaterClick}/>}

          {!hideGameButton && 
            <img src={gamebutton} 
                 style={gameStyle} 
                 onClick={this.handleGameClick}/>}

      </div>
    );
  }
}