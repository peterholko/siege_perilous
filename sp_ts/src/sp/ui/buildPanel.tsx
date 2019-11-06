import * as React from "react";
import HalfPanel from "./halfPanel";
import { Global } from "../global";
import leftbutton from "ui_comp/leftbutton.png";
import rightbutton from "ui_comp/rightbutton.png";
import buildbutton from "ui_comp/buildbutton.png";
import { Network } from "../network";
import { GameEvent } from "../gameEvent";
import ResourceItem from "./resourceItem";

interface BuildPanelProps {
  structuresData,
}

export default class BuildPanel extends React.Component<BuildPanelProps, any> {
  constructor(props) {
    super(props);

    this.state = {
      structure : this.props.structuresData[0],
      index : 0
    };

    this.handleLeftClick = this.handleLeftClick.bind(this);
    this.handleRightClick = this.handleRightClick.bind(this);
    this.handleBuildClick = this.handleBuildClick.bind(this);
  }

  handleLeftClick(event) {
    if(this.state.index != 0) {
      const newIndex = this.state.index - 1;
      this.setState({structure: this.props.structuresData[newIndex],
                     index: newIndex})
    } 
  }

  handleRightClick(event) {
    if(this.state.index != (this.props.structuresData.length - 1)) {
      const newIndex = this.state.index + 1;
      this.setState({structure: this.props.structuresData[newIndex],
                     index: newIndex})
    } 
  }

  handleBuildClick() {
    Network.sendCreateFoundation(Global.heroId, this.state.structure.name);
    Global.gameEmitter.emit(GameEvent.START_BUILD_CLICK, {});
  }

  render() {
    var imageName = this.state.structure.name.toLowerCase() + '.png';
    const reqs = [];

    for(var i = 0; i < this.state.structure.req.length; i++) {
      var req = this.state.structure.req[i];

      /*reqs.push(
        <tr key={i}>
          <td>{req.type} </td>
          <td>x {req.quantity}</td>
        </tr>)*/
      reqs.push(
        <ResourceItem key={i}
                      resourceName={req.type}
                      quantity={req.quantity}
                      index={i}
                      showQuantity={true}/>
      )
    }

    const imageStyle = {
      transform: 'translate(-195px, 25px)',
      position: 'fixed'
    } as React.CSSProperties

    const spanNameStyle = {
      transform: 'translate(-323px, 100px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '323px'
    } as React.CSSProperties

    const tableStyle = {
      transform: 'translate(20px, -230px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px'
    } as React.CSSProperties

    const tableStyle2 = {
      transform: 'translate(-80px, 10px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px'
    } as React.CSSProperties

    const leftStyle = {
      transform: 'translate(-305px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const rightStyle = {
      transform: 'translate(-65px, 295px)',
      position: 'fixed'
    } as React.CSSProperties
  
    const buildStyle = {
      transform: 'translate(-187px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const reqDivStyle = {
      transform: 'translate(20px, -120px)',
      position: 'fixed'
    } as React.CSSProperties


    return (
      <HalfPanel left={true} 
                 panelType={'build'} 
                 hideExitButton={false}>
        <img src={'/static/art/' + imageName} style={imageStyle} />
        <span style={spanNameStyle}>
          {this.state.structure.name} Level {this.state.structure.level}
        </span>
        <table style={tableStyle}>
          <tbody>
            <tr>
              <td>Class:</td>
              <td>{this.state.structure.subclass}</td>
            </tr>
            <tr>
              <td>HP:</td>
              <td>{this.state.structure.base_hp}</td>
            </tr>
            <tr>
              <td>Defense:</td>
              <td>{this.state.structure.base_def}</td>
            </tr>
            <tr>
              <td>Build Time:</td>
              <td>{this.state.structure.build_time}</td>
            </tr>
            <tr>
              <td>Materials:</td>
            </tr>
          </tbody>
        </table>
        <div style={reqDivStyle}>
          {reqs}
        </div>
        <img src={leftbutton} style={leftStyle} onClick={this.handleLeftClick} />
        <img src={rightbutton} style={rightStyle} onClick={this.handleRightClick} />
        <img src={buildbutton} style={buildStyle} onClick={this.handleBuildClick} />
      </HalfPanel>
    );
  }
}



