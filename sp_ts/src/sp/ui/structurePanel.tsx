import * as React from "react";
import HalfPanel from "./halfPanel";
import { Global } from "../global";
import buildbutton from "ui_comp/buildbutton.png";
import deletebutton from "ui_comp/deletebutton.png";
import { Network } from "../network";
import { GameEvent } from "../gameEvent";
import { NetworkEvent } from "../networkEvent";

interface StructurePanelProps {
  structureData,
}

export default class StructurePanel extends React.Component<StructurePanelProps, any> {
  private timer;

  constructor(props) {
    super(props);

    this.state = {
      hideProgress : true,
      maxProgress: 100,
      progress: 0
    };

    this.handleBuildClick = this.handleBuildClick.bind(this);
    this.handleDeleteClick = this.handleDeleteClick.bind(this);
  
    this.startTimer = this.startTimer.bind(this)
    this.stopTimer = this.stopTimer.bind(this)
    
    Global.gameEmitter.on(NetworkEvent.BUILD, this.handleNetworkBuild, this);
  }

  handleBuildClick() {
    Network.sendBuild(Global.heroId, this.props.structureData.id);
    //Global.gameEmitter.emit(GameEvent.START_BUILD_CLICK, {});
  }

  handleDeleteClick() {
    console.log('Delete Structure')
  }

  handleNetworkBuild(message) {
    console.log('Network build');
    const buildTimeSeconds = Math.floor(message.build_time / 5);

    this.setState({hideProgress: false,
                   maxProgress: buildTimeSeconds});

    this.startTimer();
  }

  startTimer() {
    this.timer = setInterval(() => this.setState({
      progress: this.state.progress + 1  
    }), 1000);
  }

  stopTimer() {
    clearInterval(this.timer)
  }

  render() {
    var imageName = '';

    if(this.props.structureData.state == 'founded') {
      imageName = 'foundation.png'
    } else {
      imageName = this.props.structureData.name.toLowerCase() + '.png';
    }

    const reqs = [];

    for(var i = 0; i < this.props.structureData.req.length; i++) {
      var req = this.props.structureData.req[i];

      reqs.push(
        <tr key={i}>
          <td>{req.type} </td>
          <td>x {req.quantity}</td>
        </tr>)
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

    const buildStyle = {
      transform: 'translate(-212px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const deleteStyle = {
      transform: 'translate(-162px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    return (
      <HalfPanel left={true} 
                 panelType={'structure'} 
                 hideExitButton={false}>
        <img src={'/static/art/' + imageName} style={imageStyle} />
        <span style={spanNameStyle}>
          {this.props.structureData.name} Level {this.props.structureData.level}
        </span>
        <table style={tableStyle}>
          <tbody>
            
            <tr>
              <td>State:</td>
              <td>{this.props.structureData.state}</td>
            </tr>
            <tr>
              <td>Subclass:</td>
              <td>{this.props.structureData.subclass}</td>
            </tr>
            <tr>
              <td>Base HP:</td>
              <td>{this.props.structureData.base_hp}</td>
            </tr>
            <tr>
              <td>Base Defense:</td>
              <td>{this.props.structureData.base_def}</td>
            </tr>
            <tr>
              <td>Build Time:</td>
              <td><progress max="100" value="50"></progress></td>
            </tr>
            { ! this.state.hideProgress &&
              <tr>
                <td>Build Progress: </td>
                <td><progress max={this.state.maxProgress} value={this.state.progress}>{this.state.progress}</progress></td>
              </tr>
            }
            <tr>
              <td>Requirements:</td>
              <td>
                <table style={tableStyle2}>
                  <tbody>
                    {reqs}
                  </tbody>
                </table>
              </td>
            </tr>
          </tbody>
        </table>
        <img src={buildbutton} style={buildStyle} onClick={this.handleBuildClick} />
        <img src={deletebutton} style={deleteStyle} onClick={this.handleDeleteClick} />
      </HalfPanel>
    );
  }
}



