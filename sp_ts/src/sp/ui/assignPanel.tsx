import * as React from "react";
import HalfPanel from "./halfPanel";
import { Global } from "../global";
import leftbutton from "ui_comp/leftbutton.png";
import rightbutton from "ui_comp/rightbutton.png";
import assignbutton from "ui_comp/okbutton.png";
import { Network } from "../network";
import { GameEvent } from "../gameEvent";

interface AssignPanelProps {
  structuredId,
  assignData,
}

export default class AssignPanel extends React.Component<AssignPanelProps, any> {
  constructor(props) {
    super(props);

    this.state = {
      villager : this.props.assignData[0],
      index : 0
    };

    this.handleLeftClick = this.handleLeftClick.bind(this);
    this.handleRightClick = this.handleRightClick.bind(this);
    this.handleAssignClick = this.handleAssignClick.bind(this);
  }

  handleLeftClick(event) {
    if(this.state.index != 0) {
      const newIndex = this.state.index - 1;
      this.setState({villager: this.props.assignData[newIndex],
                     index: newIndex})
    } 
  }

  handleRightClick(event) {
    if(this.state.index != (this.props.assignData.length - 1)) {
      const newIndex = this.state.index + 1;
      this.setState({villager: this.props.assignData[newIndex],
                     index: newIndex})
    } 
  }

  handleAssignClick() {
    Network.sendAssign(this.state.villager.id, this.props.structuredId);
    Global.gameEmitter.emit(GameEvent.ASSIGN_CLICK, {});
  }

  render() {
    var imageName = this.state.villager.image.toLowerCase() + '_single.png';

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
      transform: 'translate(-320px, 60px)',
      position: 'fixed'
    } as React.CSSProperties

    const rightStyle = {
      transform: 'translate(-50px, 60px)',
      position: 'fixed'
    } as React.CSSProperties
  
    const assignStyle = {
      transform: 'translate(-187px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    return (
      <HalfPanel left={false} 
                 panelType={'assign'} 
                 hideExitButton={false}>
        <img src={'/static/art/' + imageName} style={imageStyle} />
        <span style={spanNameStyle}>
          {this.state.villager.name}
        </span>
        <table style={tableStyle}>
          <tbody>
            <tr>
              <td>Order:</td>
              <td>{this.state.villager.order}</td>
            </tr>
            <tr>
              <td>Assigned To:</td>
              <td>{this.state.villager.structure}</td>
            </tr>
          </tbody>
        </table>
        <img src={leftbutton} style={leftStyle} onClick={this.handleLeftClick} />
        <img src={rightbutton} style={rightStyle} onClick={this.handleRightClick} />
        <img src={assignbutton} style={assignStyle} onClick={this.handleAssignClick} />
      </HalfPanel>
    );
  }
}



