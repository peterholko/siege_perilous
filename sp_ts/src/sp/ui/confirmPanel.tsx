
import * as React from "react";
import confirmpanel from "ui_comp/errorframe.png";
import exitbutton from "ui_comp/exitbutton.png";
import okbutton from "ui_comp/okbutton.png";
import { Global } from "../global";
import { GameEvent } from "../gameEvent";

interface ConfirmProps {
  msg
}

export default class ConfirmPanel extends React.Component<ConfirmProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };
   
    this.handleOkClick = this.handleOkClick.bind(this);
    this.handleExitClick = this.handleExitClick.bind(this)
  }

  handleOkClick() {
    console.log('Handle Ok Click');
    Global.gameEmitter.emit(GameEvent.CONFIRM_OK_CLICK, {});
  }

  handleExitClick(event : React.MouseEvent) {
    const eventData = {panelType: "confirm"};
    Global.gameEmitter.emit(GameEvent.EXIT_HALFPANEL_CLICK, eventData);
  }

  render() {
    const confirmStyle = {
      top: '50%',
      left: '50%',
      width: '333px',
      height: '119px',
      marginTop: '-59px',
      marginLeft: '-166px',
      position: 'fixed',
      zIndex: 20
    } as React.CSSProperties

    const confirmPanelStyle = {
      position: 'fixed'
    } as React.CSSProperties

    const spanNameStyle = {
      transform: 'translate(15px, 20px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '14px',
      width: '300px'
    } as React.CSSProperties

    const okButtonStyle = {
      transform: 'translate(141px, 90px)',
      position: 'fixed'
    } as React.CSSProperties

    const exitButtonStyle = {
      transform: 'translate(285px, 0px)',
      position: 'fixed'
    } as React.CSSProperties

    return (
      <div style={confirmStyle}>
        <img src={confirmpanel} style={confirmPanelStyle}/>
        <span style={spanNameStyle}>{this.props.msg}</span>
        <img src={okbutton} style={okButtonStyle} onClick={this.handleOkClick}/>
        <img src={exitbutton} 
                   onClick={this.handleExitClick} 
                   style={exitButtonStyle}/>        
      </div>
    );
  }
}

