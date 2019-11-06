
import * as React from "react";
import errorpanel from "ui_comp/errorframe.png";
import okbutton from "ui_comp/okbutton.png";
import { Global } from "../global";
import { GameEvent } from "../gameEvent";

interface ErrorProps {
  errmsg,
}

export default class ErrorPanel extends React.Component<ErrorProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };
   
    this.handleOkClick = this.handleOkClick.bind(this);
  }

  handleOkClick() {
    Global.gameEmitter.emit(GameEvent.ERROR_OK_CLICK, {});
  }

  render() {
    const errorStyle = {
      top: '50%',
      left: '50%',
      width: '333px',
      height: '119px',
      marginTop: '-59px',
      marginLeft: '-166px',
      position: 'fixed',
      zIndex: 20
    } as React.CSSProperties

    const errorPanelStyle = {
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

    return (
      <div style={errorStyle}>
        <img src={errorpanel} style={errorPanelStyle}/>
        <span style={spanNameStyle}>{this.props.errmsg}</span>
        <img src={okbutton} style={okButtonStyle} onClick={this.handleOkClick}/>
      </div>
    );
  }
}

