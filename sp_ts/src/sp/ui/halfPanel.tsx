import * as React from "react";
import exitbutton from "ui_comp/exitbutton.png"
import { Global } from "../global";
import { GameEvent } from "../gameEvent";

interface HalfPanelProps {
  left : boolean,
  panelType : string,
  hideExitButton : boolean
}

export default class HalfPanel extends React.Component<HalfPanelProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };

    this.handleExitClick = this.handleExitClick.bind(this)
  }

  handleExitClick(event : React.MouseEvent) {
    console.log('Exit click')
    const eventData = {panelType: this.props.panelType};

    Global.gameEmitter.emit(GameEvent.EXIT_HALFPANEL_CLICK, eventData);
  }

  render() {
    if(this.props.left) {
      var halfPanelStyle = {
        top: '50%',
        left: '50%',
        width: '323px',
        height: '360px',
        marginTop: '-180px',
        marginLeft: '-323px',
        position: 'fixed',
        zIndex: 6
      } as React.CSSProperties

      var exitStyle = {
        top: '50%',
        left: '50%',
        marginTop: '-180px',
        marginLeft: '-50px',
        position: 'fixed',
        zIndex: 7
      } as React.CSSProperties

    } else {
       var halfPanelStyle = {
        top: '50%',
        left: '50%',
        width: '323px',
        height: '360px',
        marginTop: '-180px',
        marginLeft: '0px',
        position: 'fixed',
        zIndex: 6
      } as React.CSSProperties     

      var exitStyle = {
          top: '50%',
          left: '50%',
          marginTop: '-180px',
          marginLeft: '273px',
          position: 'fixed',
          zIndex: 7
        } as React.CSSProperties
    }

    return (
      <div style={halfPanelStyle}>
          <img src='/static/art/ui/halfpanel.png'/>

          {!this.props.hideExitButton && 
              <img src={exitbutton} 
                   onClick={this.handleExitClick} 
                   style={exitStyle}/> }

          {this.props.children}
      </div>
    );
  }
}