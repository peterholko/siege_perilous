
import * as React from "react";
import quickattackbutton from "ui_comp/quickattackbutton.png";
import preciseattackbutton from "ui_comp/preciseattackbutton.png";
import fierceattackbutton from "ui_comp/fierceattackbutton.png";
import cooldownbg from "ui_comp/cooldownbg.png";
import styles from "./../ui.css";
import { Global } from "../global";
import { NetworkEvent } from "../networkEvent";
import { QUICK, PRECISE, FIERCE } from "../config";

interface ActionButtonProps {
  type,
  handler
}

export default class ActionButton extends React.Component<ActionButtonProps, any> {

  constructor(props) {
    super(props);

    this.state = {
      timerId: -1,
      cooldown: -1
    };

    this.startTimer = this.startTimer.bind(this);
    this.stopTimer = this.stopTimer.bind(this);
    this.handleClick = this.handleClick.bind(this);
    this.timer = this.timer.bind(this);
    
  }

  componentDidMount() {
    Global.gameEmitter.on(NetworkEvent.ATTACK, this.handleAttack, this);
  }

  handleAttack(message) {
    if(message.attacktype == this.props.type) {
      this.setState({cooldown: message.cooldown})
      this.startTimer();
    }
  }
  
   startTimer() {
    var timerId = setInterval(this.timer, 1000);
    this.setState({timerId: timerId});
  }

  stopTimer() {
    clearInterval(this.state.timerId);
  }

  timer() {
    if(this.state.cooldown > 1) {
      this.setState({cooldown: this.state.cooldown - 1});
    } else {
      this.setState({cooldown: -1});
      this.stopTimer();
    }
  }

  handleClick = () => {
    this.props.handler()
  }

  render() {
    var buttonType;
    var cssStyle;

    if(this.props.type == QUICK) {
      buttonType = quickattackbutton;
      cssStyle = styles.quickattackbutton;
    } else if(this.props.type == PRECISE) {
      buttonType = preciseattackbutton;
      cssStyle = styles.preciseattackbutton;
    } else if(this.props.type == FIERCE) {
      buttonType = fierceattackbutton;
      cssStyle = styles.fierceattackbutton;
    }

    const spanStyle = {
      transform: 'translate(12px, 13px)',
      position: 'fixed',
      fontFamily: 'Verdana',
      fontSize: '36px',
      width: '50px',
      height: '50px',
      color: 'white',
      WebkitTextStroke: '1px black'
    } as React.CSSProperties

    const cooldownBgStyle = {
      position: 'fixed',
      opacity: 0.5,
    } as React.CSSProperties

    return (
      <div id={this.props.type + 'attackbutton'} className={cssStyle}>
        {this.state.cooldown != -1 && 
          <img src={cooldownbg} style={cooldownBgStyle}/> }

        {this.state.cooldown != -1 && 
          <span style={spanStyle}>{this.state.cooldown}</span> }

        <img src={buttonType} 
             onClick={this.handleClick}/>
      </div>
    );
  }
}
