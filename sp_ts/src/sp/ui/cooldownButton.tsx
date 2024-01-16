
import * as React from "react";
import cooldownbg from "ui_comp/cooldownbg.png";
import { Global } from "../global";
import { NetworkEvent } from "../networkEvent";

interface CooldownButtonProps {
  imageName,
  imageButton,
  className,
  handler
}

export default class CooldownButton extends React.Component<CooldownButtonProps, any> {

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
    Global.gameEmitter.on(NetworkEvent.EXPLORE, this.handleExplore, this);
  }

  handleExplore(message) {
    this.setState({ cooldown: message.explore_time })
    this.startTimer();
  }

  startTimer() {
    var timerId = setInterval(this.timer, 1000);
    this.setState({ timerId: timerId });
  }

  stopTimer() {
    clearInterval(this.state.timerId);
  }

  timer() {
    if (this.state.cooldown > 1) {
      this.setState({ cooldown: this.state.cooldown - 1 });
    } else {
      this.setState({ cooldown: -1 });
      this.stopTimer();
    }
  }

  handleClick = () => {
    this.props.handler()
  }

  render() {
    const spanStyle = {
      transform: 'translate(10px, 13px)',
      position: 'fixed',
      fontFamily: 'Verdana',
      fontSize: '30px',
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
      <div id={this.props.imageName} className={this.props.className}>
        {this.state.cooldown != -1 &&
          <img src={cooldownbg} style={cooldownBgStyle} />}

        {this.state.cooldown != -1 &&
          <span style={spanStyle}>{this.state.cooldown}</span>}

        <img src={this.props.imageButton}
          onClick={this.handleClick} />
      </div>
    );
  }
}
