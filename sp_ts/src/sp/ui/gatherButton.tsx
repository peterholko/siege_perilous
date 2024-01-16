
import * as React from "react";
import cooldownbg from "ui_comp/cooldownbg.png";
import { Global } from "../global";
import { NetworkEvent } from "../networkEvent";
import gatherbuttonimage from "ui_comp/gatherbutton.png";
import gatherbuttonimageclick from "ui_comp/gatherbutton.png";

interface GatherButtonProps {
  className,
  handler
}

export default class GatherButton extends React.Component<GatherButtonProps, any> {

  constructor(props) {
    super(props);

    this.state = {
      timerId: -1,
      cooldown: -1,
      showClicked: false
    };

    this.startTimer = this.startTimer.bind(this);
    this.stopTimer = this.stopTimer.bind(this);
    this.handleClick = this.handleClick.bind(this);
    this.timer = this.timer.bind(this);
    this.hideImage = this.hideImage.bind(this);
  }

  componentDidMount() {
    Global.gameEmitter.on(NetworkEvent.GATHER, this.handleGather, this);
  }

  handleGather(message) {
    this.setState({ cooldown: message.gather_time })
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
    this.setState({ showClicked: true });
    setTimeout(this.hideImage, 100);
  }

  hideImage() {
    this.setState({ showClicked: false });
  }

  render() {
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
      <div id='gatherbutton' className={this.props.className}>
        {this.state.cooldown != -1 &&
          <img src={cooldownbg} style={cooldownBgStyle} />}

        {this.state.cooldown != -1 &&
          <span style={spanStyle}>{this.state.cooldown}</span>}

        {!this.state.showClicked && <img src={gatherbuttonimage}
          onClick={this.handleClick} />}

        {this.state.showClicked && <img src={'/static/art/ui/gatherbutton_click.png'} />}

      </div>
    );
  }
}
