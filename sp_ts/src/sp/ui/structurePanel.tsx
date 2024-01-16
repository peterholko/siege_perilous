import * as React from "react";
import HalfPanel from "./halfPanel";
import { Global } from "../global";
import craftbutton from "ui_comp/craftbutton.png";
import buildbutton from "ui_comp/buildbutton.png";
import deletebutton from "ui_comp/deletebutton.png";
import assignbutton from "ui_comp/assignbutton.png";
import refinebutton from "ui_comp/refinebutton.png";
import experimentbutton from "ui_comp/experimentbutton.png";
import upgradebutton from "ui_comp/upgradebutton.png";
import { Network } from "../network";
import '../ui.module.css';
import { FOUNDED, PROGRESSING, STALLED, NONE, CRAFT, UPGRADING } from "../config";
import { NetworkEvent } from "../networkEvent";
import { GameEvent } from "../gameEvent";
import ResourceItem from "./resourceItem";
import SmallButton from "./smallButton";

interface StructurePanelProps {
  structureData,
}

export default class StructurePanel extends React.Component<StructurePanelProps, any> {
  private timer;

  constructor(props) {
    super(props);

    var progress = 0;
    var maxProgress = 100;

    if ('progress' in this.props.structureData) {
      if (this.props.structureData.state != UPGRADING) {
        maxProgress = this.props.structureData.build_time / 10;
      } else {
        maxProgress = this.props.structureData.upgrade_time / 10;
      }
      progress = (this.props.structureData.progress / 100) * maxProgress;
    }

    this.state = {
      maxProgress: maxProgress,
      progress: progress,
      structureData: this.props.structureData
    };

    this.handleCraftClick = this.handleCraftClick.bind(this);
    this.handleRefineClick = this.handleRefineClick.bind(this);
    this.handleBuildClick = this.handleBuildClick.bind(this);
    this.handleAssignClick = this.handleAssignClick.bind(this);
    this.handleDeleteClick = this.handleDeleteClick.bind(this);
    this.handleExperimentClick = this.handleExperimentClick.bind(this);
    this.handleUpgradeClick = this.handleUpgradeClick.bind(this);
    this.handleSendDelete = this.handleSendDelete.bind(this);
    this.handleResponseUpgrade = this.handleResponseUpgrade.bind(this);

    this.startTimer = this.startTimer.bind(this)
    this.stopTimer = this.stopTimer.bind(this)

    Global.gameEmitter.on(NetworkEvent.BUILD, this.handleNetworkBuild, this);
    Global.gameEmitter.on(NetworkEvent.UPGRADE, this.handleResponseUpgrade, this);
    Global.gameEmitter.on(GameEvent.CONFIRM_OK_CLICK, this.handleSendDelete, this);
  }

  componentWillUnmount() {
    this.stopTimer();
    Global.gameEmitter.removeListener(NetworkEvent.BUILD, this.handleNetworkBuild);
    Global.gameEmitter.removeListener(NetworkEvent.UPGRADE, this.handleResponseUpgrade);
  }

  componentDidMount() {
    if (this.props.structureData.state == PROGRESSING) {
      this.startTimer();
    }
  }

  componentDidUpdate() {
    console.log("componentDidUpdate" + JSON.stringify(this.props.structureData));
  }

  handleUpgradeClick() {
    Network.sendInfoUpgrade(this.state.structureData.id);
  }

  handleCraftClick() {
    Network.sendGetRecipeList(this.state.structureData.id);
  }

  handleRefineClick() {
    Network.sendOrderRefine(this.state.structureData.id);
  }

  handleExperimentClick() {
    Network.sendInfoExperiment(this.state.structureData.id);
  }

  handleBuildClick() {
    Network.sendBuild(Global.heroId, this.state.structureData.id);
  }

  handleAssignClick() {
    Network.sendGetAssignList();
  }

  handleSendDelete() {
    Network.sendDelete(this.state.structureData.id);    
    
    //To hide the structure panel
    Global.gameEmitter.emit(GameEvent.DELETE_STRUCTURE_CLICK, {});
  }

  handleDeleteClick() {
    console.log('Delete Structure');

    const event = {
      msg: 'Remove the structure?',
      type: 'delete_structure'
    };

    Global.gameEmitter.emit(GameEvent.CONFIRMATION, event);
  }

  handleNetworkBuild(message) {
    console.log('Network build');
    const buildTimeSeconds = Math.floor(message.build_time);

    this.setState({ maxProgress: buildTimeSeconds / 10 });
    this.startTimer();
  }

  handleResponseUpgrade(message) {
    console.log('Response Upgrade');
    const upgradeTimeSeconds = Math.floor(message.upgrade_time);

    this.setState({
      progress: 0,
      maxProgress: upgradeTimeSeconds / 10
    });
    this.startTimer();
  }

  startTimer() {
    this.timer = setInterval(() => {
      console.log("progress: " + this.state.progress);
      console.log("maxProgress: " + this.state.maxProgress);

      if (this.state.progress >= this.state.maxProgress) {
        this.stopTimer();
        console.log('progress >>> maxProgress');
        Network.sendInfoObj(this.state.structureData.id);
      } else {
        this.setState({ progress: this.state.progress + 1 });
      }
    }, 1000);
  }

  stopTimer() {
    clearInterval(this.timer)
  }

  render() {
    console.log('Rendering Structure Panel...');
    console.log(this.props.structureData);

    const showCraftButton = (this.props.structureData.state == NONE &&
      this.props.structureData.subclass == CRAFT);

    const showRefineButton = (this.props.structureData.state == NONE &&
      this.props.structureData.subclass == CRAFT);

    const showExperimentButton = (this.props.structureData.state == NONE &&
      this.props.structureData.subclass == CRAFT &&
      this.props.structureData.level != -1);

    const showBuildButton = (this.props.structureData.state == FOUNDED ||
      this.props.structureData.state == STALLED);

    const showProgress = (this.props.structureData.state == FOUNDED ||
      this.props.structureData.state == PROGRESSING ||
      this.props.structureData.state == STALLED ||
      this.props.structureData.state == UPGRADING)

    const showAssignButton = this.props.structureData.state == NONE;
    const showUpgradeButton = this.props.structureData.state == NONE;

    const isFinished = this.props.structureData.state == NONE;

    let progressLabel = "Build";
    let duration = this.props.structureData.build_time / 10;

    if (this.props.structureData.state == UPGRADING) {
      progressLabel = "Upgrade";
      //TODO need to set upgrade time on structure data on server
      //duration = this.props.structureData.upgrade_time
    }

    var imageName = '';

    if (this.props.structureData.props == 'founded') {
      imageName = 'foundation.png'
    } else {
      imageName = this.props.structureData.image + '.png';
    }

    const reqs = [];

    if (this.props.structureData.hasOwnProperty('req')) {
      for (var i = 0; i < this.props.structureData.req.length; i++) {
        var req = this.props.structureData.req[i];

        reqs.push(
          <ResourceItem key={i}
            resourceName={req.type}
            quantity={req.quantity}
            index={i}
            showQuantity={true} />
        )
      }
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

    const divReqsStyle = {
      transform: 'translate(-100px, 15px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px'
    } as React.CSSProperties

    const upgradeStyle = {
      transform: 'translate(-312px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const experimentStyle = {
      transform: 'translate(-262px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const craftStyle = {
      transform: 'translate(-212px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const refineStyle = {
      transform: 'translate(-162px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const buildStyle = {
      transform: 'translate(-112px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const assignStyle = {
      transform: 'translate(-112px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const deleteStyle = {
      transform: 'translate(-62px, 295px)',
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
              <td>Class:</td>
              <td>{this.props.structureData.subclass}</td>
            </tr>

            {isFinished &&
              <tr>
                <td>HP:</td>
                <td>{this.props.structureData.hp} / {this.props.structureData.base_hp}</td>
              </tr>
            }

            {isFinished &&
              <tr>
                <td>Defense:</td>
                <td>{this.props.structureData.base_def}</td>
              </tr>
            }

            {!isFinished &&
              <tr>
                <td>{progressLabel} Time:</td>
                <td>{duration} sec</td>
              </tr>
            }

            {showProgress &&
              <tr>
                <td>{progressLabel} Progress: </td>
                <td><progress max={this.state.maxProgress}
                  value={this.state.progress}>{this.state.progress}
                </progress></td>
              </tr>
            }

            {!isFinished &&
              <tr>
                <td>Requirements:</td>
                <td>
                  <div style={divReqsStyle}>
                    {reqs}
                  </div>
                </td>
              </tr>
            }
          </tbody>
        </table>

        {showUpgradeButton &&
          <SmallButton handler={this.handleUpgradeClick}
            imageName="upgradebutton"
            style={upgradeStyle} />}

        {showExperimentButton &&
          <SmallButton handler={this.handleExperimentClick}
            imageName="experimentbutton"
            style={experimentStyle} />}

        {showCraftButton &&
          <SmallButton handler={this.handleCraftClick}
            imageName="craftbutton"
            style={craftStyle} />}

        {showRefineButton &&
          <SmallButton handler={this.handleRefineClick}
            imageName="refinebutton"
            style={refineStyle} />}

        {showBuildButton &&
          <SmallButton handler={this.handleBuildClick}
            imageName="buildbutton"
            style={buildStyle} />}

        {showAssignButton &&
          <SmallButton handler={this.handleAssignClick}
            imageName="assignbutton"
            style={assignStyle} />}

        <SmallButton handler={this.handleDeleteClick}
          imageName="deletebutton"
          style={deleteStyle} />
      </HalfPanel>
    );
  }
}



