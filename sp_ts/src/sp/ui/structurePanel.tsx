import * as React from "react";
import HalfPanel from "./halfPanel";
import { Global } from "../global";
import craftbutton from "ui_comp/craftbutton.png";
import buildbutton from "ui_comp/buildbutton.png";
import deletebutton from "ui_comp/deletebutton.png";
import assignbutton from "ui_comp/assignbutton.png";
import refinebutton from "ui_comp/refinebutton.png";
import experimentbutton from "ui_comp/experimentbutton.png";
import { Network } from "../network";
import '../ui.css';
import { FOUNDED, PROGRESSING, STALLED, NONE, CRAFT} from "../config";
import { NetworkEvent } from "../networkEvent";
import { GameEvent } from "../gameEvent";
import ResourceItem from "./resourceItem";

interface StructurePanelProps {
  structureData,
}

export default class StructurePanel extends React.Component<StructurePanelProps, any> {
  private timer;

  constructor(props) {
    super(props);

    var progress = 0;
    var maxProgress = 100;

    if('progress' in this.props.structureData) {
      maxProgress = this.props.structureData.build_time / 5;
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
  
    this.startTimer = this.startTimer.bind(this)
    this.stopTimer = this.stopTimer.bind(this)

    Global.gameEmitter.on(NetworkEvent.BUILD, this.handleNetworkBuild, this);
    Global.gameEmitter.on(GameEvent.OBJ_UPDATE, this.handleObjUpdate, this);
   }

  componentWillUnmount() {
    this.stopTimer();
    Global.gameEmitter.removeListener(NetworkEvent.BUILD, this.handleNetworkBuild);
    Global.gameEmitter.removeListener(GameEvent.OBJ_UPDATE, this.handleObjUpdate);
  }

  componentDidMount() {
    if(this.props.structureData.state == PROGRESSING) {
      this.startTimer();
    }
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
    //Global.gameEmitter.emit(GameEvent.START_BUILD_CLICK, {});
  }

  handleAssignClick() {
    Network.sendGetAssignList();
  }

  handleDeleteClick() {
    console.log('Delete Structure')
  }

  handleNetworkBuild(message) {
    console.log('Network build');
    const buildTimeSeconds = Math.floor(message.build_time);

    this.setState({maxProgress: buildTimeSeconds,
                   structureState: PROGRESSING});

    this.startTimer();
  }

  handleObjUpdate(objId) {
    console.log('Obj Update: ' + objId + " " + Math.random());
    if(objId == this.state.structureData.id) {
      let newStructureData = this.state.structureData;
      newStructureData.state = Global.objectStates[objId].state;
      //this.setState({structureData: newStructureData});
    }
  }

  startTimer() {
    this.timer = setInterval(() => {
      if(this.state.progress >= this.state.maxProgress) {
        this.stopTimer();
        Network.sendInfoObj(this.state.structureData.id);        
      } else {
        this.setState({progress: this.state.progress + 1});
      }
    }, 1000);
  }

  stopTimer() {
    clearInterval(this.timer)
  }

  render() {
    const showCraftButton = (this.state.structureData.state == NONE && 
                             this.state.structureData.subclass == CRAFT);

    const showRefineButton = (this.state.structureData.state == NONE &&
                              this.state.structureData.name == 'Blacksmith');

    const showExperimentButton = (this.state.structureData.state == NONE &&
                                  this.state.structureData.subclass == CRAFT);

    const showBuildButton = (this.state.structureData.state == FOUNDED ||
                             this.state.structureData.state == STALLED);                             

    const showProgress = (this.state.structureData.state == FOUNDED ||
                          this.state.structureData.state == PROGRESSING ||
                          this.state.structureData.state == STALLED)

    const showAssignButton = this.state.structureData.state == NONE;

    const isFinished = this.state.structureData.state == NONE;

    var imageName = '';

    if(this.state.structureData.state == 'founded') {
      imageName = 'foundation.png'
    } else {
      imageName = this.state.structureData.name.toLowerCase() + '.png';
    }

    const reqs = [];

    for(var i = 0; i < this.state.structureData.req.length; i++) {
      var req = this.state.structureData.req[i];

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

    const divReqsStyle = {
      transform: 'translate(-100px, 15px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px'
    } as React.CSSProperties

    const craftStyle = {
      transform: 'translate(-262px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const refineStyle = {
      transform: 'translate(-212px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const buildStyle = {
      transform: 'translate(-162px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const assignStyle = {
      transform: 'translate(-162px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const experimentStyle = {
      transform: 'translate(-312px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const deleteStyle = {
      transform: 'translate(-112px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    return (
      <HalfPanel left={true} 
                 panelType={'structure'} 
                 hideExitButton={false}>
        <img src={'/static/art/' + imageName} style={imageStyle} />
        <span style={spanNameStyle}>
          {this.state.structureData.name} Level {this.state.structureData.level}
        </span>
        <table style={tableStyle}>
          <tbody>
            
            <tr>
              <td>State:</td>
              <td>{this.state.structureData.state}</td>
            </tr>
            <tr>
              <td>Class:</td>
              <td>{this.state.structureData.subclass}</td>
            </tr>

            { isFinished && 
              <tr>
                <td>HP:</td>
                <td>{this.state.structureData.base_hp}</td>
              </tr> 
            }

            { isFinished &&
              <tr>
                <td>Defense:</td>
                <td>{this.state.structureData.base_def}</td>
              </tr> 
            }

            <tr>
              <td>Build Time:</td>
              <td>{this.state.structureData.build_time / 5}</td>
            </tr>
            { showProgress &&
              <tr>
                <td>Build Progress: </td>
                <td><progress max={this.state.maxProgress} 
                              value={this.state.progress}>{this.state.progress}
                              </progress></td>
              </tr>
            }
            <tr>
              <td>Requirements:</td>
              <td>
                <div style={divReqsStyle}>
                  {reqs}
                </div>
              </td>
            </tr>
          </tbody>
        </table>

        {showCraftButton &&
          <img src={craftbutton}
               style={craftStyle}
               onClick={this.handleCraftClick} />}
 
        {showRefineButton &&
          <img src={refinebutton}
               style={refineStyle}
               onClick={this.handleRefineClick} />}

        {showExperimentButton && 
          <img src={experimentbutton}
               style={experimentStyle}
               onClick={this.handleExperimentClick} />}
 
        {showBuildButton && 
          <img src={buildbutton} 
               style={buildStyle} 
               onClick={this.handleBuildClick} />}

        {showAssignButton && 
          <img src={assignbutton}
               style={assignStyle}
               onClick={this.handleAssignClick} />}

        <img src={deletebutton} style={deleteStyle} onClick={this.handleDeleteClick} />
      </HalfPanel>
    );
  }
}



