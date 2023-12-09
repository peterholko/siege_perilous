import * as React from "react";
import HalfPanel from "./halfPanel";
import { Global } from "../global";
import upgradebutton from "ui_comp/upgradebutton.png";
import leftbutton from "ui_comp/leftbutton.png";
import rightbutton from "ui_comp/rightbutton.png";
import { Network } from "../network";
import { NetworkEvent } from "../networkEvent";
import ResourceItem from "./resourceItem";

interface SUPProps {
  upgradeData,
}

export default class StructureUpgradePanel extends React.Component<SUPProps, any> {
  constructor(props) {
    super(props);

    this.state = {
      upgradeStructure: this.props.upgradeData.upgrade_list[0],
      index: 0
    };

    this.handleLeftClick = this.handleLeftClick.bind(this);
    this.handleRightClick = this.handleRightClick.bind(this);
    this.handleUpgradeClick = this.handleUpgradeClick.bind(this);
    
    Global.gameEmitter.on(NetworkEvent.UPGRADE, this.handleUpgradeResponse, this);
  }

  handleLeftClick(event) {
    if(this.state.index != 0) {
      const newIndex = this.state.index - 1;
      this.setState({upgradeStructure: this.props.upgradeData.upgrade_list[newIndex],
                     index: newIndex})
    } 
  }

  handleRightClick(event) {
    if(this.state.index != (this.props.upgradeData.upgrade_list.length - 1)) {
      const newIndex = this.state.index + 1;
      this.setState({upgradeStructure: this.props.upgradeData.upgrade_list[newIndex],
                     index: newIndex})
    } 
  }

  handleUpgradeClick() {
    Network.sendUpgrade(Global.heroId, this.props.upgradeData.id, this.state.upgradeStructure.name);
  }

  handleUpgradeResponse(message) {
    console.log(message);


  }

  render() {
    console.log(this.state);

    let structureImage = this.state.upgradeStructure.template.toLowerCase().replace(/\s/g, '');
    let structureImagePath = '/static/art/' + structureImage + '.png';

    let nextStructureName = this.state.upgradeStructure.name;

    const reqs = [];

    for (var i = 0; i < this.props.upgradeData.req.length; i++) {
      var req = this.props.upgradeData.req[i];

      reqs.push(
        <ResourceItem key={i}
          resourceName={req.type}
          quantity={req.quantity}
          index={i}
          showQuantity={true} />
      )
    }

    const structureStyle = {
      transform: 'translate(-195px, 25px)',
      position: 'fixed'
    } as React.CSSProperties

    const nextStructureNameStyle = {
      transform: 'translate(-323px, 100px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '323px'
    } as React.CSSProperties

    const upgradeStyle = {
      transform: 'translate(-185px, 285px)',
      position: 'fixed'
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

    const leftStyle = {
      transform: 'translate(-305px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    const rightStyle = {
      transform: 'translate(-65px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    return (
      <HalfPanel left={false}
        panelType={'upgrade'}
        hideExitButton={false}>

        <img src={structureImagePath} style={structureStyle} />
        <span style={nextStructureNameStyle}>{nextStructureName}</span>

        <table style={tableStyle}>
          <tbody>
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

        <img src={leftbutton} style={leftStyle} onClick={this.handleLeftClick} />
        <img src={rightbutton} style={rightStyle} onClick={this.handleRightClick} />
        <img src={upgradebutton} style={upgradeStyle} onClick={this.handleUpgradeClick} /> */
      </HalfPanel>
    );
  }
}

