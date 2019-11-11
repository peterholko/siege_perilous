import * as React from "react";
import HalfPanel from "./halfPanel";
import { Global } from "../global";
import leftbutton from "ui_comp/leftbutton.png";
import rightbutton from "ui_comp/rightbutton.png";
import hirebutton from "ui_comp/okbutton.png";
import { Network } from "../network";
import { GameEvent } from "../gameEvent";

interface MHPProps {
  hireData,
}

export default class MerchantHirePanel extends React.Component<MHPProps, any> {
  constructor(props) {
    super(props);

    this.state = {
      villager : this.props.hireData[0],
      index : 0
    };

    this.handleLeftClick = this.handleLeftClick.bind(this);
    this.handleRightClick = this.handleRightClick.bind(this);
    this.handleHireClick = this.handleHireClick.bind(this);
  }

  handleLeftClick(event) {
    if(this.state.index != 0) {
      const newIndex = this.state.index - 1;
      this.setState({villager: this.props.hireData[newIndex],
                     index: newIndex})
    } 
  }

  handleRightClick(event) {
    if(this.state.index != (this.props.hireData.length - 1)) {
      const newIndex = this.state.index + 1;
      this.setState({villager: this.props.hireData[newIndex],
                     index: newIndex})
    } 
  }

  handleHireClick() {
    Network.sendHire(Global.merchantSellTarget, this.state.villager.id);
    Global.gameEmitter.emit(GameEvent.MERCHANT_HIRE_CLICK, {});
  }

  render() {
    var imageName = this.state.villager.image.toLowerCase() + '_single.png';

    var topStats = [];

    topStats.push({'name': 'Creativity', 'value': this.state.villager.Creativity});
    topStats.push({'name': 'Dexterity', 'value': this.state.villager.Dexterity});
    topStats.push({'name': 'Endurance', 'value': this.state.villager.Endurance});
    topStats.push({'name': 'Focus', 'value': this.state.villager.Focus});
    topStats.push({'name': 'Intellect', 'value': this.state.villager.Intellect});
    topStats.push({'name': 'Spirit', 'value': this.state.villager.Spirit});
    topStats.push({'name': 'Strength', 'value': this.state.villager.Strength});
    topStats.push({'name': 'Toughness', 'value': this.state.villager.Toughness});

    topStats.sort((a, b) => (a.value < b.value) ? 1 : -1);

    var skills = [];
    var key = 0;

    for(var skill in this.state.villager.skills) {
      skills.push(<tr key={key}>
                    <td>{skill}</td>
                    <td>{this.state.villager.skills[skill]}</td>
                  </tr>);

      key++;
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

    const tableStyle2 = {
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
                 panelType={'hire'} 
                 hideExitButton={false}>
        <img src={'/static/art/' + imageName} style={imageStyle} />
        <span style={spanNameStyle}>
          {this.state.villager.name}
        </span>
        <table style={tableStyle}>
          <tbody>
            <tr>
              <td>Wage:</td>
              <td>{this.state.villager.wage}</td>
            </tr>
            <tr>
              <td>Primary Skills:</td>
              <td><table style={tableStyle2}>
                <tbody>
                  {skills}
                </tbody>
                </table></td>
            </tr>
            <tr>
              <td>Primary Stats:</td>
              <td>{topStats[0].name} ({topStats[0].value}) <br/>
                  {topStats[1].name} ({topStats[1].value}) <br/>
                  {topStats[2].name} ({topStats[2].value})
              </td>
            </tr>
          </tbody>
        </table>
        <img src={leftbutton} style={leftStyle} onClick={this.handleLeftClick} />
        <img src={rightbutton} style={rightStyle} onClick={this.handleRightClick} />
        <img src={hirebutton} style={assignStyle} onClick={this.handleHireClick} />
      </HalfPanel>
    );
  }
}



