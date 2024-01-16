import * as React from "react";
import HalfPanel from "./halfPanel";
import attrsbutton from "ui_comp/attrsbutton.png";
import skillsbutton from "ui_comp/skillsbutton.png";
import upgradebutton from "ui_comp/upgradebutton.png";
import { Global } from "../global";
import { Network } from "../network";
import SmallButton from "./smallButton";

interface HeroPanelProps {
  heroData,
}

export default class HeroPanel extends React.Component<HeroPanelProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };

    this.handleAttrsClick = this.handleAttrsClick.bind(this)
    this.handleSkillsClick = this.handleSkillsClick.bind(this)
    this.handleAdvanceClick = this.handleAdvanceClick.bind(this)
  }

  handleAttrsClick() {
    Network.sendInfoAttrs(Global.heroId);
  }

  handleSkillsClick() {
    Network.sendInfoSkills(Global.heroId);
  }

  handleAdvanceClick() {
    Network.sendInfoAdvance(Global.heroId);
  }

  render() {
    let imageName = Global.objectStates[Global.heroId].image.toLowerCase().replace(/\s/g, '');
    let imagePath = '/static/art/' + imageName + '_single.png';

    const heroStyle = {
      transform: 'translate(-185px, 25px)',
      position: 'fixed'
    } as React.CSSProperties

    const tableStyle = {
      transform: 'translate(20px, -250px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px'
    } as React.CSSProperties

    const attrsStyle = {
      top: '50%',
      left: '50%',
      marginTop: '-50px',
      marginLeft: '-68px',
      position: 'fixed',
      zIndex: 7
    } as React.CSSProperties

    const skillsStyle = {
      top: '50%',
      left: '50%',
      marginTop: '0px',
      marginLeft: '-68px',
      position: 'fixed',
      zIndex: 7
    } as React.CSSProperties

    const advanceStyle = {
      top: '50%',
      left: '50%',
      marginTop: '50px',
      marginLeft: '-68px',
      position: 'fixed',
      zIndex: 7
    } as React.CSSProperties

    return (
      <HalfPanel left={true}
        panelType={'hero'}
        hideExitButton={false}>
        <img src={imagePath} style={heroStyle} />
        <table style={tableStyle}>
          <tbody>
            <tr>
              <td>Name: </td>
              <td>{this.props.heroData.name}</td>
            </tr>
            <tr>
              <td>HP: </td>
              <td>{this.props.heroData.hp}</td>
            </tr>
            <tr>
              <td>Stamina: </td>
              <td>{this.props.heroData.stamina}</td>
            </tr>
            <tr>
              <td>State: </td>
              <td>{this.props.heroData.state}</td>
            </tr>
          </tbody>
        </table>

        <SmallButton handler={this.handleAttrsClick}
          imageName="attrsbutton"
          style={attrsStyle} />

        <SmallButton handler={this.handleSkillsClick}
          imageName="skillsbutton"
          style={skillsStyle} />

        <SmallButton handler={this.handleAdvanceClick}
          imageName="upgradebutton"
          style={advanceStyle} />

      </HalfPanel>
    );
  }
}

/*
base_def: 1
base_dmg: 3
base_hp: 100
base_speed: 5
base_stamina: 10000
base_vision: 2
capacity: 300
class: "unit"
dmg_range: 8
effects: []
hp: 100
id: 5
image: "warrior"
items: (8) [{…}, {…}, {…}, {…}, {…}, {…}, {…}, {…}]
name: "Warrior"
packet: "info_unit"
skills: {}
stamina: 10000
state: "none"
subclass: "hero"
template: "Warrior"
total_weight: 2265.25
__proto__: Object
*/