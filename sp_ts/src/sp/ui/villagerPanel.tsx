import * as React from "react";
import HalfPanel from "./halfPanel";
import attrsbutton from "ui_comp/attrsbutton.png";
import skillsbutton from "ui_comp/skillsbutton.png";
import { GameEvent } from "../gameEvent";
import { Network } from "../network";

interface VillagerPanelProps {
  villagerData,
}

export default class VillagerPanel extends React.Component<VillagerPanelProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };
   
    this.handleAttrsClick = this.handleAttrsClick.bind(this)
    this.handleSkillsClick = this.handleSkillsClick.bind(this)
  }

  handleAttrsClick() {
    Network.sendInfoAttrs(this.props.villagerData.id);
  }

  handleSkillsClick() {
    Network.sendInfoSkills(this.props.villagerData.id);
  }

  render() {
    var imageName = this.props.villagerData.image;
    imageName = imageName.replace(/ /g, '') + '_single.png';

    var effects = this.props.villagerData.effects.join();

    /*for(var i = 0; i < this.props.villagerData.effects.length; i++) {
      effects = effects + ', ' + this.props.villagerData.effects[i];
    }*/

    const heroStyle = {
      transform: 'translate(-195px, 25px)',
      position: 'fixed'
    } as React.CSSProperties

    const spanNameStyle = {
      transform: 'translate(-323px, 90px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '323px'
    } as React.CSSProperties

    const tableStyle = {
      transform: 'translate(20px, -250px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px'
    } as React.CSSProperties

    const effectsStyle = {
      transform: 'translate(20px, -50px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '300px'
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

    return (
      <HalfPanel left={true} 
                 panelType={'villager'} 
                 hideExitButton={false}>
        <img src={'/static/art/' + imageName} style={heroStyle} />
        <span style={spanNameStyle}>{this.props.villagerData.name} (Villager)</span>
        <table style={tableStyle}>
          <tbody>
          <tr>
            <td>Hp: </td>
            <td>{this.props.villagerData.hp} /  
                {this.props.villagerData.base_hp}</td>
          </tr>
          <tr>
            <td>Stamina: </td>
            <td>{this.props.villagerData.stamina} /  
                {this.props.villagerData.base_stamina}</td>
          </tr>
          <tr>
            <td>Speed: </td>
            <td>{this.props.villagerData.base_speed}</td>
          </tr>
          <tr>
            <td>Order: </td>
            <td>{this.props.villagerData.order}</td>
          </tr>
          <tr>
            <td>Action: </td>
            <td>{this.props.villagerData.action}</td>
          </tr>
          <tr>
            <td>State: </td>
            <td>{this.props.villagerData.state}</td>
          </tr>
          <tr>
            <td>Shelter: </td>
            <td>{this.props.villagerData.shelter}</td>
          </tr>
          <tr>
            <td>Structure: </td>
            <td>{this.props.villagerData.structure}</td>
          </tr>
          <tr>
            <td>Effects: </td>
            <td>{effects}</td>
          </tr>
         
          </tbody>
        </table>
        <img src={attrsbutton} style={attrsStyle} onClick={this.handleAttrsClick} />
        <img src={skillsbutton} style={skillsStyle} onClick={this.handleSkillsClick} /> 
      </HalfPanel>
    );
  }
}

