import * as React from "react";
import HalfPanel from "./halfPanel";
import { Global } from "../global";
import leftbutton from "ui_comp/leftbutton.png";
import rightbutton from "ui_comp/rightbutton.png";
import craftbutton from "ui_comp/craftbutton.png";
import { Network } from "../network";
import { GameEvent } from "../gameEvent";
import { WEAPON } from "../config";

interface CraftPanelProps {
  structureId,
  recipesData,
}

export default class CraftPanel extends React.Component<CraftPanelProps, any> {
  constructor(props) {
    super(props);

    this.state = {
      recipe : this.props.recipesData[0],
      index : 0
    };

    this.handleLeftClick = this.handleLeftClick.bind(this);
    this.handleRightClick = this.handleRightClick.bind(this);
    this.handleCraftClick = this.handleCraftClick.bind(this);
  }u

  handleLeftClick(event) {
    if(this.state.index != 0) {
      const newIndex = this.state.index - 1;
      this.setState({recipe: this.props.recipesData[newIndex],
                     index: newIndex})
    } 
  }

  handleRightClick(event) {
    if(this.state.index != (this.props.recipesData.length - 1)) {
      const newIndex = this.state.index + 1;
      this.setState({recipe: this.props.recipesData[newIndex],
                     index: newIndex})
    } 
  }

  handleCraftClick() {
    Network.sendOrderCraft(this.props.structureId, this.state.recipe.name);
  }

  render() {
    var imageName = this.state.recipe.item.toLowerCase().replace(/\s/g, '') + '.png';

    const reqs = [];

    for(var i = 0; i < this.state.recipe.req.length; i++) {
      var req = this.state.recipe.req[i];

      reqs.push(
        <tr key={i}>
          <td>{req.type} </td>
          <td>x {req.quantity}</td>
        </tr>)
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
      transform: 'translate(-80px, 10px)',
      position: 'fixed',
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
  
    const craftStyle = {
      transform: 'translate(-187px, 295px)',
      position: 'fixed'
    } as React.CSSProperties

    return (
      <HalfPanel left={false} 
                 panelType={'craft'} 
                 hideExitButton={false}>
        <img src={'/static/art/' + imageName} style={imageStyle} />
        <span style={spanNameStyle}>
          {this.state.recipe.name}
        </span>
        <table style={tableStyle}>
          <tbody>
            <tr>
              <td>Class:</td>
              <td>{this.state.recipe.class}</td>
            </tr>
            <tr>
              <td>Subclass:</td>
              <td>{this.state.recipe.subclass}</td>
            </tr>
            <tr>
              <td>Slot: </td>
              <td>{this.state.recipe.slot}</td>
            </tr>

            {this.state.recipe.hasOwnProperty('damage') && 
              <tr>
                <td>Damage:</td>
                <td>{this.state.recipe.damage}</td>
              </tr> 
            }

            {this.state.recipe.hasOwnProperty('speed') && 
              <tr>
                <td>Speed:</td>
                <td>{this.state.recipe.speed}</td>
              </tr> 
            }

            {this.state.recipe.hasOwnProperty('skill_req') && 
              <tr>
                <td>Skill Req:</td>
                <td>{this.state.recipe.skill_req}</td>
              </tr> 
            }

            {this.state.recipe.hasOwnProperty('stamina_req') && 
              <tr>
                <td>Stamina Req:</td>
                <td>{this.state.recipe.stamina_req}</td>
              </tr> 
            }
     
            {this.state.recipe.hasOwnProperty('armor') && 
              <tr>
                <td>Armor:</td>
                <td>{this.state.recipe.armor}</td>
              </tr> 
            }
             
            <tr>
              <td>Requirements:</td>
              <td>
                <table style={tableStyle2}>
                  <tbody>
                    {reqs}
                  </tbody>
                </table>
              </td>
            </tr>
          </tbody>
        </table>
        <img src={leftbutton} style={leftStyle} onClick={this.handleLeftClick} />
        <img src={rightbutton} style={rightStyle} onClick={this.handleRightClick} />
        <img src={craftbutton} style={craftStyle} onClick={this.handleCraftClick} />
      </HalfPanel>
    );
  }
}



